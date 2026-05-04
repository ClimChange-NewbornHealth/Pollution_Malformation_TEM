# 5.6 Exposure models /10 estratificados por sexo (misma especificación que Code/5.4) -----
# PM2.5: columnas *_PM25_*_10; Levo/K: nivel bruto. Sin sexo_rn en el ajuste dentro del estrato.

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos ----

data_full_wide <- rio::import("Output/Data_malf_exposure_wide.RData")

## 2. Preparamos los datos ----

data_model <- data_full_wide |>
  dplyr::filter(!is.na(malf))

data_model_card <- data_full_wide |>
  dplyr::filter(!is.na(malf_card_bin)) |>
  dplyr::filter(!is.na(malf)) |>
  dplyr::filter(!(malf == 1 & malf_card_bin == 0))

## 3. Grillas iguales a 5.4 (solo malf y malf_card_bin como outcomes) ----

control_vars_stratum <- c("edad_madre", "a_nac", "estacion")

time_periods <- c("pct1", "t1", "t2", "t3", "w20", "tot")
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")

pred_single_div10 <- function(tiempo_lab, contaminante, tipo) {
  if (contaminante == "PM25") {
    paste0(tiempo_lab, "_PM25_", tipo, "_10")
  } else {
    paste0(tiempo_lab, "_", contaminante, "_", tipo)
  }
}

pred_joint_div10_pct <- function(contaminante, tipo) {
  pr <- c("pct1", "t1", "t2", "t3")
  paste(
    vapply(pr, pred_single_div10, character(1),
           contaminante = contaminante, tipo = tipo),
    collapse = " + "
  )
}

pred_joint_div10_t123 <- function(contaminante, tipo) {
  pr <- c("t1", "t2", "t3")
  paste(
    vapply(pr, pred_single_div10, character(1),
           contaminante = contaminante, tipo = tipo),
    collapse = " + "
  )
}

make_combinations <- function(dependent_vec) {
  combinations_single <- expand.grid(
    dependent = dependent_vec,
    tiempo = time_periods,
    contaminante = contaminants,
    tipo = types,
    model_type = "single",
    adjustment = c("Unadjusted", "Adjusted"),
    stringsAsFactors = FALSE
  )
  combinations_single <- combinations_single |>
    mutate(predictor = purrr::pmap_chr(list(tiempo, contaminante, tipo), pred_single_div10))

  combinations_pct1_t1_t2_t3 <- expand.grid(
    dependent = dependent_vec,
    contaminante = contaminants,
    tipo = types,
    model_type = "pct1_t1_t2_t3",
    adjustment = c("Unadjusted", "Adjusted"),
    stringsAsFactors = FALSE
  )
  combinations_pct1_t1_t2_t3 <- combinations_pct1_t1_t2_t3 |>
    mutate(
      predictor = purrr::pmap_chr(
        list(contaminante, tipo),
        \(co, tp) pred_joint_div10_pct(co, tp)
      ),
      tiempo = "pct1_t1_t2_t3"
    )

  combinations_t1_t2_t3 <- expand.grid(
    dependent = dependent_vec,
    contaminante = contaminants,
    tipo = types,
    model_type = "t1_t2_t3",
    adjustment = c("Unadjusted", "Adjusted"),
    stringsAsFactors = FALSE
  )
  combinations_t1_t2_t3 <- combinations_t1_t2_t3 |>
    mutate(
      predictor = purrr::pmap_chr(
        list(contaminante, tipo),
        \(co, tp) pred_joint_div10_t123(co, tp)
      ),
      tiempo = "t1_t2_t3"
    )

  combinations <- bind_rows(
    combinations_single,
    combinations_pct1_t1_t2_t3,
    combinations_t1_t2_t3
  )

  pred_pm25_div10 <- names(data_model)[
    grepl("^(pct1|t1|t2|t3|w20|tot)_", names(data_model)) &
      grepl("_PM25_", names(data_model)) &
      grepl("_10$", names(data_model))
  ]
  raw_levo_k_preds <- names(data_model)[
    grepl("^(pct1|t1|t2|t3|w20|tot)_(Levo|K)_", names(data_model)) &
      !grepl("^iqr_", names(data_model)) &
      !grepl("_10$", names(data_model))
  ]
  available_predictors <- union(pred_pm25_div10, raw_levo_k_preds)

  combinations_single_valid <- combinations |>
    filter(model_type == "single") |>
    filter(predictor %in% available_predictors)

  combinations_multi <- combinations |>
    filter(model_type != "single") |>
    rowwise() |>
    mutate(
      predictors_list = list(stringr::str_split(predictor, " \\+ ")[[1]]),
      all_exist = all(trimws(predictors_list) %in% available_predictors)
    ) |>
    ungroup() |>
    filter(all_exist) |>
    select(-predictors_list, -all_exist)

  bind_rows(combinations_single_valid, combinations_multi)
}

combinations_malf <- make_combinations("malf")
combinations_card <- make_combinations("malf_card_bin")

writexl::write_xlsx(
  list(malf = combinations_malf, malf_card_bin = combinations_card),
  path = "Output/Models/List_models_exposure_malf_div10_by_sex.xlsx"
)

## 4. Estimación logística por estrato ----

fit_logit_model <- function(dependent, predictor, tiempo, contaminante, tipo,
                            model_type, data, control_vars,
                            conf.level = 0.95, adjustment = "Adjusted") {
  if (model_type == "single") {
    predictors_list <- predictor
  } else {
    predictors_list <- trimws(stringr::str_split(predictor, " \\+ ")[[1]])
  }

  missing_predictors <- predictors_list[!predictors_list %in% names(data)]
  if (length(missing_predictors) > 0) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = 0
    ))
  }

  data_subset <- data |>
    dplyr::filter(!is.na(.data[[dependent]]))

  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }

  if (nrow(data_subset) < 10) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    ))
  }

  if (identical(adjustment, "Adjusted")) {
    available_controls <- control_vars[control_vars %in% names(data_subset)]
    rhs <- if (length(available_controls) > 0) {
      paste(
        paste(predictors_list, collapse = " + "),
        paste("+", paste(available_controls, collapse = " + "))
      )
    } else {
      paste(predictors_list, collapse = " + ")
    }
  } else {
    rhs <- paste(predictors_list, collapse = " + ")
  }

  fml <- as.formula(paste0(dependent, " ~ ", rhs))

  model_fit <- tryCatch({
    glm(fml, data = data_subset, family = binomial(link = "logit"))
  }, error = function(e) {
    NULL
  })

  if (is.null(model_fit)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    ))
  }

  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  z <- qnorm(1 - (1 - conf.level) / 2)
  tbl_exposure <- tbl[tbl$term %in% predictors_list, ]

  if (nrow(tbl_exposure) > 0) {
    tbl_exposure <- tbl_exposure |>
      dplyr::mutate(
        or = exp(estimate),
        conf.low = exp(estimate - z * std.error),
        conf.high = exp(estimate + z * std.error),
        estimate = or,
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset)
      ) |>
      dplyr::select(term, estimate, std.error, statistic, p.value,
                    conf.low, conf.high, dependent_var, predictor,
                    tiempo, contaminante, tipo, model_type, adjustment, n)
  } else {
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    )
  }

  rm(model_fit)
  gc()

  tbl_exposure
}

run_models_parallel <- function(combinations_grid, data_stratum, label_msg) {
  plan(multisession, workers = parallel::detectCores() - 4)
  options(future.globals.maxSize = 1.5 * 1024^3)
  tic()
  out <- future_lapply(seq_len(nrow(combinations_grid)), function(i) {
    message(label_msg, " ", i, "/", nrow(combinations_grid), " PID ", Sys.getpid())
    fit_logit_model(
      dependent = combinations_grid$dependent[i],
      predictor = combinations_grid$predictor[i],
      tiempo = combinations_grid$tiempo[i],
      contaminante = combinations_grid$contaminante[i],
      tipo = combinations_grid$tipo[i],
      model_type = combinations_grid$model_type[i],
      data = data_stratum,
      control_vars = control_vars_stratum,
      adjustment = combinations_grid$adjustment[i]
    )
  }, future.seed = TRUE)
  toc()
  plan(sequential)
  out
}

sex_levels <- c("Femenino", "Masculino")

out_dir_tables <- "Output/Tables"
out_dir_models <- "Output/Models"
if (!dir.exists(out_dir_tables)) dir.create(out_dir_tables, recursive = TRUE)

## 5–6. Tablas mismo formato que 5.5; coeficientes /10 + brutos -----

period_order <- c("pct1", "t1", "t2", "t3")
period_order_joint_t123 <- c("t1", "t2", "t3")
period_order_joint_pct <- c("pct1", "t1", "t2", "t3")
period_labels <- c(
  pct1 = "Preconception (pct1)",
  t1   = "Trimester 1",
  t2   = "Trimester 2",
  t3   = "Trimester 3"
)
tipo_order <- c("cs", "sp")
tipo_labels <- c(
  cs = "Fixed site (cs)",
  sp = "Land-use regression (sp)"
)
poll_order <- c("PM25", "Levo", "K")
poll_headers <- c(PM25 = "PM2.5", Levo = "Levo", K = "K")

fmt2 <- function(x) {
  if (length(x) != 1L) return("")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("")
  sprintf("%.2f", round(x, 2))
}

fmt_ci <- function(lo, hi) {
  lo <- suppressWarnings(as.numeric(lo))
  hi <- suppressWarnings(as.numeric(hi))
  if (length(lo) != 1L || length(hi) != 1L || is.na(lo) || is.na(hi)) return("")
  sprintf("(%s, %s)", fmt2(lo), fmt2(hi))
}

fmt_rr_ci <- function(estimate, lo, hi) {
  rr <- fmt2(estimate)
  ci <- fmt_ci(lo, hi)
  if (rr == "" && ci == "") return("")
  if (ci == "") return(rr)
  if (rr == "") return(ci)
  paste(rr, ci)
}

period_from_div10_term <- function(term) {
  m <- stringr::str_match(term, "^(pct1|t1|t2|t3)_(PM25|Levo|K)_")
  ifelse(is.na(m[, 1L]), NA_character_, m[, 2L])
}

build_template_table <- function(results_df, dependent_target) {
  d <- results_df |>
    dplyr::filter(
      .data$dependent_var == dependent_target,
      .data$adjustment == "Adjusted",
      .data$model_type == "single",
      .data$tiempo %in% period_order,
      .data$contaminante %in% poll_order,
      .data$tipo %in% tipo_order
    )

  rows <- list()
  for (tp in tipo_order) {
    for (ti in period_order) {
      row <- tibble::tibble(
        Section = tipo_labels[[tp]],
        `Exposure period` = unname(period_labels[[ti]])
      )
      for (co in poll_order) {
        r <- d |>
          dplyr::filter(.data$tipo == tp, .data$tiempo == ti, .data$contaminante == co) |>
          dplyr::slice(1)
        row[[poll_headers[[co]]]] <- if (nrow(r) == 1L) {
          fmt_rr_ci(r$estimate, r$conf.low, r$conf.high)
        } else {
          ""
        }
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  dplyr::bind_rows(rows)
}

build_joint_template_table <- function(results_df, dependent_target, model_type_val, periods_vec) {
  d <- results_df |>
    dplyr::filter(
      .data$dependent_var == dependent_target,
      .data$adjustment == "Adjusted",
      .data$model_type == model_type_val,
      .data$contaminante %in% poll_order,
      .data$tipo %in% tipo_order
    ) |>
    dplyr::mutate(period_key = period_from_div10_term(.data$term)) |>
    dplyr::filter(.data$period_key %in% periods_vec)

  rows <- list()
  for (tp in tipo_order) {
    for (ti in periods_vec) {
      row <- tibble::tibble(
        Section = tipo_labels[[tp]],
        `Exposure period` = unname(period_labels[[ti]])
      )
      for (co in poll_order) {
        r <- d |>
          dplyr::filter(
            .data$tipo == tp,
            .data$period_key == ti,
            .data$contaminante == co
          ) |>
          dplyr::slice(1)
        row[[poll_headers[[co]]]] <- if (nrow(r) == 1L) {
          fmt_rr_ci(r$estimate, r$conf.low, r$conf.high)
        } else {
          ""
        }
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  dplyr::bind_rows(rows)
}

footnote_single <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 /10 (_10 suffix); Levo/K raw concentrations.",
  "Stratified by infant sex (female/male); sex excluded from adjustment within stratum.",
  "Adjustment: maternal age, year of birth, season of conception.",
  "Each period (pct1, t1, t2, t3) estimated in a separate model (single-period predictors only)."
)

footnote_joint_t123 <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 /10 predictors (_10); Levo/K raw.",
  "Stratified by infant sex (female/male); sex excluded from adjustment within stratum.",
  "Adjustment: maternal age, year of birth, season of conception.",
  "One mutually adjusted model per pollutant and exposure type (mixed scaling)."
)

footnote_joint_pct <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 /10 predictors (_10); Levo/K raw.",
  "Stratified by infant sex (female/male); sex excluded from adjustment within stratum.",
  "Adjustment: maternal age, year of birth, season of conception.",
  "One mutually adjusted model per pollutant and exposure type (mixed scaling)."
)

footnote_tbl <- tibble::tibble(
  Sheet = c(
    "Table 2",
    "Mutual t1+t2+t3",
    "Mutual pct1+t1+t2+t3"
  ),
  Note = c(footnote_single, footnote_joint_t123, footnote_joint_pct)
)

for (sx in sex_levels) {
  data_ms <- data_model |> dplyr::filter(as.character(sexo_rn) == sx)
  data_cs <- data_model_card |> dplyr::filter(as.character(sexo_rn) == sx)

  sfx <- sx

  results_list <- run_models_parallel(
    combinations_malf,
    data_ms,
    paste0("Malf div10 ", sfx)
  )
  results_malf_df <- dplyr::bind_rows(results_list)

  save(results_list, file = file.path(out_dir_models, paste0("Exposure_models_malf_div10_", sfx, ".RData")))
  writexl::write_xlsx(
    results_malf_df,
    path = file.path(out_dir_models, paste0("Exposure_models_malf_div10_", sfx, ".xlsx"))
  )

  tab_malf_single <- build_template_table(results_malf_df, "malf")
  tab_malf_joint_t123 <- build_joint_template_table(
    results_malf_df, "malf", "t1_t2_t3", period_order_joint_t123
  )
  tab_malf_joint_pct <- build_joint_template_table(
    results_malf_df, "malf", "pct1_t1_t2_t3", period_order_joint_pct
  )
  writexl::write_xlsx(
    list(
      `Table 2` = tab_malf_single,
      `Mutual t1+t2+t3` = tab_malf_joint_t123,
      `Mutual pct1+t1+t2+t3` = tab_malf_joint_pct,
      Footnote = footnote_tbl
    ),
    path = file.path(out_dir_tables, paste0("Table_div10_adjusted_malf_sex_", sfx, ".xlsx"))
  )

  results_list_card <- run_models_parallel(
    combinations_card,
    data_cs,
    paste0("Card div10 ", sfx)
  )
  results_card_df <- dplyr::bind_rows(results_list_card)

  save(results_list_card, file = file.path(out_dir_models, paste0("Exposure_models_malf_card_bin_div10_", sfx, ".RData")))
  writexl::write_xlsx(
    results_card_df,
    path = file.path(out_dir_models, paste0("Exposure_models_malf_card_bin_div10_", sfx, ".xlsx"))
  )

  tab_card_single <- build_template_table(results_card_df, "malf_card_bin")
  tab_card_joint_t123 <- build_joint_template_table(
    results_card_df, "malf_card_bin", "t1_t2_t3", period_order_joint_t123
  )
  tab_card_joint_pct <- build_joint_template_table(
    results_card_df, "malf_card_bin", "pct1_t1_t2_t3", period_order_joint_pct
  )
  writexl::write_xlsx(
    list(
      `Table 2` = tab_card_single,
      `Mutual t1+t2+t3` = tab_card_joint_t123,
      `Mutual pct1+t1+t2+t3` = tab_card_joint_pct,
      Footnote = footnote_tbl
    ),
    path = file.path(out_dir_tables, paste0("Table_div10_adjusted_malf_card_sex_", sfx, ".xlsx"))
  )
}
