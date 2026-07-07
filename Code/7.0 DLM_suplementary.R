# Code 7.0: DLNM suplementario (cohort / cross-basis) ----
# Basado en 6.1 y demo Gasparrini DLNM cohort (Ref/Time_event_exposure)
# Cuatro especificaciones de cross-basis; sin PCT (semanas gestacionales 0-39)
# crosspred: PM2.5 bruto (/10 µg/m³) cen=mediana, at=mediana+1 (+10 µg/m³); Levo/K IQR cen=0, at=1
# Lag con knots en semanas 13 y 27 (límites trimestrales) salvo el modelo lineal simple
# Salidas en Output/SUP/

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

if (!requireNamespace("dlnm", quietly = TRUE)) {
  install.packages("dlnm", repos = "https://cloud.r-project.org")
}
library(dlnm)

## Datos ----
data_wide <- rio::import("Output/Data_malf_exposure_wide.RData")

dependent_vars <- c("malf", "malf_card_bin")
contaminantes <- c("PM25", "Levo", "K")
tipos <- c("cs", "sp")
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")
gest_weeks <- 0:39
out_weeks <- 2:39

out_dir <- "Output/SUP"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

lag_knots_trim <- list(fun = "ns", knots = c(13, 27))

## Especificaciones cross-basis (4 alternativas) ----
crossbasis_specs <- list(
  lin_simple = list(
    label = "Linear exposure + NS lag (df=4)",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = 4)
  ),
  lin_knots1327 = list(
    label = "Linear exposure + NS lag (knots 13, 27)",
    argvar = list(fun = "lin"),
    arglag = lag_knots_trim
  ),
  bs2_knots1327 = list(
    label = "Quadratic B-spline exposure (df=3) + NS lag (knots 13, 27)",
    argvar = list("bs", degree = 2, df = 3),
    arglag = lag_knots_trim
  ),
  bs3_knots1327 = list(
    label = "Cubic B-spline exposure (df=4) + NS lag (knots 13, 27)",
    argvar = list("bs", degree = 3, df = 4),
    arglag = lag_knots_trim
  )
)

## Matriz de historiales de exposición (filas = sujetos, columnas = semana 0-39) ----
build_exposure_matrix <- function(data, contaminante, tipo) {
  if (contaminante == "PM25") {
    cols <- paste0("w", gest_weeks, "_PM25_", tipo, "_10")
  } else {
    cols <- paste0("iqr_w", gest_weeks, "_", contaminante, "_", tipo)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas de exposición: ", missing_cols[1])
  }
  Q <- as.matrix(data[, cols, drop = FALSE])
  Q[is.na(Q)] <- 0
  Q
}

## Preparar datos de análisis ----
prepare_analysis_data <- function(data, dep_var) {
  df <- data |>
    dplyr::filter(!is.na(.data[[dep_var]])) |>
    dplyr::filter(!is.na(edad_madre), sexo_rn != "Indefinido")

  if (dep_var == "malf_card_bin") {
    df <- df |>
      dplyr::filter(!is.na(malf_card_bin), !is.na(malf)) |>
      dplyr::filter(!(malf == 1 & malf_card_bin == 0))
  }

  df
}

## Contraste crosspred por contaminante (alineado con 6.1) ----
crosspred_contrast <- function(contaminante, Q) {
  if (contaminante == "PM25") {
    cen <- stats::median(Q, na.rm = TRUE)
    at <- cen + 1
  } else {
    cen <- 0
    at <- 1
  }
  list(cen = cen, at = at)
}

## Estimar un modelo DLNM ----
fit_dlnm_model <- function(data, dep_var, contaminante, tipo, spec_id, spec) {
  df <- prepare_analysis_data(data, dep_var)
  if (nrow(df) == 0) return(NULL)

  Q <- build_exposure_matrix(df, contaminante, tipo)
  lag <- c(min(gest_weeks), max(gest_weeks))

  cb <- crossbasis(
    Q,
    lag = lag,
    argvar = spec$argvar,
    arglag = spec$arglag
  )

  control_str <- paste(control_vars, collapse = " + ")
  fml <- stats::as.formula(
    paste(dep_var, "~ cb +", control_str)
  )

  mod <- stats::glm(fml, data = df, family = stats::binomial(link = "logit"))

  contrast <- crosspred_contrast(contaminante, Q)
  pred <- crosspred(cb, mod, cen = contrast$cen, at = contrast$at)

  lag_cols <- colnames(pred$matRRfit)
  lag_vals <- as.integer(sub("^lag", "", lag_cols))
  rr <- as.numeric(pred$matRRfit[1, ])
  rr_l <- as.numeric(pred$matRRlow[1, ])
  rr_h <- as.numeric(pred$matRRhigh[1, ])

  res <- data.frame(
    Week = lag_vals,
    beta = log(rr),
    se = (log(rr_h) - log(rr_l)) / (2 * stats::qnorm(0.975)),
    Lower = log(rr_l),
    Upper = log(rr_h),
    beta_exp = rr,
    Lower_exp = rr_l,
    Upper_exp = rr_h,
    stringsAsFactors = FALSE
  )
  res <- res[is.finite(res$beta_exp) & res$Week %in% out_weeks, , drop = FALSE]
  res$`No Obs` <- stats::nobs(mod)
  res$AIC <- stats::AIC(mod)
  res$BIC <- stats::BIC(mod)
  res$spec_id <- spec_id
  res$spec_label <- spec$label
  res$dependent <- dep_var
  res$contaminante <- contaminante
  res$tipo <- tipo
  res$cen <- contrast$cen
  res$at <- contrast$at

  list(
    results = res,
    model = mod,
    crossbasis = cb,
    pred = pred,
    fit = data.frame(
      spec_id = spec_id,
      spec_label = spec$label,
      dependent = dep_var,
      contaminante = contaminante,
      tipo = tipo,
      n = stats::nobs(mod),
      AIC = stats::AIC(mod),
      BIC = stats::BIC(mod),
      cen = contrast$cen,
      at = contrast$at,
      stringsAsFactors = FALSE
    )
  )
}

## Estética figuras (igual que 6.1) ----
trimester_bands <- tibble::tribble(
  ~xmin, ~xmax, ~fill_col,
  -Inf, 13, "gray70",
  13, 27, "white",
  27, Inf, "gray70"
)

trimester_labels <- tibble::tribble(
  ~x, ~label,
  6.5, "T1",
  18.5, "T2",
  31, "T3"
)

add_hr_color <- function(d) {
  d |>
    dplyr::mutate(
      hr_color = dplyr::case_when(
        .data$Lower_exp > 1 ~ "Increased risk",
        .data$Upper_exp < 1 ~ "Protective",
        TRUE ~ "Null"
      )
    )
}

plot_dlm_single <- function(data, panel_title) {
  data <- dplyr::filter(data, !is.na(.data$Week), .data$Week <= 37)
  if (nrow(data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = panel_title, subtitle = "Sin puntos con Week <= 37")
    )
  }
  y_delta <- max(
    abs(data$Lower_exp - 1), abs(data$Upper_exp - 1),
    na.rm = TRUE
  )
  if (!is.finite(y_delta) || y_delta < 1e-6) y_delta <- 0.1
  y_step <- y_delta / 2
  y_breaks <- 1 + (-3:3) * y_step
  y_limits <- range(y_breaks)

  ggplot2::ggplot(data, ggplot2::aes(x = .data$Week, y = .data$beta_exp)) +
    ggplot2::geom_rect(
      data = trimester_bands,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_col),
      inherit.aes = FALSE,
      alpha = 0.15
    ) +
    ggplot2::geom_text(
      data = trimester_labels,
      ggplot2::aes(x = x, y = Inf, label = label),
      inherit.aes = FALSE,
      vjust = 1.2,
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Lower_exp, ymax = .data$Upper_exp, color = .data$hr_color),
      width = 0.3
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$hr_color), size = 2) +
    ggplot2::scale_color_manual(
      values = c(
        "Increased risk" = "#E41A1C",
        "Protective" = "#377EB8",
        "Null" = "black"
      ),
      breaks = c("Increased risk", "Protective", "Null"),
      name = NULL
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = scales::label_number(accuracy = 0.01, decimal.mark = ".")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 37, by = 3), limits = c(1, 37)) +
    ggplot2::labs(title = panel_title, y = "RR (95% CI)", x = "Gestational week") +
    ggplot2::theme_light(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, hjust = 0, face = "bold"),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(4, 4, 4, 4, "pt")
    )
}

contam_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

build_outcome_panel <- function(all_results, dep_var, spec_id) {
  plots_list <- list()
  for (tipo_val in tipos) {
    for (j in seq_along(contaminantes)) {
      contam <- contaminantes[j]
      model_key <- paste(spec_id, dep_var, contam, tipo_val, sep = "|")
      res <- all_results[[model_key]]
      if (is.null(res)) {
        plots_list[[length(plots_list) + 1L]] <-
          ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::labs(title = paste("Sin datos:", contam, tipo_val))
      } else {
        d <- add_hr_color(as.data.frame(res))
        letter_col <- LETTERS[j]
        tit <- paste0(
          letter_col, ". ", unname(contam_labels[contam]), " (", tipo_val, ")"
        )
        plots_list[[length(plots_list) + 1L]] <- plot_dlm_single(d, tit)
      }
    }
  }
  ggpubr::ggarrange(plotlist = plots_list, ncol = 3, nrow = 2, align = "hv")
}

## Tabla resumen (formato 6.3) ----
build_summary_table <- function(all_results, spec_id, dep, tipo) {
  to_cell <- function(d) {
    dplyr::transmute(
      d,
      Week = as.integer(.data$Week),
      cell = sprintf(
        "%.2f (%.2f-%.2f)",
        .data$beta_exp,
        .data$Lower_exp,
        .data$Upper_exp
      )
    )
  }

  pm_key <- paste(spec_id, dep, "PM25", tipo, sep = "|")
  le_key <- paste(spec_id, dep, "Levo", tipo, sep = "|")
  k_key <- paste(spec_id, dep, "K", tipo, sep = "|")

  pmf <- if (!is.null(all_results[[pm_key]])) dplyr::rename(to_cell(all_results[[pm_key]]), PM25 = cell) else NULL
  lef <- if (!is.null(all_results[[le_key]])) dplyr::rename(to_cell(all_results[[le_key]]), Levo = cell) else NULL
  kkf <- if (!is.null(all_results[[k_key]])) dplyr::rename(to_cell(all_results[[k_key]]), K = cell) else NULL

  out <- tibble::tibble(Week = out_weeks)
  if (!is.null(pmf)) out <- dplyr::left_join(out, pmf, by = "Week")
  if (!is.null(lef)) out <- dplyr::left_join(out, lef, by = "Week")
  if (!is.null(kkf)) out <- dplyr::left_join(out, kkf, by = "Week")
  out
}

safe_sheet_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  substr(x, 1, 31)
}

## AIC/BIC del DLM semanal (Code 6.1) ----
dlm_6_1_xlsx <- "Output/DLM/Malf_DLM_results_all_models_IQR.xlsx"

extract_dlm_6_1_fit <- function(xlsx_path = dlm_6_1_xlsx) {
  if (!file.exists(xlsx_path)) {
    warning("No se encontró ", xlsx_path, "; omitiendo filas DLM 6.1 en tabla de ajuste.")
    return(tibble::tibble())
  }

  sheets <- readxl::excel_sheets(xlsx_path)
  rows <- list()

  for (dep_var in dependent_vars) {
    for (contam in contaminantes) {
      for (tipo_val in tipos) {
        sheet <- if (contam == "PM25") {
          paste(dep_var, "PM25_10", tipo_val, sep = "_")
        } else {
          paste(dep_var, contam, tipo_val, sep = "_")
        }
        if (!sheet %in% sheets) next

        d <- readxl::read_excel(xlsx_path, sheet = sheet)
        if (!all(c("AIC", "BIC") %in% names(d))) next

        rows[[length(rows) + 1L]] <- tibble::tibble(
          spec_id = "dlm_6.1",
          spec_label = "DLM weekly (6.1): current week + inverse-distance lag",
          dependent = dep_var,
          contaminante = contam,
          tipo = tipo_val,
          n = if ("No.Obs" %in% names(d)) d$`No.Obs`[1] else NA_real_,
          AIC = mean(d$AIC, na.rm = TRUE),
          BIC = mean(d$BIC, na.rm = TRUE),
          n_weekly_models = sum(is.finite(d$AIC)),
          cen = NA_real_,
          at = NA_real_
        )
      }
    }
  }

  dplyr::bind_rows(rows)
}

## Estimación ----
all_results <- list()
fit_table <- list()

for (spec_id in names(crossbasis_specs)) {
  spec <- crossbasis_specs[[spec_id]]
  for (dep_var in dependent_vars) {
    for (contam in contaminantes) {
      for (tipo_val in tipos) {
        model_key <- paste(spec_id, dep_var, contam, tipo_val, sep = "|")
        fit <- tryCatch(
          fit_dlnm_model(
            data = data_wide,
            dep_var = dep_var,
            contaminante = contam,
            tipo = tipo_val,
            spec_id = spec_id,
            spec = spec
          ),
          error = function(e) {
            warning("Fallo ", model_key, ": ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(fit)) {
          all_results[[model_key]] <- fit$results
          fit_table[[length(fit_table) + 1L]] <- fit$fit
        }
      }
    }
  }
}

fit_df <- dplyr::bind_rows(fit_table)
fit_dlm_6_1 <- extract_dlm_6_1_fit()
fit_df <- dplyr::bind_rows(fit_df, fit_dlm_6_1)

## Excel: resultados por especificación ----
for (spec_id in names(crossbasis_specs)) {
  spec_results <- all_results[grepl(paste0("^", spec_id, "\\|"), names(all_results))]
  if (length(spec_results) == 0) next

  results_xlsx <- stats::setNames(
    spec_results,
    vapply(names(spec_results), function(s) {
      safe_sheet_name(sub(paste0("^", spec_id, "\\|"), "", s))
    }, character(1))
  )

  writexl::write_xlsx(
    results_xlsx,
    path = file.path(out_dir, paste0("Malf_DLNM_SUP_", spec_id, "_results.xlsx"))
  )
}

writexl::write_xlsx(
  list(Model_fit = fit_df),
  path = file.path(out_dir, "Malf_DLNM_SUP_model_fit_AIC_BIC.xlsx")
)

## Tabla de ajuste para el paper ----
build_paper_fit_table <- function(fit_df) {
  outcome_labels <- c(
    malf = "Any congenital malformation",
    malf_card_bin = "Congenital heart disease"
  )
  pollutant_labels <- c(
    PM25 = "PM2.5",
    Levo = "Levoglucosan",
    K = "Potassium"
  )
  exposure_type_labels <- c(
    cs = "Fixed site (cs)",
    sp = "Land-use regression (sp)"
  )
  model_family_labels <- c(
    lin_simple = "DLNM (cohort cross-basis)",
    lin_knots1327 = "DLNM (cohort cross-basis)",
    bs2_knots1327 = "DLNM (cohort cross-basis)",
    bs3_knots1327 = "DLNM (cohort cross-basis)",
    dlm_6.1 = "DLM (weekly, primary analysis)"
  )
  spec_order <- c(
    "lin_simple", "lin_knots1327", "bs2_knots1327", "bs3_knots1327", "dlm_6.1"
  )

  fit_df |>
    dplyr::mutate(
      Outcome = unname(outcome_labels[.data$dependent]),
      Model = unname(model_family_labels[.data$spec_id]),
      `Model specifications` = dplyr::if_else(
        .data$spec_id == "dlm_6.1",
        paste0(
          .data$spec_label,
          " (AIC and BIC averaged over ",
          .data$n_weekly_models,
          " gestational-week models)."
        ),
        .data$spec_label
      ),
      Pollutant = unname(pollutant_labels[.data$contaminante]),
      `Exposure model` = unname(exposure_type_labels[.data$tipo]),
      AIC = sprintf("%.3f", .data$AIC),
      BIC = sprintf("%.3f", .data$BIC),
      spec_ord = match(.data$spec_id, spec_order),
      dep_ord = match(.data$dependent, names(outcome_labels)),
      poll_ord = match(.data$contaminante, names(pollutant_labels)),
      tipo_ord = match(.data$tipo, names(exposure_type_labels))
    ) |>
    dplyr::arrange(.data$dep_ord, .data$tipo_ord, .data$poll_ord, .data$spec_ord) |>
    dplyr::select(
      Outcome,
      Model,
      `Model specifications`,
      Pollutant,
      `Exposure model`,
      AIC,
      BIC
    )
}

paper_fit_table <- build_paper_fit_table(fit_df)
writexl::write_xlsx(
  list(Model_fit_paper = paper_fit_table),
  path = file.path(out_dir, "Malf_DLNM_SUP_model_fit_AIC_BIC_paper.xlsx")
)

## Excel: tablas resumen por especificación ----
for (spec_id in names(crossbasis_specs)) {
  summary_list <- list(
    malf_cs_gest = build_summary_table(all_results, spec_id, "malf", "cs"),
    malf_sp_gest = build_summary_table(all_results, spec_id, "malf", "sp"),
    malf_card_cs_gest = build_summary_table(all_results, spec_id, "malf_card_bin", "cs"),
    malf_card_sp_gest = build_summary_table(all_results, spec_id, "malf_card_bin", "sp")
  )
  writexl::write_xlsx(
    summary_list,
    path = file.path(out_dir, paste0("Malf_DLNM_SUP_", spec_id, "_summary.xlsx"))
  )
}

## Figuras por outcome y especificación ----
for (spec_id in names(crossbasis_specs)) {
  for (dv in dependent_vars) {
    p_out <- build_outcome_panel(all_results, dv, spec_id)
    fn <- file.path(
      out_dir,
      paste0("Malf_DLNM_SUP_", spec_id, "_trimester_panel_", dv, ".png")
    )
    ggplot2::ggsave(
      fn,
      plot = p_out,
      res = 300,
      width = 28,
      height = 16,
      scale = 1,
      units = "cm",
      device = ragg::agg_png
    )
  }
}

cat("DLNM supplementary analysis completed. Outputs in", out_dir, "\n")
