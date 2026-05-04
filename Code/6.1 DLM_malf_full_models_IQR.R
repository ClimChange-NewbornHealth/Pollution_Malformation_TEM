# Code 6.1 DLM Malformaciones - Full Models (exposición en IQR, eje semanas gestacionales) ----
# Variables dependientes: malf, malf_card_bin × PM25, Levo, K × cs, sp
# Figuras: 2 paneles (una por outcome), cada uno 2 filas (cs, sp) × 3 columnas (contaminantes)

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## Open Data (wide → long solo columnas iqr_w*) ----
data_wide <- rio::import("Output/Data_malf_exposure_wide.RData")

dependent_vars <- c("malf", "malf_card_bin")
contaminantes <- c("PM25", "Levo", "K")
tipos <- c("cs", "sp")
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")

dep_cov <- c("idbase", dependent_vars, control_vars)
iqr_week_cols <- grep("^iqr_w-?\\d+_(PM25|Levo|K)_(cs|sp)$", names(data_wide), value = TRUE)

data_full_long <- data_wide |>
  dplyr::select(dplyr::any_of(dep_cov), dplyr::all_of(iqr_week_cols)) |>
  tidyr::pivot_longer(
    dplyr::all_of(iqr_week_cols),
    names_to = "metrica",
    values_to = "exposicion"
  ) |>
  tidyr::extract(
    "metrica",
    into = c("tiempo", "contaminante", "tipo"),
    regex = "^iqr_(w-?\\d+)_(PM25|Levo|K)_(cs|sp)$",
    remove = FALSE
  ) |>
  dplyr::mutate(
    week = as.integer(sub("^w", "", tiempo))
  ) |>
  dplyr::arrange(malf, idbase, week)

# PM2.5 en escala /10 (weekly `w*_PM25_*_10`), solo para modelos y figura PM2.5 ----
pm25_10_week_cols <- grep("^w-?\\d+_PM25_(cs|sp)_10$", names(data_wide), value = TRUE)
if (length(pm25_10_week_cols) > 0) {
  data_pm25_10_long <- data_wide |>
    dplyr::select(dplyr::any_of(dep_cov), dplyr::all_of(pm25_10_week_cols)) |>
    tidyr::pivot_longer(
      dplyr::all_of(pm25_10_week_cols),
      names_to = "metrica",
      values_to = "exposicion"
    ) |>
    tidyr::extract(
      "metrica",
      into = c("tiempo", "tipo"),
      regex = "^(w-?\\d+)_PM25_(cs|sp)_10$",
      remove = FALSE
    ) |>
    dplyr::mutate(
      week = as.integer(sub("^w", "", tiempo)),
      contaminante = "PM25"
    ) |>
    dplyr::arrange(malf, idbase, week)
} else {
  data_pm25_10_long <- data_full_long |> dplyr::filter(FALSE)
}

## Función para procesar un modelo ----
process_dlm_model <- function(dep_var, contam, tipo_val, data_long, control_vars) {
  data_long <- data_long |>
    dplyr::filter(!is.na(.data[[dep_var]]))

  if (dep_var == "malf_card_bin") {
    data_long <- data_long |>
      dplyr::filter(!is.na(malf_card_bin), !is.na(malf)) |>
      dplyr::filter(!(malf == 1 & malf_card_bin == 0))
  }

  data_filtered <- data_long |>
    dplyr::filter(tipo == tipo_val, contaminante == contam) |>
    dplyr::filter(week >= 0) |>
    dplyr::select(dplyr::all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion")))

  if (nrow(data_filtered) == 0) {
    return(list(results = NULL, plot = NULL))
  }

  data_wide <- data_filtered |>
    dplyr::select(dplyr::all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |>
    tidyr::pivot_wider(
      names_from = week,
      values_from = "exposicion",
      names_prefix = "exposicion_"
    )

  data_lagged <- data_filtered |>
    dplyr::select(dplyr::all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |>
    dplyr::arrange(idbase, week) |>
    dplyr::group_by(idbase) |>
    dplyr::mutate(exposicion_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
      if (week[i] == 0) return(NA_real_)
      past_rows <- which(week < week[i])
      if (length(past_rows) == 0) return(NA_real_)
      weights <- 1 / (week[i] - week[past_rows])
      exposures <- exposicion[past_rows]
      sum(weights * exposures, na.rm = TRUE)
    }))

  setDT(data_lagged)

  data_lagged_wide <- data_lagged |>
    dplyr::select(idbase, week, exposicion_lagged) |>
    tidyr::pivot_wider(
      names_from = week,
      values_from = exposicion_lagged,
      names_prefix = "exposicion_lagged_"
    ) |>
    dplyr::ungroup()

  data_model <- dplyr::left_join(data_wide, data_lagged_wide, by = "idbase") |>
    dplyr::arrange(idbase) |>
    tidyr::drop_na(edad_madre) |>
    dplyr::filter(sexo_rn != "Indefinido")

  setDT(data_model)

  expo <- grep("^exposicion_\\d+$", names(data_model), value = TRUE)
  expo_lag <- grep("^exposicion_lagged_\\d+$", names(data_model), value = TRUE)

  if (length(expo) == 0 || length(expo_lag) == 0) {
    return(list(results = NULL, plot = NULL))
  }

  expo_weeks <- as.numeric(gsub("exposicion_", "", expo))
  expo_lag_weeks <- as.numeric(gsub("exposicion_lagged_", "", expo_lag))

  common_weeks <- intersect(expo_weeks, expo_lag_weeks)
  common_weeks <- common_weeks[common_weeks >= 2 & common_weeks <= 39]

  if (length(common_weeks) == 0) {
    return(list(results = NULL, plot = NULL))
  }

  expo_vars <- paste0("exposicion_", common_weeks)
  expo_vars_lag <- paste0("exposicion_lagged_", common_weeks)

  res_combo <- data.table()

  for (i in seq_along(expo_vars)) {
    exp <- expo_vars[i]
    lag <- expo_vars_lag[i]
    control_str <- paste(control_vars, collapse = " + ")

    formula_str <- paste0(
      dep_var, " ~ ",
      exp, " + ", lag,
      " + ", control_str
    )

    fml <- as.formula(formula_str)
    mod <- glm(fml, data = data_model, family = binomial(link = "logit"))

    ci_prof <- suppressMessages(confint(mod, parm = exp))

    dat <- data.frame(
      Week = common_weeks[i],
      Exposure = exp,
      Lagged = lag,
      `No Obs` = mod$df.null + 1,
      beta = unname(mod$coefficients[exp]),
      se = summary(mod)$coefficients[exp, "Std. Error"],
      Lower = ci_prof[1],
      Upper = ci_prof[2],
      AIC = AIC(mod),
      BIC = BIC(mod)
    )

    dat$beta_exp <- exp(dat$beta)
    dat$Lower_exp <- exp(dat$Lower)
    dat$Upper_exp <- exp(dat$Upper)

    res_combo <- rbind(res_combo, dat)
  }

  return(list(results = res_combo, plot = NULL))
}

## Estética figuras (T1/T2/T3 sombreadas, HR coloreado) ----
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

plot_dlm_single <- function(data, contam_code, panel_title) {
  data <- dplyr::filter(data, !is.na(.data$Week), .data$Week <= 37)
  if (nrow(data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = panel_title, subtitle = "Sin puntos con Week ≤ 37")
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
    ggplot2::labs(title = panel_title, y = "HR (95% CI)", x = "Gestational week") +
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

build_outcome_panel <- function(all_results, dep_var, contam_labels) {
  plots_list <- list()
  for (tipo_val in tipos) {
    for (j in seq_along(contaminantes)) {
      contam <- contaminantes[j]
      model_key <- if (contam == "PM25") {
        paste(dep_var, "PM25_10", tipo_val, sep = "|")
      } else {
        paste(dep_var, contam, tipo_val, sep = "|")
      }
      res <- all_results[[model_key]]
      if (is.null(res)) {
        plots_list[[length(plots_list) + 1L]] <-
          ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::labs(title = paste("Sin datos:", contam, tipo_val))
      } else {
        d <- add_hr_color(as.data.frame(res))
        letter_col <- LETTERS[j]
        tit <- if (contam == "PM25") {
          paste0(letter_col, ". ", unname(contam_labels[contam]), " (", tipo_val, ")")
        } else {
          paste0(letter_col, ". ", unname(contam_labels[contam]), " (", tipo_val, ")")
        }
        plots_list[[length(plots_list) + 1L]] <- plot_dlm_single(d, contam, tit)
      }
    }
  }
  ggpubr::ggarrange(plotlist = plots_list, ncol = 3, nrow = 2, align = "hv")
}

## Estimamos todos los modelos ----
all_results <- list()

for (dep_var in dependent_vars) {
  for (contam in contaminantes) {
    for (tipo_val in tipos) {
      model_key <- paste(dep_var, contam, tipo_val, sep = "|")
      result <- process_dlm_model(dep_var, contam, tipo_val, data_full_long, control_vars)
      if (!is.null(result$results)) {
        all_results[[model_key]] <- result$results
      }
    }
  }
}

# PM2.5 adicional: misma especificación DLM con predictores /10 (se guarda en Excel; figura PM2.5)
for (dep_var in dependent_vars) {
  for (tipo_val in tipos) {
    model_key <- paste(dep_var, "PM25_10", tipo_val, sep = "|")
    result <- process_dlm_model(dep_var, "PM25", tipo_val, data_pm25_10_long, control_vars)
    if (!is.null(result$results)) {
      all_results[[model_key]] <- result$results
    }
  }
}

contam_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

## Guardamos resultados en Excel ----
out_xlsx <- "Output/DLM/Malf_DLM_results_all_models_IQR.xlsx"
if (length(all_results) > 0) {
  if (!dir.exists("Output/DLM")) dir.create("Output/DLM", recursive = TRUE)
  results_xlsx <- stats::setNames(
    all_results,
    vapply(names(all_results), function(s) {
      x <- gsub("[^A-Za-z0-9]", "_", s)
      substr(x, 1, min(31L, nchar(x)))
    }, "")
  )
  writexl::write_xlsx(results_xlsx, out_xlsx)
}

## Figuras: un archivo PNG por outcome (2×3: filas cs/sp, columnas contaminantes) ----
if (length(all_results) > 0) {
  for (dv in dependent_vars) {
    p_out <- build_outcome_panel(all_results, dv, contam_labels)
    fn <- file.path("Output/DLM", paste0("Malf_DLM_IQR_trimester_panel_", dv, ".png"))
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

all_results
