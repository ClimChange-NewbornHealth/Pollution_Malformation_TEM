# 5.5 Tables: modelos PM2.5 /10 + Levo/K bruto (salida de Code/5.4 Exposure_Models_Malf_10.R) -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos resultados generados por 5.4 ----

load("Output/Models/Exposure_models_malf_div10.RData")
results_malf <- dplyr::bind_rows(results_list)

load("Output/Models/Exposure_models_malf_card_bin_div10.RData")
results_card <- dplyr::bind_rows(results_list_card)

## 2. Etiquetas de filas ----

period_order <- c("pct1", "t1", "t2", "t3")
period_labels <- c(
  pct1 = "Preconception (pct1)",
  t1   = "Trimester 1",
  t2   = "Trimester 2",
  t3   = "Trimester 3"
)
period_order_joint_t123 <- c("t1", "t2", "t3")
period_order_joint_pct <- c("pct1", "t1", "t2", "t3")

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

# Coeficientes: `t1_PM25_cs_10` (PM2.5) o `t1_Levo_cs` (bruto)
period_from_div10_term <- function(term) {
  m <- stringr::str_match(term, "^(pct1|t1|t2|t3)_(PM25|Levo|K)_")
  ifelse(is.na(m[, 1L]), NA_character_, m[, 2L])
}

## 3. Construimos tablas tipo plantilla ----

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

## malf primero, luego malf_card ----

tab_malf_single <- build_template_table(results_malf, "malf")
tab_malf_joint_t123 <- build_joint_template_table(
  results_malf, "malf", "t1_t2_t3", period_order_joint_t123
)
tab_malf_joint_pct <- build_joint_template_table(
  results_malf, "malf", "pct1_t1_t2_t3", period_order_joint_pct
)

tab_card_single <- build_template_table(results_card, "malf_card_bin")
tab_card_joint_t123 <- build_joint_template_table(
  results_card, "malf_card_bin", "t1_t2_t3", period_order_joint_t123
)
tab_card_joint_pct <- build_joint_template_table(
  results_card, "malf_card_bin", "pct1_t1_t2_t3", period_order_joint_pct
)

footnote_single <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 as level divided by 10 (suffix _10 on predictor).",
  "Levo and K: raw concentration (baseline exposure column names).",
  "Adjustment: maternal age, infant sex, year of birth, season of conception.",
  "Each period (pct1, t1, t2, t3) estimated in a separate model (single-period predictors only)."
)

footnote_joint_t123 <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 /10 predictors (_10); Levo/K raw.",
  "Adjustment: maternal age, infant sex, year of birth, season of conception.",
  "One mutually adjusted model per pollutant and exposure type (mixed scaling)."
)

footnote_joint_pct <- paste(
  "Adjusted logistic models (OR as RR); PM2.5 /10 predictors (_10); Levo/K raw.",
  "Adjustment: maternal age, infant sex, year of birth, season of conception.",
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

## 4. Exportamos Excel ----

out_dir <- "Output/Tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

writexl::write_xlsx(
  list(
    `Table 2` = tab_malf_single,
    `Mutual t1+t2+t3` = tab_malf_joint_t123,
    `Mutual pct1+t1+t2+t3` = tab_malf_joint_pct,
    Footnote = footnote_tbl
  ),
  path = file.path(out_dir, "Table_div10_adjusted_malf.xlsx")
)

writexl::write_xlsx(
  list(
    `Table 2` = tab_card_single,
    `Mutual t1+t2+t3` = tab_card_joint_t123,
    `Mutual pct1+t1+t2+t3` = tab_card_joint_pct,
    Footnote = footnote_tbl
  ),
  path = file.path(out_dir, "Table_div10_adjusted_malf_card.xlsx")
)

tab_malf_single
tab_malf_joint_t123
tab_malf_joint_pct
tab_card_single
tab_card_joint_t123
tab_card_joint_pct
