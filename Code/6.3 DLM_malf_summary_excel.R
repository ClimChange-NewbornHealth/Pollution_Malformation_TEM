source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

options(OutDec = ".")

path_gest <- "Output/DLM/Malf_DLM_results_all_models_IQR.xlsx"
path_pct <- "Output/DLM/PCT/Malf_DLM_results_PCT_models_IQR.xlsx"
path_out <- "Output/DLM/Malf_DLM_summary_DLM.xlsx"

weeks_gest <- 2:39
weeks_pct <- -11:39

sheet_pm25_10 <- function(dep, tipo) {
  paste0(dep, "_PM25_10_", tipo)
}

sheet_contam <- function(dep, contam, tipo) {
  paste(dep, contam, tipo, sep = "_")
}

read_fmt <- function(path, dep, tipo, path_is_pct) {
  pm <- readxl::read_excel(path, sheet = sheet_pm25_10(dep, tipo))
  le <- readxl::read_excel(path, sheet = sheet_contam(dep, "Levo", tipo))
  kk <- readxl::read_excel(path, sheet = sheet_contam(dep, "K", tipo))
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
  pmf <- dplyr::rename(to_cell(pm), PM25 = "cell")
  lef <- dplyr::rename(to_cell(le), Levo = "cell")
  kkf <- dplyr::rename(to_cell(kk), K = "cell")
  wk <- if (isTRUE(path_is_pct)) weeks_pct else weeks_gest
  tibble::tibble(Week = wk) |>
    dplyr::left_join(pmf, by = "Week") |>
    dplyr::left_join(lef, by = "Week") |>
    dplyr::left_join(kkf, by = "Week")
}

out_list <- list(
  malf_cs_gest = read_fmt(path_gest, "malf", "cs", path_is_pct = FALSE),
  malf_sp_gest = read_fmt(path_gest, "malf", "sp", path_is_pct = FALSE),
  malf_card_cs_gest = read_fmt(path_gest, "malf_card_bin", "cs", path_is_pct = FALSE),
  malf_card_sp_gest = read_fmt(path_gest, "malf_card_bin", "sp", path_is_pct = FALSE),
  malf_cs_pct = read_fmt(path_pct, "malf", "cs", path_is_pct = TRUE),
  malf_sp_pct = read_fmt(path_pct, "malf", "sp", path_is_pct = TRUE),
  malf_card_cs_pct = read_fmt(path_pct, "malf_card_bin", "cs", path_is_pct = TRUE),
  malf_card_sp_pct = read_fmt(path_pct, "malf_card_bin", "sp", path_is_pct = TRUE)
)

if (!dir.exists("Output/DLM")) {
  dir.create("Output/DLM", recursive = TRUE)
}

writexl::write_xlsx(out_list, path_out)
