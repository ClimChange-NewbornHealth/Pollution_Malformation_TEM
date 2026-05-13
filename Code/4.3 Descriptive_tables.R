# 4.3 Descriptive Analysis -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos ----

data_full_wide <- rio::import("Output/Data_malf_exposure_wide.RData")
#data_full_long <- rio::import("Output/Data_malf_exposure_long.RData")
glimpse(data_full_wide)

## 2. Análisis descriptivo ----

table(data_full_wide$malf)
only_malf <- data_full_wide |> 
  dplyr::filter(malf==1) |> 
  select(
    malf_card_bin, 
    malf_nerv_bin, 
    malf_resp_bin, 
    malf_dig_bin, 
    malf_uri_bin, 
    malf_og_bin, 
    malf_om_bin, 
    malf_ococ_bin
  )

glimpse(only_malf)

table(only_malf$malf_card_bin)
table(only_malf$malf_nerv_bin)
table(only_malf$malf_resp_bin)
table(only_malf$malf_dig_bin)
table(only_malf$malf_uri_bin)
table(only_malf$malf_og_bin)
table(only_malf$malf_om_bin)
table(only_malf$malf_ococ_bin)

# Upset plot para malformaciones
names(only_malf) <- c(
  "Congenital heart disease", 
  "Nervous system", 
  "Respiratory", 
  "Digestive", 
  "Urinary",
  "Genital", 
  "Muscle", 
  "Ocular and craniofacial")

png(
  filename = "Output/Upset_malformation.png",
  width = 37.5,
  height = 20,
  units = "cm",
  res = 300
)
UpSetR::upset(
  only_malf,
  sets = rev(names(only_malf)),
  nsets = 8,
  nintersects = NA,
  order.by = "freq",
  decreasing = c(TRUE, TRUE),
  keep.order = TRUE,
  #sets.bar.color = "#56B4E9",
  #main.bar.color = "#0072B2",
  point.size = 2.5, 
  line.size = 0.75, 
  matrix.color = "gray50",
  text.scale = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5)
  ) 
dev.off()

Vars <- c("edad_gest", "sexo_rn", "estacion", "a_nac", "edad_madre")
labs <- c("Gestational age", "Sex, male", "Season of conception", "Year of birth", "Maternal age")

# Values are n (%) or median (interquartile range)
# Chi-square (correct=TRUE → corrección de Yates en tablas 2×2) o Wilcoxon rank sum

## Helpers formato ----
fmt_one_dec <- function(x) {
  x <- round(as.numeric(x), 1)
  neg <- x < 0
  s <- sprintf("%.1f", abs(x))
  parts <- strsplit(s, ".", fixed = TRUE)[[1]]
  int_part <- parts[[1]]
  dec_part <- parts[[2]]
  int_part <- gsub("(\\d)(?=(\\d{3})+$)", "\\1,", int_part, perl = TRUE)
  paste0(if (neg) "-" else "", int_part, ".", dec_part)
}

fmt_int <- function(x) {
  s <- sprintf("%d", as.integer(abs(x)))
  gsub("(\\d)(?=(\\d{3})+$)", "\\1,", s, perl = TRUE)
}

fmt_num_median_iqr <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return("")
  qs <- stats::quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  sprintf("%s (%s-%s)", fmt_one_dec(qs[2]), fmt_one_dec(qs[1]), fmt_one_dec(qs[3]))
}

fmt_cat_cell <- function(subdf, varnm, lev) {
  x <- subdf[[varnm]]
  ok <- !is.na(x)
  x <- x[ok]
  if (!length(x)) return("")
  n <- sum(as.character(x) == lev, na.rm = TRUE)
  pct <- 100 * n / length(x)
  sprintf("%s (%s%%)", fmt_int(n), fmt_one_dec(pct))
}

fmt_p <- function(p) {
  if (length(p) != 1L || is.na(p)) return("")
  if (p < 0.01) return("<0.01")
  sprintf("%.2f", round(p, 2))
}

p_chisq <- function(d_a, d_b, varnm, levs) {
  x <- c(as.character(d_a[[varnm]]), as.character(d_b[[varnm]]))
  g <- factor(c(rep(1L, nrow(d_a)), rep(2L, nrow(d_b))), levels = 1:2)
  x <- factor(x, levels = levs)
  tab <- table(g, x)
  suppressWarnings(stats::chisq.test(tab, correct = TRUE)$p.value)
}

p_wilcox_two <- function(d_a, d_b, varnm) {
  xa <- d_a[[varnm]][is.finite(d_a[[varnm]])]
  xb <- d_b[[varnm]][is.finite(d_b[[varnm]])]
  stats::wilcox.test(xa, xb)$p.value
}

## Subconjuntos ----
dat <- data_full_wide |> dplyr::filter(!is.na(malf))

dat_m0 <- dat |> dplyr::filter(as.character(malf) == "0")
dat_m1 <- dat |> dplyr::filter(as.character(malf) == "1")
dat_heart <- dat |> dplyr::filter(!is.na(malf_card_bin), malf_card_bin == 1)

n0 <- nrow(dat_m0)
n1 <- nrow(dat_m1)
nh <- nrow(dat_heart)

hdr_no <- paste0("Malformation (-) n=", fmt_int(n0))
hdr_yes <- paste0("Malformation (+) n=", fmt_int(n1))
hdr_heart <- paste0("Heart Malformation (+) n=", fmt_int(nh))

vars_cat <- c("sexo_rn", "estacion", "a_nac")
vars_num <- c("edad_gest", "edad_madre")

hdr_p_malf <- "p-value (Malformation + vs -)"
hdr_p_heart <- "p-value (Heart vs Malformation -)"

build_table <- function() {
  blocks <- list()

  for (vn in Vars) {
    lb <- labs[match(vn, Vars)]

    if (vn %in% vars_num) {
      p_any <- p_wilcox_two(dat_m0, dat_m1, vn)
      p_ht <- p_wilcox_two(dat_m0, dat_heart, vn)
      blocks[[length(blocks) + 1L]] <- tibble::tibble(
        Variables = lb,
        !!hdr_no := fmt_num_median_iqr(dat_m0[[vn]]),
        !!hdr_yes := fmt_num_median_iqr(dat_m1[[vn]]),
        !!hdr_p_malf := fmt_p(p_any),
        !!hdr_heart := fmt_num_median_iqr(dat_heart[[vn]]),
        !!hdr_p_heart := fmt_p(p_ht)
      )
      next
    }

    if (vn %in% vars_cat) {
      levs <- levels(droplevels(as.factor(dat[[vn]])))
      p_any <- tryCatch(p_chisq(dat_m0, dat_m1, vn, levs), error = function(e) NA_real_)
      p_ht <- tryCatch(p_chisq(dat_m0, dat_heart, vn, levs), error = function(e) NA_real_)

      blocks[[length(blocks) + 1L]] <- tibble::tibble(
        Variables = lb,
        !!hdr_no := "",
        !!hdr_yes := "",
        !!hdr_p_malf := fmt_p(p_any),
        !!hdr_heart := "",
        !!hdr_p_heart := fmt_p(p_ht)
      )

      for (lev in levs) {
        blocks[[length(blocks) + 1L]] <- tibble::tibble(
          Variables = paste0("  ", lev),
          !!hdr_no := fmt_cat_cell(dat_m0, vn, lev),
          !!hdr_yes := fmt_cat_cell(dat_m1, vn, lev),
          !!hdr_p_malf := "",
          !!hdr_heart := fmt_cat_cell(dat_heart, vn, lev),
          !!hdr_p_heart := ""
        )
      }
    }
  }

  dplyr::bind_rows(blocks)
}

tab_desc <- build_table()

writexl::write_xlsx(
  list(`Descriptive malformation` = tab_desc),
  path = file.path("Output", "Descriptive_table_malformation.xlsx")
)
