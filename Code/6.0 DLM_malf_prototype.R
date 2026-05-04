# Code 6.0 DLM Malformaciones ----
# Aplicación de DLM para malf y malf_card_bin con pm25, levo y k
# Dos secuencias temporales: w-12 a max y w1 a max semanas

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
#source("Code/0.3 Funciones.R")

## Open Data ---- 
# Datos en formato wide con exposiciones semanales
#data_full <- rio::import("Output/Data_malf_exposure_wide.RData")
data_full <- rio::import("Output/Data_malf_exposure_long.RData") 

# Variables
dependent_vars <- c("malf", "malf_card_bin")
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")

## Preparamos los datos ---- 

# Ajustamos los datos
data_full_long <- data_full |> 
  filter(str_detect(tiempo, "w")) |> # Filtramos solo las observaciones con los datos de exposición semanal
  mutate(week = as.integer(str_extract(tiempo, "(?<=w)-?\\d+"))) |> 
  mutate(week_pct = as.integer(str_extract(tiempo, "(?<=w)-?\\d+"))+13) |> 
  arrange(malf, idbase, week)

glimpse(data_full_long) # 5.316.864
table(data_full_long$week)

# Transformamos los datos de exposición a formato wide 
malf_pm25_cs <- data_full_long |> 
  filter(tipo == "cs" & contaminante == "PM25") |>
  filter(week >= 1) |>
  select(all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |> 
  pivot_wider(
    names_from = week, 
    values_from = "exposicion", 
    names_prefix = "exposicion_")

glimpse(malf_pm25_cs) # Datos en formato wide 

# Especificamos los datos y ponderadores
malf_pm25_long_cs <- data_full_long |> 
  filter(tipo == "cs" & contaminante == "PM25") |>
  filter(week >= 1) |>
  select(all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |> 
  arrange(idbase, week) |> 
  group_by(idbase) |> 
  dplyr::mutate(exposicion_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
    if (week[i] == 0) return(NA_real_)
    past_rows <- which(week < week[i])
    weights <- 1 / (week[i] - week[past_rows])
    exposures <- exposicion[past_rows]
    sum(weights * exposures, na.rm = TRUE)
  })) 

setDT(malf_pm25_long_cs)
glimpse(malf_pm25_long_cs)

# Lagged en formato wide 
malf_pm25_long_cs_lagged <- malf_pm25_long_cs |> 
  select(idbase, week, exposicion_lagged) %>%
  pivot_wider(
    names_from = week,
    values_from = exposicion_lagged,
    names_prefix = "exposicion_lagged_"
  ) |> 
  ungroup()

data_pm25 <- left_join(malf_pm25_cs, 
  malf_pm25_long_cs_lagged,
  by = "idbase") |> 
  arrange(idbase) |> 
  #select(-exposicion_lagged_1) |> 
  drop_na(edad_madre) |> 
  filter(sexo_rn != "Indefinido")

setDT(data_pm25)
glimpse(data_pm25)

## Seleccionamos variables ---- 

# Exposition variables 
expo <- grep("^exposicion_\\d+$", names(data_pm25), value = TRUE)
expo_lag <- grep("^exposicion_lagged_\\d+$", names(data_pm25), value = TRUE)
expo
expo_lag

# Extract week numbers
expo_weeks <- as.numeric(gsub("exposicion_", "", expo))
expo_lag_weeks <- as.numeric(gsub("exposicion_lagged_", "", expo_lag))
expo_weeks 
expo_lag_weeks

# Semana (donde existe el lag)
common_weeks <- intersect(expo_weeks, expo_lag_weeks)
common_weeks <- common_weeks[common_weeks >= 2 & common_weeks <= 39]
common_weeks

# Generamos los vectores de trabajo
expo_vars <- paste0("exposicion_", common_weeks)
expo_vars_lag <- paste0("exposicion_lagged_", common_weeks)
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")

# Guardamos los resultados
res_combo <- data.table()

for (i in seq_along(expo_vars)) {
  
  exp <- expo_vars[i]
  lag <- expo_vars_lag[i]
  control_str <- paste(control_vars, collapse = " + ")

  formula_str <- paste0(
    "malf", " ~ ",
    exp, " + ", lag,
    " + ", control_str
  )
  
  fml <- as.formula(formula_str)
  mod <- glm(fml, data = data_pm25, family = binomial(link = "logit"))
  
  dat <- data.frame(
    Week = common_weeks[i],
    Exposure = exp,
    Lagged = lag,
    `No Obs`  = mod$df.null + 1,
    beta = unname(mod$coefficients[exp]),
    se = summary(mod)$coefficients[exp, "Std. Error"],
    AIC = AIC(mod),
    BIC = BIC(mod)
  )
  
  # CI and exp(beta)
  dat$Lower <- dat$beta - qnorm(0.975) * dat$se
  dat$Upper <- dat$beta + qnorm(0.975) * dat$se
  dat$beta_exp <- exp(dat$beta)
  dat$Lower_exp <- exp(dat$Lower)
  dat$Upper_exp <- exp(dat$Upper)
  
  res_combo <- rbind(res_combo, dat)
}

# Miramos los resultados
res_combo

write_xlsx(res_combo, "Output/DLM/Malf_DLM_results_pm25_cs.xlsx")

g1 <- ggplot(res_combo, aes(x = Week, y = beta_exp)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_exp, ymax = Upper_exp), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_continuous(limits = c(0.95, 1.05)) +
  scale_x_continuous(breaks= seq(1,41, by=3)) +
  labs(
    #title = expression(bold("A. Overall - PM" [2.5])),
    x = "Gestational Weeks",
    y = "OR (95% CI)"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

g1

ggsave("Output/DLM/Malf_DLM_models_pm25_cs.png", g1, 
       res = 300, width = 15, height = 10, units = "cm",
       scaling = 0.9, device = ragg::agg_png)
