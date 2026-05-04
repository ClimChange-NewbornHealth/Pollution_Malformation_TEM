# 4.1 Data new analysis -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos de exposición ----
full_data <- rio::import("Output/Data_full_sample_predictions_exposure.RData")
glimpse(full_data)

## 2. Unimos los datos de exposición ----
exposure_data <- full_data |> 
  mutate(idbase = paste0("ID-", as.character(idbase))) |> 
  select(-malf)

glimpse(exposure_data)
unique(exposure_data$idbase)

## 3. Unimos los datos de exposición a la matriz pre-procesada ----
# Base con covariables de referencia
data <- rio::import("Output/base_final.RData") |> 
  select(idbase:malf_otra) |> 
  mutate(idbase = str_replace(idbase, "CK-|EB-", "ID-")) 

glimpse(data)

# Generamos outcomes de pretermino 
# Refs p10 weeks 28-42 (Alarcón & Pittaluga)
ref_p10 <- tribble(
  ~edad_gest, ~p10,
    28,  945.7,
    29, 1092.1,
    30, 1258.2,
    31, 1439.2,
    32, 1630.8,
    33, 1828.7,
    34, 2028.6,
    35, 2226.0,
    36, 2416.7,
    37, 2562.2,
    38, 2760.2,
    39, 2904.2,
    40, 3024.1,
    41, 3115.3,
    42, 3173.5,
    43,    NA_real_,  # dejamos NA para calcularlo
    44,    NA_real_
)

data <- data |> 
  left_join(ref_p10, by = "edad_gest") |> 
  mutate(p10 = if_else(is.na(p10), quantile(peso_rn, probs = 0.1, na.rm = TRUE), p10)) |> 
  mutate(birth_preterm = if_else(edad_gest < 37, 1, 0)) |>
  mutate(birth_extremely_preterm = if_else(edad_gest < 28, 1, 0)) |> 
  mutate(birth_very_preterm = if_else(edad_gest >= 28 & edad_gest <32, 1, 0)) |> 
  mutate(birth_moderately_preterm = if_else(edad_gest >= 32 & edad_gest <33, 1, 0)) |> 
  mutate(birth_late_preterm = if_else(edad_gest >= 34 & edad_gest <37, 1, 0)) |> 
  mutate(birth_term = if_else(edad_gest >= 37 & edad_gest <42, 1, 0)) |> 
  mutate(birth_posterm = if_else(edad_gest >= 42, 1, 0)) |> 
  mutate(lbw = if_else(peso_rn < 2500, 1, 0)) |> 
  mutate(tlbw = if_else(peso_rn < 2500 & edad_gest >= 37, 1, 0)) |> 
  mutate(sga = if_else(peso_rn < p10, 1, 0)) |> 
  select(-p10) 

glimpse(data)

# Unimos con los datos de exposición
data <- data |> 
  left_join(exposure_data, by = "idbase")

glimpse(data)

# Generamos la fecha de inicio y filtramos semana 1 desde el 2009-01-01
ids <- unique(exposure_data$idbase)

data_exp <- data |> 
  filter(idbase %in% ids) |>
  mutate(fecha_ini = fecha_nac - weeks(edad_gest-1)) |> 
  filter(fecha_ini >= as.Date("2009-01-01")) |> 
  relocate(fecha_ini, .after = idbase)

glimpse(data_exp) # 15557 

## 4. Guardamos los datos procesados ----

save(data_exp, file = "Output/Data_full_sample_exposure.RData")