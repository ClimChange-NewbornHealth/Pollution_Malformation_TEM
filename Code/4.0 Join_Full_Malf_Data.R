# 4.0 Join data frames -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos de exposición ----
malf_data <- rio::import("Output/Data_malf_sample_predictions_exposure.RData")
full_data <- rio::import("Output/Data_full_sample_predictions_exposure.RData")

## 2. Unimos los datos de exposición ----
exposure_data <- malf_data |> 
  bind_rows(full_data) |> 
  relocate(malf, .after = idbase) |> 
  mutate(idbase = paste0("ID-", as.character(idbase)))

glimpse(exposure_data)
unique(exposure_data$idbase)

## 3. Unimos los datos de exposición a la matriz pre-procesada ----
# Esto es a partir de la matriz pre-procesada de Ismael
data <- rio::import("Output/base_final.RData")
glimpse(data)

# Ajustamos la base previo a la unión 
data <- data |> 
  select(idbase, fecha_nac:malf_otra) |> 
  mutate(idbase = str_replace(idbase, "CK-|EB-", "ID-"))

glimpse(data)
unique(data$idbase)

table(c(unique(exposure_data$idbase) %in% unique(data$idbase)))
table(c(unique(data$idbase) %in% unique(exposure_data$idbase)))

# Unimos 
data_full_wide <- data |> 
  left_join(exposure_data,
            by = c("idbase"))

# Ajustamos los datos
data_full_wide <- data_full_wide  |>
  mutate(
    malf_num = as.numeric(malf_num),
    malf_mult = as.numeric(malf_mult),
    malf = as.factor(malf),
    sexo_rn = as.factor(sexo_rn),
    comuna = as.factor(comuna),
    estacion = as.factor(estacion),
    fecha_inicio = as.Date(fecha_nac - days(edad_gest * 7)),
    mes_inicio = month(fecha_inicio),
    a_nac = as.factor(a_nac),
    estacion = case_when(
      mes_inicio %in% c(12, 1, 2) ~ "Summer",
      mes_inicio %in% c(3, 4, 5)  ~ "Fall",
      mes_inicio %in% c(6, 7, 8)  ~ "Winter",
      mes_inicio %in% c(9, 10, 11) ~ "Spring",
      TRUE ~ NA_character_), 
    season_cold = ifelse(estacion %in% c("Fall", "Winter"), 1, 0), 
    season_winter_only = ifelse(estacion == "Winter", 1, 0)
  )

# Ajustes de variables de malformaciones
vars_malf <- c(
  "malf_nerv", "malf_ococ", "malf_card", "malf_resp",
  "malf_dig", "malf_og", "malf_uri", "malf_om",
  "malf_otra"
)

data_full_wide <- data_full_wide  |>
  mutate(
    across(
      all_of(vars_malf),
      ~ as.numeric(if_else(is.na(.) | . == 0, 0, 1)),
      .names = "{.col}_bin"
    )
  )

data_full_wide <- data_full_wide |>
  relocate(fecha_inicio:season_winter_only, .after = edad_gest) |> 
  relocate(malf_nerv_bin:malf_otra_bin, .after = malf)

# Ajustamos casos duplicados para malf 

data_full_wide <- data_full_wide |> 
  distinct(idbase, .keep_all = TRUE) |> 
  filter(!is.na(malf))

table(data_full_wide$malf, useNA = "ifany")

glimpse(data_full_wide)

# Pasamos a formato long (columnas de exposición)
exposure_cols <- names(data_full_wide)[grepl("(_PM25_|_Levo_|_K_)", names(data_full_wide))]

data_full_long <- data_full_wide |> 
  tidyr::pivot_longer(
    cols = all_of(exposure_cols),
    names_to = "metrica",
    values_to = "exposicion"
  ) 

data_full_long <- data_full_long  |> 
  mutate(
    tiempo = stringr::str_extract(metrica, "^[^_]+"),
    contaminante = stringr::str_extract(metrica, "PM25|Levo|K"),
    tipo = stringr::str_extract(metrica, "cs|sp")
  )

glimpse(data_full_long)

## 4. Hacemos un check id de los datos ----
check_ids_wide <- rio::import("Output/base_final_rev.RData")
check_ids_wide <- check_ids_wide |> 
  select(idbase) |> 
  mutate(idbase = str_replace(idbase, "CK-|EB-", "ID-"))

ids_org <- unique(data_full_wide$idbase) # 15824
ids_check <- unique(check_ids_wide$idbase) # 15452
# 372 ids deben ser removidos para que la tabla de datos sea equivalente a la original

id_remove <- setdiff(ids_org, ids_check) # 372 ids
id_remove

data_full_wide <- data_full_wide |> 
  filter(!idbase %in% id_remove) 

data_full_long <- data_full_long |> 
  filter(!idbase %in% id_remove) 

table(data_full_wide$malf, useNA = "ifany")
table(data_full_long$malf, useNA = "ifany")

## 5. Agregamos métricas con IQR y x/10----

exposure_vars <- data_full_wide |> 
  select(
    starts_with(c("pct1", "t1", "t2", "t3", "tot", "w"))
  ) |> 
  colnames()

exposure_vars # 360 cols

# exposure_vars/IQR(exposure_vars)
iqr_vals <- data_full_wide |>
  summarise(across(all_of(exposure_vars), ~ IQR(.x, na.rm = TRUE))) |>
  as.list()

iqr_vals # Calculate IQR per variable 

writexl::write_xlsx(iqr_vals |> data.frame(), paste0("Input/", "Data_IQR_ref_values.xlsx"))

data_full_wide  <- data_full_wide |> 
  mutate(across(all_of(exposure_vars), ~ .x / iqr_vals[[cur_column()]], .names = "iqr_{.col}")) |> 
  mutate(across(all_of(exposure_vars), ~ .x / 10, .names = "{.col}_10"))

glimpse(data_full_wide)

## 6. Guardamos los resultados ----
save(data_full_wide, file = "Output/Data_malf_exposure_wide.RData")
save(data_full_long, file = "Output/Data_malf_exposure_long.RData")

## 7. Descriptive exposure -----

# Función para construir tabla descriptiva de exposición por contaminante
build_exposure_table <- function(data, contaminant, group_name = "all") {
  
  # Identificamos las columnas de exposición para el contaminante específico
  exposure_cols <- names(data)[grepl(paste0("_", contaminant, "_"), names(data))]
  
  # Definimos los períodos de tiempo que queremos incluir
  # Nota: para semanas negativas, el formato en los nombres puede ser "w-12" o "w-12"
  time_periods <- c("pct1", "t1", "t2", "t3", "tot", "w20",
                    paste0("w", -12:43))
  
  # Función auxiliar para formatear valores como Mediana (P25 - P75)
  format_median_iqr <- function(values) {
    if (length(values) == 0 || all(is.na(values))) {
      return(NA_character_)
    }
    values_clean <- na.omit(values)
    if (length(values_clean) == 0) {
      return(NA_character_)
    }
    median_val <- median(values_clean)
    p25 <- quantile(values_clean, 0.25)
    p75 <- quantile(values_clean, 0.75)
    iqr <- p75 - p25
    return(sprintf("%.2f (IQR=%.2f)", median_val, iqr))
  }
  
  # Creamos data frame para almacenar resultados para el contaminante específico
  result_table <- data.frame(
    periodo = time_periods,
    cs = NA_character_,
    sp = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Calculamos mediana e IQR para cada período de tiempo para el contaminante específico
  for (period in time_periods) {
    # Escapamos el guión en períodos negativos para regex
    period_escaped <- gsub("-", "\\\\-", period)
    
    # Buscamos columnas que coincidan con el período y el contaminante específico
    # Patrón: periodo_contaminante_cs o periodo_contaminante_sp
    cs_pattern <- paste0("^", period_escaped, "_", contaminant, "_cs$")
    sp_pattern <- paste0("^", period_escaped, "_", contaminant, "_sp$")
    
    cs_cols <- exposure_cols[grepl(cs_pattern, exposure_cols)]
    sp_cols <- exposure_cols[grepl(sp_pattern, exposure_cols)]
    
    # Calculamos mediana e IQR de las columnas encontradas
    if (length(cs_cols) > 0) {
      cs_values <- unlist(data[, cs_cols, drop = FALSE])
      result_table$cs[result_table$periodo == period] <- format_median_iqr(cs_values)
    }
    
    if (length(sp_cols) > 0) {
      sp_values <- unlist(data[, sp_cols, drop = FALSE])
      result_table$sp[result_table$periodo == period] <- format_median_iqr(sp_values)
    }
  }
  
  return(result_table)
}

# Construimos las 9 tablas (3 grupos × 3 contaminantes)
contaminants <- c("PM25", "Levo", "K")
groups <- list(
  "Full_sample" = data_full_wide,
  "Malf_0" = data_full_wide |> filter(malf == 0 | is.na(malf)),
  "Malf_1" = data_full_wide |> filter(malf == 1), 
  "Malf_card_bin_0" = data_full_wide |> filter(malf_card_bin == 0 | is.na(malf_card_bin)),
  "Malf_card_bin_1" = data_full_wide |> filter(malf_card_bin == 1)
)

# Creamos lista para almacenar todas las tablas
tables_list <- list()

# Generamos las 9 tablas
for (group_name in names(groups)) {
  for (contaminant in contaminants) {
    table_name <- paste0(group_name, "_", contaminant)
    tables_list[[table_name]] <- build_exposure_table(
      groups[[group_name]], 
      contaminant = contaminant, 
      group_name = group_name
    )
  }
}

# Guardamos las tablas en Excel
writexl::write_xlsx(
  tables_list,
  path = "Output/Descriptive_exposure_tables_malf.xlsx"
)
