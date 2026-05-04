# 4.1 Descriptive figure exposure -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos ----

data_full_wide <- rio::import("Output/Data_malf_exposure_wide.RData")
#data_full_long <- rio::import("Output/Data_malf_exposure_long.RData")
glimpse(data_full_wide)

## 2. Generamos la tabla ----

# Función para construir tabla descriptiva de exposición por contaminante
build_exposure_table <- function(data, contaminant, group_name = "all") {
  
  # Identificamos las columnas de exposición para el contaminante específico
  exposure_cols <- names(data)[grepl(paste0("_", contaminant, "_"), names(data))]
  
  # Definimos los períodos de tiempo que queremos incluir (solo pct1, t1, t2, t3)
  time_periods <- c("pct1", "t1", "t2", "t3")
  
  # Creamos lista para almacenar resultados
  result_list <- list()
  
  # Calculamos mediana, p25 y p75 para cada período de tiempo y tipo
  for (period in time_periods) {
    # Escapamos el guión en períodos negativos para regex
    period_escaped <- gsub("-", "\\\\-", period)
    
    # Buscamos columnas que coincidan con el período y el contaminante específico
    # Patrón: periodo_contaminante_cs o periodo_contaminante_sp
    cs_pattern <- paste0("^", period_escaped, "_", contaminant, "_cs$")
    sp_pattern <- paste0("^", period_escaped, "_", contaminant, "_sp$")
    
    cs_cols <- exposure_cols[grepl(cs_pattern, exposure_cols)]
    sp_cols <- exposure_cols[grepl(sp_pattern, exposure_cols)]
    
    # Función auxiliar para calcular estadísticas
    calc_stats <- function(values) {
      if (length(values) == 0 || all(is.na(values))) {
        return(list(median = NA_real_, p25 = NA_real_, p75 = NA_real_))
      }
      values_clean <- na.omit(values)
      if (length(values_clean) == 0) {
        return(list(median = NA_real_, p25 = NA_real_, p75 = NA_real_))
      }
      return(list(
        median = median(values_clean),
        p25 = quantile(values_clean, 0.25),
        p75 = quantile(values_clean, 0.75)
      ))
    }
    
    # Calculamos estadísticas para CS
    if (length(cs_cols) > 0) {
      cs_values <- unlist(data[, cs_cols, drop = FALSE])
      cs_stats <- calc_stats(cs_values)
      result_list[[length(result_list) + 1]] <- data.frame(
        muestra = group_name,
        contaminante = contaminant,
        periodo = period,
        tipo = "cs",
        median = cs_stats$median,
        p25 = cs_stats$p25,
        p75 = cs_stats$p75,
        stringsAsFactors = FALSE
      )
    }
    
    # Calculamos estadísticas para SP
    if (length(sp_cols) > 0) {
      sp_values <- unlist(data[, sp_cols, drop = FALSE])
      sp_stats <- calc_stats(sp_values)
      result_list[[length(result_list) + 1]] <- data.frame(
        muestra = group_name,
        contaminante = contaminant,
        periodo = period,
        tipo = "sp",
        median = sp_stats$median,
        p25 = sp_stats$p25,
        p75 = sp_stats$p75,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combinamos todos los resultados en un solo data.frame
  if (length(result_list) > 0) {
    result_table <- do.call(rbind, result_list)
    return(result_table)
  } else {
    return(data.frame(
      muestra = character(0),
      contaminante = character(0),
      periodo = character(0),
      tipo = character(0),
      median = numeric(0),
      p25 = numeric(0),
      p75 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
}

# Construimos las 9 tablas (3 grupos × 3 contaminantes)
contaminants <- c("PM25", "Levo", "K")
groups <- list(
  "Full_sample" = data_full_wide,
  "Malf_0" = data_full_wide |> filter(malf == 0 | is.na(malf)),
  "Malf_1" = data_full_wide |> filter(malf == 1)
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

# Combinamos todas las tablas en una sola base de datos
exposure_table <- do.call(rbind, tables_list)

# Retornamos la tabla (no la guardamos)
exposure_table

## 3. Generamos la figura con medianas e IQR ----

exposure_table |> 
  #filter(muestra=="Full_sample") |> 
  ggplot(aes(x = periodo, y = median, color = tipo)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line() +
  geom_errorbar(aes(ymin = p25, ymax = p75), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(muestra ~ contaminante, scales = "free_y") +
  #facet_grid(~ contaminante, scales = "free_y") +
  labs(title = "Mediana de exposición por período y tipo",
       x = "Período de tiempo",
       y = "Mediana de exposición") +
  theme_light()

## 4. Generamos boxplots ----

# Transformamos los datos a formato largo para boxplots
# Creamos lista para almacenar datos transformados
exposure_long_list <- list()

contaminants <- c("PM25", "Levo", "K")
time_periods <- c("pct1", "t1", "t2", "t3")
tipos <- c("cs", "sp")
groups <- list(
  "Full_sample" = data_full_wide,
  "Malf_0" = data_full_wide |> filter(malf == 0 | is.na(malf)),
  "Malf_1" = data_full_wide |> filter(malf == 1),
  "Malf_card_1" = data_full_wide |> filter(!is.na(malf_card_bin), malf_card_bin == 1)
)

# Transformamos cada grupo a formato largo
for (group_name in names(groups)) {
  for (contaminant in contaminants) {
    # Identificamos las columnas de exposición para el contaminante específico
    exposure_cols <- names(groups[[group_name]])[grepl(paste0("_", contaminant, "_"), names(groups[[group_name]]))]
    
    # Transformamos cada período y tipo a formato largo
    for (period in time_periods) {
      # Escapamos el guión en períodos negativos para regex
      period_escaped <- gsub("-", "\\\\-", period)
      
      # Buscamos columnas que coincidan con el período y el contaminante específico
      cs_pattern <- paste0("^", period_escaped, "_", contaminant, "_cs$")
      sp_pattern <- paste0("^", period_escaped, "_", contaminant, "_sp$")
      
      cs_cols <- exposure_cols[grepl(cs_pattern, exposure_cols)]
      sp_cols <- exposure_cols[grepl(sp_pattern, exposure_cols)]
      
      # Creamos datos para CS
      if (length(cs_cols) > 0) {
        cs_values <- unlist(groups[[group_name]][, cs_cols, drop = FALSE])
        cs_df <- data.frame(
          muestra = group_name,
          contaminante = contaminant,
          periodo = period,
          tipo = "cs",
          exposicion = cs_values,
          stringsAsFactors = FALSE
        )
        exposure_long_list[[length(exposure_long_list) + 1]] <- cs_df
      }
      
      # Creamos datos para SP
      if (length(sp_cols) > 0) {
        sp_values <- unlist(groups[[group_name]][, sp_cols, drop = FALSE])
        sp_df <- data.frame(
          muestra = group_name,
          contaminante = contaminant,
          periodo = period,
          tipo = "sp",
          exposicion = sp_values,
          stringsAsFactors = FALSE
        )
        exposure_long_list[[length(exposure_long_list) + 1]] <- sp_df
      }
    }
  }
}

# Combinamos todos los datos en una sola base
exposure_long <- do.call(rbind, exposure_long_list)

# Eliminamos valores faltantes
exposure_long <- exposure_long |> 
  filter(muestra != "Full_sample") |>
  filter(!is.na(exposicion)) |> 
  mutate(
    periodo = factor(periodo, levels = time_periods, labels = c("PCT1", "T1", "T2", "T3")),
    tipo = factor(tipo, levels = c("cs", "sp"), labels = c("Fixed Site", "Land-use Regression")),
    contaminante = factor(contaminante, levels = contaminants, labels = c("PM2.5", "Levoglucosan", "Ksol")),
    muestra = factor(
      muestra,
      levels = c("Malf_0", "Malf_1", "Malf_card_1"),
      labels = c(
        "Congenital Malformation (-)",
        "Congenital Malformation (+)",
        "Congenital Heart Malformation (+)"
      )
    )
  ) |> 
  filter(exposicion <= 200) |> 
  mutate(aux = if_else(contaminante == "Ksol" & exposicion > 3, 1, 0)) |> 
  filter(aux == 0) 

# Creamos boxplots
boxplot <- ggplot(exposure_long, aes(x = periodo, y = exposicion, fill = tipo)) +
  geom_boxplot(width=0.5, outlier.shape = NA, outlier.size = 0.5, alpha = 0.5) +
  #geom_violin(width=0.5, alpha= 0.3) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  scale_fill_discrete(palette = "dark2") +
  facet_grid(contaminante ~ muestra, scales = "free_y", axes = "all", axis.labels = "all_x") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    legend.text = element_text(size = 12),
    strip.text.y = element_text(size = 12, color = "black", angle = 0),
    strip.text.x = element_text(size = 12, color = "black"),
    strip.background.y = element_rect(fill = "white", color = "white", size = 0.5),
    strip.background.x = element_rect(fill = "white", color = "black", size = 0.5),
  )

boxplot

# Guardamos la figura
ggsave("Output/Exposure_boxplot.png", 
       boxplot,
       res = 300, 
       width = 37.5, 
       height = 20, 
       units = "cm",
       scaling = 1, 
       device = ragg::agg_png)


