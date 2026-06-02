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


## 5. Tabla de correlaciones entre exposiciones (pct1, t1, t2, t3) ----

contaminants <- c("PM25", "Levo", "K")
cor_periods <- c("pct1", "t1", "t2", "t3")
cor_period_labels <- c("pct1", "t1", "t2", "t3")

build_correlation_matrices <- function(data, tipo) {
  out <- list()
  for (cont in contaminants) {
    vars <- paste0(cor_periods, "_", cont, "_", tipo)
    dat <- data |>
      dplyr::select(dplyr::all_of(vars))
    colnames(dat) <- cor_period_labels

    cor_mat <- stats::cor(dat, use = "pairwise.complete.obs", method = "pearson")
    rownames(cor_mat) <- cor_period_labels
    colnames(cor_mat) <- cor_period_labels
    cor_mat <- round(cor_mat, 2)

    p_mat <- matrix(NA_real_, nrow = length(cor_period_labels), ncol = length(cor_period_labels),
                    dimnames = list(cor_period_labels, cor_period_labels))
    for (i in seq_along(cor_period_labels)) {
      for (j in seq_along(cor_period_labels)) {
        if (i == j) next
        ct <- tryCatch(
          stats::cor.test(dat[[i]], dat[[j]], method = "pearson", exact = FALSE),
          error = function(e) list(p.value = NA_real_)
        )
        p_mat[i, j] <- ct$p.value
      }
    }

    out[[paste0("cor_", cont, "_", tipo)]] <- as.data.frame(cor_mat) |>
      tibble::rownames_to_column("period")
    out[[paste0("pval_", cont, "_", tipo)]] <- as.data.frame(p_mat) |>
      tibble::rownames_to_column("period")
  }
  out
}

tabla_correlaciones <- c(
  build_correlation_matrices(data_full_wide, "cs"),
  build_correlation_matrices(data_full_wide, "sp")
)

writexl::write_xlsx(
  tabla_correlaciones,
  path = file.path("Output", "Correlation_exposure_table.xlsx")
)

## 6. Figuras de dispersión entre ventanas de exposición ----

pairs_list <- list(
  c("pct1", "t1"), c("pct1", "t2"), c("pct1", "t3"), c("pct1", "tot"),
  c("t1", "t2"), c("t1", "t3"), c("t2", "t3"),
  c("t1", "tot"), c("t2", "tot"), c("t3", "tot")
)
pair_labels <- c(
  "PCT1 vs T1", "PCT1 vs T2", "PCT1 vs T3", "PCT1 vs Overall",
  "T1 vs T2", "T1 vs T3", "T2 vs T3",
  "T1 vs Overall", "T2 vs Overall", "T3 vs Overall"
)
contaminant_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

format_pval_label <- function(p) {
  if (is.na(p)) return(" = NA")
  if (p < 0.001) return(" < 0.001")
  paste0(" = ", sub(",", ".", format.pval(p, digits = 3, eps = 0.001), fixed = TRUE))
}

make_cor_label <- function(x, y, data) {
  test <- stats::cor.test(data[[x]], data[[y]], exact = FALSE)
  r <- formatC(test$estimate, format = "f", digits = 2, decimal.mark = ".")
  paste0("r = ", r, ", p", format_pval_label(test$p.value))
}

build_scatter_data <- function(type_suffix) {
  out_list <- list()
  for (cont in contaminants) {
    for (k in seq_along(pairs_list)) {
      x_var <- paste0(pairs_list[[k]][1], "_", cont, type_suffix)
      y_var <- paste0(pairs_list[[k]][2], "_", cont, type_suffix)
      dat <- data_full_wide |>
        dplyr::select(x = dplyr::all_of(x_var), y = dplyr::all_of(y_var)) |>
        dplyr::filter(stats::complete.cases(x, y))
      if (nrow(dat) < 10) next
      fit <- stats::lm(y ~ x, data = dat)
      dat$resid <- abs(stats::residuals(fit))
      dat$intensity <- 1 - (dat$resid / max(dat$resid, na.rm = TRUE))
      dat$contaminant <- factor(contaminant_labels[cont], levels = unname(contaminant_labels))
      dat$pair <- factor(pair_labels[k], levels = pair_labels)
      out_list[[paste(cont, k, sep = "_")]] <- dat
    }
  }
  dplyr::bind_rows(out_list)
}

build_label_data <- function(type_suffix) {
  out_list <- list()
  for (cont in contaminants) {
    for (k in seq_along(pairs_list)) {
      x_var <- paste0(pairs_list[[k]][1], "_", cont, type_suffix)
      y_var <- paste0(pairs_list[[k]][2], "_", cont, type_suffix)
      dat <- data_full_wide |>
        dplyr::select(dplyr::all_of(c(x_var, y_var)))
      dat <- dat[stats::complete.cases(dat), , drop = FALSE]
      if (nrow(dat) < 10) next
      lab <- make_cor_label(x_var, y_var, dat)
      x_pos <- max(dat[[x_var]], na.rm = TRUE)
      y_pos <- max(dat[[y_var]], na.rm = TRUE)
      out_list[[paste(cont, k, sep = "_")]] <- data.frame(
        contaminant = factor(contaminant_labels[cont], levels = unname(contaminant_labels)),
        pair = factor(pair_labels[k], levels = pair_labels),
        label = lab,
        x_pos = x_pos,
        y_pos = y_pos,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(out_list)
}

plot_scatter_one_contaminant <- function(data_plot, data_labels, cont_label) {
  ggplot(data_plot, aes(x = x, y = y, color = intensity)) +
    geom_point(alpha = 0.7, size = 1.2) +
    scale_color_viridis_c(option = "plasma", limits = c(0, 1), guide = "none") +
    geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha = 0.3, linewidth = 1, se = TRUE, inherit.aes = FALSE, aes(x = x, y = y)) +
    geom_text(
      data = data_labels,
      aes(x = x_pos, y = y_pos, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1,
      size = 3.5
    ) +
    facet_wrap(~ pair, scales = "free", nrow = 1) +
    scale_x_continuous(labels = scales::label_number(decimal.mark = "."), expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = "."), expand = expansion(mult = c(0.02, 0.05))) +
    labs(x = NULL, y = NULL, title = cont_label) +
    theme_light(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0, margin = margin(b = 6)),
      strip.text.x = element_text(size = 10, color = "black"),
      strip.placement = "outside",
      strip.background = element_rect(fill = "white", color = "gray95"),
      legend.position = "none"
    )
}

build_scatter_figure <- function(type_suffix) {
  data_all <- build_scatter_data(type_suffix)
  labels_all <- build_label_data(type_suffix)
  plots <- lapply(unname(contaminant_labels), function(cont_label) {
    dat <- dplyr::filter(data_all, contaminant == cont_label)
    lab <- dplyr::filter(labels_all, contaminant == cont_label)
    if (nrow(dat) == 0) return(NULL)
    plot_scatter_one_contaminant(dat, lab, cont_label)
  })
  plots <- plots[!vapply(plots, is.null, logical(1))]
  ggpubr::ggarrange(
    plotlist = plots,
    nrow = length(plots),
    ncol = 1,
    heights = rep(1, length(plots))
  )
}

fig_scatter_cs <- build_scatter_figure("_cs")
fig_scatter_sp <- build_scatter_figure("_sp")

ggsave(
  filename = file.path("Output", "Scatter_correlations_exposure_cs.png"),
  plot = fig_scatter_cs,
  width = 60,
  height = 7 * length(contaminant_labels),
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

ggsave(
  filename = file.path("Output", "Scatter_correlations_exposure_sp.png"),
  plot = fig_scatter_sp,
  width = 60,
  height = 7 * length(contaminant_labels),
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

