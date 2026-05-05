# Code 3.0 Prediction LUR models ----
# Nota: se utilizan datas pre-procesadas y modelos pre-parametrizados por Ruiz et al.

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Funciones.R")

## 1. Cargamos datos y modelos --------------------------------------------------------

# Cargamos los modelos LUR
load("Input/LUR_modelos_cs.RData")

# Cargamos los ambientes de datos preparados en 1.0 Preparation_LUR_Data.R
load("Input/LUR_data_environments.RData")

# Extraemos el ambiente malf_sample para trabajar solo con esta muestra
env_malf <- LUR_data$malf_sample

# Extraemos los objetos del ambiente malf_sample
base <- get("base", envir = env_malf)
pmcs <- get("pmcs", envir = env_malf)
modelopm <- get("modelopm", envir = env_malf)
modelolevo <- get("modelolevo", envir = env_malf)
modelok <- get("modelok", envir = env_malf)

# Verificamos qué objetos están disponibles
print(ls(env_malf))

## 2. Preparamos las bases de datos --------------------------------------------------------

# Preparamos las variables del sitio central
pmcs$LLEE_PM25 <- pmcs$PM25LE
pmcs$LLEE_TEMP <- pmcs$Temp

# Guardamos una copia de la base inicial
baseinit <- base

# Filtramos datos desde 2009-01-01
base <- base[which(base$fechaini >= as.Date("2009-01-01")), ]

## 3. Fuunciones auxiliares --------------------------------------------------------

## 3.1 Función para colapsar contaminación del aire en rango de fechas ----

ColapseByMeanDates 

ColapseByMeanDates(as.Date("2010-06-01"), as.Date("2010-06-07"),pmcs,"fecha","LLEE_PM25")

## 3.2 Función para predecir PM por modelo en rango de tiempo ----

SolutionByDates 

# Checkeamos 
modelolevo
test<-SolutionByDates(modelolevo, "levo", base$idbase[1],as.Date("2010-06-01"), 8, as.Date("2010-12-14"))
test

## 3.3 Función para predecir por semana individual ----

SolutionByWeek 

# Check
test<-SolutionByWeek(modelolevo, "levo", base$idbase[1],as.Date("2010-06-01"), 8, as.Date("2010-12-14"))
test

# Calculamos el número máximo de semanas gestacionales desde fechaini hasta fechafin
max_weeks_gestational <- max(round((base$fechafin - base$fechaini) / 7, 0), na.rm = TRUE)
cat("Número máximo de semanas gestacionales:", max_weeks_gestational)

## 4 Predicciones modelo PM25 --------------------------------------------------------

## 4.1 Predicciones sitio central (CS) ----

# Predicciones estándar del código de referencia
tic()
base$pct1_PM25_cs <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[1, ]
base$t1_PM25_cs <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini, 13, base$fechafin)[1, ]
base$t2_PM25_cs <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini + 13*7, 14, base$fechafin)[1, ]
base$t3_PM25_cs <- mapply(SolutionByDates_trimester3, list(modelopm), "pm",
                          base$idbase, base$fechaini, base$fechafin)[1, ]
base$w20_PM25_cs <- mapply(SolutionByDates, list(modelopm), "pm",
                           base$idbase, base$fechaini, 20, base$fechafin)[1, ]
base$tot_PM25_cs <- mapply(SolutionByDates, list(modelopm), "pm",
                           base$idbase, base$fechaini,
                           round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                           base$fechafin)[1, ]
toc() # 6 sec

## 4.2 Predicciones datos espaciales (SP) ----

tic()
base$pct1_PM25_sp <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[2, ]
base$t1_PM25_sp <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini, 13, base$fechafin)[2, ]
base$t2_PM25_sp <- mapply(SolutionByDates, list(modelopm), "pm",
                          base$idbase, base$fechaini + 13*7, 14, base$fechafin)[2, ]
base$t3_PM25_sp <- mapply(SolutionByDates_trimester3, list(modelopm), "pm",
                          base$idbase, base$fechaini, base$fechafin)[2, ]
base$w20_PM25_sp <- mapply(SolutionByDates, list(modelopm), "pm",
                           base$idbase, base$fechaini, 20, base$fechafin)[2, ]
base$tot_PM25_sp <- mapply(SolutionByDates, list(modelopm), "pm",
                           base$idbase, base$fechaini,
                           round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                           base$fechafin)[2, ]
toc() # 6 sec 

## 4.3 Predicciones por semana de gestación (desde -12 semanas hasta final del período) ----

# Creamos columnas para cada semana desde -12 hasta el final del período gestacional
tic()
for (week in -12:max_weeks_gestational) {
  week_name_cs <- paste0("w", week, "_PM25_cs")
  week_name_sp <- paste0("w", week, "_PM25_sp")
  
  # Predicciones sitio central
  base[[week_name_cs]] <- mapply(SolutionByWeek, list(modelopm), "pm",
                                 base$idbase, base$fechaini, week, base$fechafin)[1, ]
  
  # Predicciones espaciales
  base[[week_name_sp]] <- mapply(SolutionByWeek, list(modelopm), "pm",
                                  base$idbase, base$fechaini, week, base$fechafin)[2, ]
}
toc() # 30 sec

## 5 Predicciones modelo Levo --------------------------------------------------------

## 5.1 Predicciones sitio central (CS) ----

tic()
base$pct1_Levo_cs <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[1, ]
base$t1_Levo_cs <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini, 13, base$fechafin)[1, ]
base$t2_Levo_cs <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini + 13*7, 14, base$fechafin)[1, ]
base$t3_Levo_cs <- mapply(SolutionByDates_trimester3, list(modelolevo), "levo",
                          base$idbase, base$fechaini, base$fechafin)[1, ]
base$w20_Levo_cs <- mapply(SolutionByDates, list(modelolevo), "levo",
                           base$idbase, base$fechaini, 20, base$fechafin)[1, ]
base$tot_Levo_cs <- mapply(SolutionByDates, list(modelolevo), "levo",
                           base$idbase, base$fechaini,
                           round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                           base$fechafin)[1, ]
toc() # 7 sec

## 5.2 Predicciones datos espaciales (SP) ----

tic()
base$pct1_Levo_sp <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[2, ]
base$t1_Levo_sp <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini, 13, base$fechafin)[2, ]
base$t2_Levo_sp <- mapply(SolutionByDates, list(modelolevo), "levo",
                          base$idbase, base$fechaini + 13*7, 14, base$fechafin)[2, ]
base$t3_Levo_sp <- mapply(SolutionByDates_trimester3, list(modelolevo), "levo",
                          base$idbase, base$fechaini, base$fechafin)[2, ]
base$w20_Levo_sp <- mapply(SolutionByDates, list(modelolevo), "levo",
                           base$idbase, base$fechaini, 20, base$fechafin)[2, ]
base$tot_Levo_sp <- mapply(SolutionByDates, list(modelolevo), "levo",
                           base$idbase, base$fechaini,
                           round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                           base$fechafin)[2, ]
toc() #7 sec 

## 5.3 Predicciones por semana de gestación (desde -12 semanas hasta final del período) ----

tic()
for (week in -12:max_weeks_gestational) {
  week_name_cs <- paste0("w", week, "_Levo_cs")
  week_name_sp <- paste0("w", week, "_Levo_sp")
  
  base[[week_name_cs]] <- mapply(SolutionByWeek, list(modelolevo), "levo",
                                 base$idbase, base$fechaini, week, base$fechafin)[1, ]
  
  base[[week_name_sp]] <- mapply(SolutionByWeek, list(modelolevo), "levo",
                                  base$idbase, base$fechaini, week, base$fechafin)[2, ]
}
toc()

## 6. Predicciones modelo K --------------------------------------------------------

## 6.1 Predicciones sitio central (CS) ----

tic()
base$pct1_K_cs <- mapply(SolutionByDates, list(modelok), "k",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[1, ]
base$t1_K_cs <- mapply(SolutionByDates, list(modelok), "k",
                       base$idbase, base$fechaini, 13, base$fechafin)[1, ]
base$t2_K_cs <- mapply(SolutionByDates, list(modelok), "k",
                       base$idbase, base$fechaini + 13*7, 14, base$fechafin)[1, ]
base$t3_K_cs <- mapply(SolutionByDates_trimester3, list(modelok), "k",
                       base$idbase, base$fechaini, base$fechafin)[1, ]
base$w20_K_cs <- mapply(SolutionByDates, list(modelok), "k",
                        base$idbase, base$fechaini, 20, base$fechafin)[1, ]
base$tot_K_cs <- mapply(SolutionByDates, list(modelok), "k",
                        base$idbase, base$fechaini,
                        round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                        base$fechafin)[1, ]
toc() # 7 sec

## 6.2 Predicciones datos espaciales (SP) ----

tic()
base$pct1_K_sp <- mapply(SolutionByDates, list(modelok), "k",
                          base$idbase, base$fechaini - 12*7, 12, base$fechafin)[2, ]
base$t1_K_sp <- mapply(SolutionByDates, list(modelok), "k",
                       base$idbase, base$fechaini, 13, base$fechafin)[2, ]
base$t2_K_sp <- mapply(SolutionByDates, list(modelok), "k",
                       base$idbase, base$fechaini + 13*7, 14, base$fechafin)[2, ]
base$t3_K_sp <- mapply(SolutionByDates_trimester3, list(modelok), "k",
                       base$idbase, base$fechaini, base$fechafin)[2, ]
base$w20_K_sp <- mapply(SolutionByDates, list(modelok), "k",
                        base$idbase, base$fechaini, 20, base$fechafin)[2, ]
base$tot_K_sp <- mapply(SolutionByDates, list(modelok), "k",
                        base$idbase, base$fechaini,
                        round((base$fechafin - base$fechaini) / 7, 0) + 2, 
                        base$fechafin)[2, ]
toc() # 7 sec

## 6.3 Predicciones por semana de gestación (desde -12 semanas hasta final del período) ----

tic()
for (week in -12:max_weeks_gestational) {
  week_name_cs <- paste0("w", week, "_K_cs")
  week_name_sp <- paste0("w", week, "_K_sp")
  
  base[[week_name_cs]] <- mapply(SolutionByWeek, list(modelok), "k",
                                 base$idbase, base$fechaini, week, base$fechafin)[1, ]
  
  base[[week_name_sp]] <- mapply(SolutionByWeek, list(modelok), "k",
                                  base$idbase, base$fechaini, week, base$fechafin)[2, ]
}
toc()

## 7. Guardamos los resultados --------------------------------------------------------

# Guardamos el objeto SpatialPointsDataFrame completo
base_exposure <- base@data

base_exposure_malf <- base_exposure |> 
  select(idbase, starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_"),
         matches("^w-?\\d+_")) |> 
  mutate(malf = 1)

glimpse(base_exposure_malf)

# Guardamos los resultados
save(base_exposure_malf, 
     file = "Output/Data_malf_sample_predictions_exposure.RData")
