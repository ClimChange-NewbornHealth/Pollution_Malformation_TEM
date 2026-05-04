# Code 1.0 Preparation LUR Data set ----
# Nota: se utilizan datas pre-procesadas por Ruiz et al.

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## 1. Cargamos los datos ----
full <- "Input/full_sample/"
malf <- "Input/malf_sample/"

# Creamos una función para cargar datos en un ambiente específico
load_data_to_env <- function(data_path, env) {
  # Lista de archivos a cargar
  files <- c("Data1_women.RData", 
             "Data2_spatial.RData", 
             "Data3_CentralSite.RData", 
             "Data4_extractions.RData")
  
  # Cargar cada archivo en el ambiente especificado
  for(file in files) {
    file_path <- file.path(data_path, file)
    if(file.exists(file_path)) {
      load(file_path, envir = env)
    } else {
      warning(paste("Archivo no encontrado:", file_path))
    }
  }
}

## 2. Creamos dos ambientes separados para cada muestra -----
env_full <- new.env()
env_malf <- new.env()

# Cargamos datos de full_sample en su ambiente
load_data_to_env(full, env_full)

# Cargamos datos de malf_sample en su ambiente
load_data_to_env(malf, env_malf)

# Creamos objeto estructurado que contiene ambos ambientes
LUR_data <- list(
  full_sample = env_full,
  malf_sample = env_malf
)

## 3. Guardamos un objeto con ambos ambientes ------
save(LUR_data, file = "Input/LUR_data_environments.RData")

