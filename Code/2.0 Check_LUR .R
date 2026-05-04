# 2.0 Check LUR models ----
# Modelo pre-parametrizado 

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

# 1. Load LUR models ----
load("Input/LUR_modelos_cs.RData")

# 2. Check LUR models ----
# Mediciones en una fecha dada con rotación en 15 lugares de la zona urbana.
# Ubicación fija / fecha dada
# La unidad de análisis es espacio-temporal 
# Datos independientes de las gestantes 
summary(modeloKcs)
summary(modelolevocs)
summary(modelopmcs)

