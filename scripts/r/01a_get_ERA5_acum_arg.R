

# ---- CARGA DE LIBRARÍA GxETools ----
library(devtools)
library(dotenv)
library(ecmwfr)
library(glue)
# Cargar librería GxETools
gxetools_dir <- "..."
load_all(gxetools_dir)

load_era5_dependencies()

# ---- CONFIGURACIÓN PARA VARIABLES ACUMULADAS ----

# Crear configuración para descarga ERA5 variables acumuladas
config <- create_era5_accumulated_configuration(
  start_date = "2008-01-01",
  end_date = "2025-3-30",
  area_bbox = c(-24.68900,-68.17652,-37.03857,-58.11940),
  area_name = "Argentina",
  variables = c("total_precipitation", # Precipitación total acumulada),
  base_dir = "...",
  env_file = "..."
)


# Ejecutar pipeline ERA5 para variables acumuladas con verbose detallado
cat("🚀 Iniciando descarga de variables acumuladas con feedback detallado...\n")
result <- run_era5_accumulated_pipeline(config, verbose = TRUE)