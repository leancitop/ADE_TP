library(devtools)
library(dotenv)
library(ecmwfr)
library(glue)

gxetools_dir <- "..."
load_all(gxetools_dir)

config <- create_era5_configuration(
  start_date = "2008'-01-01",
  end_date = "2025-3-30",
  area_bbox = c(-24.2612680173,-68.2441188274,-36.9506292975,-57.2577907024),
  area_name = "Argentina",
  variables = list(
    "2m_temperature" = c("daily_maximum", "daily_minimum")
  ),
  base_dir = "...",
  env_file = "...",
  timezone = "utc-03:00" # horario argentina
)

config <- setup_era5_simple_logging(config)

cat("🚀 Iniciando descarga con feedback detallado...\n")
result <- run_era5_pipeline(config, verbose = TRUE)
