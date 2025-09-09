library(devtools)
library(dotenv)
library(ecmwfr)
library(glue)

gxetools_dir <- "..."
load_all(gxetools_dir)

config <- create_era5_configuration(
  start_date = "2008'-01-01",
  end_date = "2025-3-30",
  area_bbox = c(-25.552447324, 26.553787703,-29.9344699399,31.0793081895),
  area_name = "Southafrica",
  variables = list(
    "2m_temperature" = c("daily_maximum", "daily_minimum")
  ),
  base_dir = "...",
  env_file = "...",
  timezone = "utc+02:00" # horario sudafrica
)

config <- setup_era5_simple_logging(config)

cat("🚀 Iniciando descarga con feedback detallado...\n")
result <- run_era5_pipeline(config, verbose = TRUE)
