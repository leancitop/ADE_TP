# 01_download_data.R
# Ejemplos de rutas y descarga (ajustar a sus fuentes reales)
library(data.table)
library(terra)

dir.create("data/raw/chirps", recursive = TRUE, showWarnings = FALSE)

# Nota: CHIRPS requiere descarga manual/API externa; esto es un placeholder.
# Guardar los TIF anuales/estacionales en data/raw/chirps/
message("Descargá/colocá los TIF de CHIRPS/WorldClim en data/raw/chirps/.")
