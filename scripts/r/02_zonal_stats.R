# 02_zonal_stats.R
library(terra); library(sf); library(data.table)

rasters <- list.files("data/raw/chirps", pattern = "\\.(tif|tiff)$", full.names = TRUE)
if (!length(rasters)) stop("No hay rasters en data/raw/chirps")

# ejemplo de lectura de polígonos (regiones de interés)
# reemplazar por tu archivo real (GPKG o SHP)
roi_path <- "data/external/regiones.gpkg"
if (!file.exists(roi_path)) stop("Falta regiones.gpkg en data/external")

roi <- st_read(roi_path, quiet = TRUE)
rlist <- lapply(rasters, rast)

# exactextractr es más rápido, pero acá mostramos un flujo con terra::extract
# unir resultados por nombre de raster
out_list <- list()
for (i in seq_along(rlist)) {
  r <- rlist[[i]]
  v <- terra::extract(r, vect(roi), fun = mean, na.rm = TRUE)
  v$layer <- basename(rasters[i])
  out_list[[i]] <- v
}
res <- rbindlist(out_list, fill = TRUE)
fwrite(res, "data/processed/zonal_mean.csv")
message("Listo: data/processed/zonal_mean.csv")
