# 03_cv_maps.R
library(terra); library(data.table)

# Suponiendo stack anual de precipitación 1981-2023 en una misma resolución
files <- list.files("data/raw/chirps", pattern = "\\.(tif|tiff)$", full.names = TRUE)
if (length(files) < 5) stop("Necesitás varios años para CV")

s <- rast(files)
mean_r <- app(s, mean, na.rm=TRUE)
sd_r   <- app(s, sd, na.rm=TRUE)
cv_r   <- (sd_r / mean_r) * 100

dir.create("data/processed", showWarnings = FALSE, recursive = True)
writeRaster(mean_r, "data/processed/precip_mean.tif", overwrite=TRUE)
writeRaster(sd_r,   "data/processed/precip_sd.tif", overwrite=TRUE)
writeRaster(cv_r,   "data/processed/precip_cv.tif", overwrite=TRUE)
message("Exportado: precip_mean/sd/cv en data/processed")
