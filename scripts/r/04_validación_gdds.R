library(terra)
library(fs)

dir_output <- "./score_target_loc/data/ERA5/output_gs"
TBASE  <- 10
TUPPER <- 30

path_tmin <- function(region, y0=2008, y1=y0+1)
  file.path(dir_output, "temps/agg", region, sprintf("%d", y0),
            sprintf("2m_temperature_min_gs_%d_%d.nc", y0, y1))
path_tmax <- function(region, y0=2008, y1=y0+1)
  file.path(dir_output, "temps/agg", region, sprintf("%d", y0),
            sprintf("2m_temperature_max_gs_%d_%d.nc", y0, y1))
path_gdd  <- function(region, y0=2008, y1=y0+1, base=TBASE, upper=TUPPER)
  file.path(dir_output, "gdds", region, sprintf("%d", y0),
            sprintf("gdd_base%d_upper%d_gs_%d_%d.nc", base, upper, y0, y1))

ensure_file <- function(p) { if (file.exists(p)) p else { p2 <- sub("\\.nc$",".tif",p); if(file.exists(p2)) p2 else stop("No existe: ", p)} }

# elijo un píxel NO-NA simultáneamente en rmin y rmax (hasta 200 intentos)
pick_xy_no_na_both <- function(rmin, rmax, tries=200){
  for(i in 1:tries){
    cell <- sample(ncell(rmin), 1)
    xy   <- xyFromCell(rmin, cell)
    # convertir a SpatVector puntual
    p <- vect(matrix(xy, ncol=2), crs=crs(rmin), type="points")
    v1 <- try(extract(rmin[[1]], p, ID=FALSE)[1,1], silent=TRUE)
    v2 <- try(extract(rmax[[1]], p, ID=FALSE)[1,1], silent=TRUE)
    if(!inherits(v1,"try-error") && !inherits(v2,"try-error") && !is.na(v1) && !is.na(v2)) {
      return(as.vector(xy))
    }
  }
  stop("No pude encontrar pixel válido en ambas capas.")
}

# cálculo “a mano” (vectores) de GDD simple capped
gdd_from_vectors <- function(tmin_c, tmax_c, base=TBASE, upper=TUPPER){
  tmin_cap <- pmax(pmin(tmin_c, upper), base)
  tmax_cap <- pmin(tmax_c, upper)
  tmin_ord <- pmin(tmin_cap, tmax_cap)
  tmax_ord <- pmax(tmin_cap, tmax_cap)
  pmax(((tmin_ord + tmax_ord)/2) - base, 0)
}

validate_region <- function(region, y0=2008){
  cat("\n========================\nREGIÓN:", region, "GS", y0, "-", y0+1, "\n")
  pminf <- ensure_file(path_tmin(region, y0))
  pmaxf <- ensure_file(path_tmax(region, y0))
  rmin  <- rast(pminf); rmax <- rast(pmaxf)
  
  # K -> °C
  rmin_c <- rmin - 273.15
  rmax_c <- rmax - 273.15
  
  # alinear si hace falta
  if (!isTRUE(all.equal(ext(rmin_c), ext(rmax_c))) ||
      !isTRUE(all.equal(res(rmin_c), res(rmax_c))) ||
      as.character(crs(rmin_c)) != as.character(crs(rmax_c))) {
    rmax_c <- resample(rmax_c, rmin_c, method="bilinear")
  }
  
  # punto aleatorio válido en ambas
  xy <- pick_xy_no_na_both(rmin_c, rmax_c)
  cat(sprintf("Punto aleatorio: lon=%.5f  lat=%.5f\n", xy[1], xy[2]))
  p <- vect(matrix(xy, ncol=2), crs=crs(rmin_c), type="points")
  
  # extraer series (ID=FALSE => sin columna ID; 1 fila x n_bandas)
  vmin <- as.numeric(extract(rmin_c, p, ID=FALSE)[1, ])
  vmax <- as.numeric(extract(rmax_c, p, ID=FALSE)[1, ])
  dates <- time(rmin_c)
  
  # sanity checks
  stopifnot(length(vmin) == nlyr(rmin_c),
            length(vmax) == nlyr(rmax_c),
            length(dates) == nlyr(rmin_c),
            nlyr(rmin_c) == nlyr(rmax_c))
  
  # cálculo manual
  gdd_manual <- gdd_from_vectors(vmin, vmax, TBASE, TUPPER)
  
  tab <- data.frame(
    date = as.Date(dates),
    tmin_c = round(vmin, 2),
    tmax_c = round(vmax, 2),
    gdd_manual = round(gdd_manual, 3)
  )
  
  # comparación con raster GDD (si existe)
  gdd_path_try <- path_gdd(region, y0)
  gdd_path <- if (file.exists(gdd_path_try) || file.exists(sub("\\.nc$",".tif", gdd_path_try))) ensure_file(gdd_path_try) else NA_character_
  
  if (!is.na(gdd_path)) {
    rgdd <- rast(gdd_path)
    if (!isTRUE(all.equal(ext(rgdd), ext(rmin_c))) ||
        !isTRUE(all.equal(res(rgdd), res(rmin_c))) ||
        as.character(crs(rgdd)) != as.character(crs(rmin_c))) {
      rgdd <- resample(rgdd, rmin_c, method="bilinear")
    }
    vgdd <- as.numeric(extract(rgdd, p, ID=FALSE)[1, ])
    stopifnot(length(vgdd) == nlyr(rgdd), nlyr(rgdd) == nlyr(rmin_c))
    tab$gdd_raster <- round(vgdd, 3)
    tab$diff <- round(tab$gdd_manual - tab$gdd_raster, 5)
    cat(sprintf("Comparación con %s\n", basename(gdd_path)))
    cat(sprintf("Error abs. máximo: %.6f\n", max(abs(tab$diff), na.rm=TRUE)), "\n")
    cat(sprintf("Error abs. medio:  %.6f\n", mean(abs(tab$diff), na.rm=TRUE)), "\n")
  } else {
    cat("Sin raster GDD para comparar (se muestra solo cálculo manual).\n")
  }
  
  print(head(tab, 10))
  invisible(tab)
}      

set.seed(123)
tab_ARG <- validate_region("ARG", 2008)
tab_SDA <- validate_region("SDA", 2008)

# (Opcional) resumen de errores si hubo comparación
if ("diff" %in% names(tab_ARG)) print(summary(tab_ARG$diff))
if ("diff" %in% names(tab_SDA)) print(summary(tab_SDA$diff))
