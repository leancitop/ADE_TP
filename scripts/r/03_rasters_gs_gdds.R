# --- GDDs diarios por Growing Season (ARG/SDA) ---
# Insumos (anuales, diarios):
#   ./score_target_loc/data/ERA5/shift_rasters/temps/agg/{REGION}/{YYYY}/2m_temperature_{min|max}_{YYYY}.nc
# Salida (una banda por día del GS):
#   ./score_target_loc/data/ERA5/output_gs/gdds/{REGION}/{YYYY}/gdd_base<TBASE>_upper<TUPPER>_gs_YYYY_YYYY+1.(nc|tif)
# --- CONFIG ---
library(terra)
library(ncdf4)
library(fs)

dir_base   <- "./score_target_loc/data/ERA5/shift_rasters"
dir_output <- "./score_target_loc/data/ERA5/output_gs"

# Parámetros GDD
TBASE  <- 10   # °C
TUPPER <- 30   # °C

# --- Helpers: tiempo desde NetCDF ---
nc_time_vector_any <- function(nc_path, nlyr_hint = NULL) {
  nc <- nc_open(nc_path); on.exit(nc_close(nc), add = TRUE)
  cand_names <- c("time","valid_time","time_counter","t","step",
                  "forecast_reference_time","forecast_time","leadtime")
  var_names  <- names(nc$var)
  dim_names  <- names(nc$dim)
  name <- intersect(cand_names, c(var_names, dim_names))
  if (length(name) == 0) name <- intersect(tolower(cand_names), tolower(c(var_names, dim_names)))
  if (length(name) == 0) name <- character(0)
  
  get_units <- function(nm){
    att <- try(ncatt_get(nc, nm, "units")$value, silent = TRUE)
    if (!inherits(att, "try-error") && !is.null(att)) return(att)
    if (nm %in% dim_names) {
      u <- nc$dim[[nm]]$units
      if (!is.null(u)) return(u)
    }
    NULL
  }
  get_vals <- function(nm){
    if (nm %in% var_names)      return(ncvar_get(nc, nm))
    else if (nm %in% dim_names) return(nc$dim[[nm]]$vals)
    else                        return(NULL)
  }
  
  if (length(name) > 0) {
    nm <- name[1]; vals <- get_vals(nm); units <- get_units(nm)
    if (!is.null(units) && grepl("since", units, ignore.case = TRUE)) {
      origin_str <- sub(".*since\\s*", "", units, ignore.case = TRUE)
      origin_str <- sub("Z$", "", origin_str)
      origin <- suppressWarnings(as.POSIXct(origin_str, tz="UTC"))
      if (is.na(origin)) origin <- suppressWarnings(as.POSIXct(substr(origin_str, 1, 19), tz="UTC"))
      mult <- if (grepl("^\\s*days", units, ignore.case=TRUE)) 24*3600 else
        if (grepl("^\\s*hours", units, ignore.case=TRUE)) 3600    else
          if (grepl("^\\s*seconds", units, ignore.case=TRUE)) 1     else NA_real_
      if (is.na(mult)) stop("Unidades de tiempo no soportadas: ", units)
      return(as.Date(origin + vals * mult))
    }
  }
  
  # Fallback por nombre y nlyr
  if (is.null(nlyr_hint)) {
    nlyr_hint <- try(nlyr(rast(nc_path)), silent = TRUE)
    if (inherits(nlyr_hint, "try-error")) nlyr_hint <- NULL
  }
  year <- as.integer(sub(".*_(\\d{4})\\.nc$", "\\1", nc_path))
  if (is.na(year) || is.null(nlyr_hint)) {
    stop("No hay variable/dim de tiempo ni puedo inferir fechas desde el nombre.")
  }
  seq.Date(as.Date(sprintf("%d-01-01", year)), by="day", length.out=nlyr_hint)
}

attach_time <- function(path) {
  r <- rast(path)
  tm <- try(time(r), silent = TRUE)
  if (inherits(tm, "try-error") || all(is.na(tm))) {
    d <- nc_time_vector_any(path, nlyr(r))
    time(r) <- d
    names(r) <- format(d, "%Y-%m-%d")
  }
  r
}

# --- Ventanas GS ---
gs_window <- function(region, y_start) {
  if (region == "ARG") {
    start <- as.Date(sprintf("%d-09-15", y_start))
  } else if (region == "SDA") {
    start <- as.Date(sprintf("%d-10-15", y_start))
  } else {
    stop("Región no soportada: ", region)
  }
  end <- start + 124  # 125 días
  list(start = start, end = end)
}

# --- Chequeo de geometría robusto ---
same_geom <- function(a, b) {
  same_ext <- isTRUE(all.equal(terra::ext(a), terra::ext(b)))
  same_res <- isTRUE(all.equal(terra::res(a), terra::res(b)))
  crs_a <- try(as.character(terra::crs(a)), silent = TRUE)
  crs_b <- try(as.character(terra::crs(b)), silent = TRUE)
  same_crs <- !inherits(crs_a, "try-error") && !inherits(crs_b, "try-error") && identical(crs_a, crs_b)
  same_ext && same_res && same_crs
}

# --- Descubrir años disponibles ---
years_available <- function(root_subdir, region) {
  base <- file.path(dir_base, root_subdir, region)
  if (!dir_exists(base)) return(integer(0))
  ys <- dir_ls(base, type = "directory", recurse = FALSE)
  as.integer(basename(ys))
}

# --- I/O paths ---
var_defs <- list(
  tmin = list(
    subdir   = "temps/agg",
    filename = function(y) sprintf("2m_temperature_min_%d.nc", y)
  ),
  tmax = list(
    subdir   = "temps/agg",
    filename = function(y) sprintf("2m_temperature_max_%d.nc", y)
  )
)

build_paths <- function(var_type, region, year) {
  file.path(dir_base, var_type$subdir, region, sprintf("%d", year), var_type$filename(year))
}

# --- Writer: NetCDF si hay driver; si no, GeoTIFF ---
write_out <- function(r, out_path_nc) {
  dir.create(dirname(out_path_nc), recursive = TRUE, showWarnings = FALSE)
  ok <- try({
    terra::writeRaster(r, out_path_nc, overwrite = TRUE, filetype = "CDF")
  }, silent = TRUE)
  if (!inherits(ok, "try-error")) {
    message(">> Escrito (NetCDF): ", out_path_nc); return(invisible(out_path_nc))
  }
  out_path_tif <- sub("\\.nc$", ".tif", out_path_nc)
  ok2 <- try({
    terra::writeRaster(r, out_path_tif, overwrite = TRUE, filetype = "GTiff")
  }, silent = TRUE)
  if (inherits(ok2, "try-error")) {
    stop("No pude escribir ni como NetCDF ni como GeoTIFF. Error original: ", as.character(ok))
  } else {
    message(">> Driver NetCDF no disponible. Escrito (GeoTIFF): ", out_path_tif)
    return(invisible(out_path_tif))
  }
}

# --- Cargar Y y Y+1, alinear y recortar GS ---
load_concat_crop <- function(region, var_key, y) {
  v  <- var_defs[[var_key]]
  pY  <- build_paths(v, region, y)
  pY1 <- build_paths(v, region, y + 1L)
  if (!file_exists(pY) || !file_exists(pY1)) return(NULL)
  
  rY  <- attach_time(pY)
  rY1 <- attach_time(pY1)
  
  if (!same_geom(rY, rY1)) {
    rY1 <- terra::resample(rY1, rY, method = "bilinear")
  }
  
  r_all <- c(rY, rY1)
  d_all <- time(r_all)
  
  # Saneo fechas NA
  if (any(is.na(d_all))) {
    keep <- !is.na(d_all)
    r_all <- r_all[[keep]]
    d_all <- d_all[keep]
    if (length(d_all) == 0) return(NULL)
  }
  
  w <- gs_window(region, y)
  sel <- d_all >= w$start & d_all <= w$end
  if (!any(sel)) return(NULL)
  
  r_gs <- r_all[[sel]]
  time(r_gs) <- d_all[sel]
  names(r_gs) <- format(d_all[sel], "%Y-%m-%d")
  r_gs
}

# --- Cálculo GDD diarios por GS para una región ---
process_region_gdd <- function(region) {
  ys <- sort(years_available("temps/agg", region))  # referenciamos por tmin/tmax
  if (length(ys) == 0) {
    message("No hay años para GDD en ", region)
    return(invisible(NULL))
  }
  
  for (y in ys) {
    r_tmin <- load_concat_crop(region, "tmin", y)
    r_tmax <- load_concat_crop(region, "tmax", y)
    if (is.null(r_tmin) || is.null(r_tmax)) {
      message("Saltando GDD ", region, " ", y, "-", y+1, ": falta tmin/tmax o no hay capas.")
      next
    }
    
    # Kelvin -> °C
    r_tmin_c <- r_tmin - 273.15
    r_tmax_c <- r_tmax - 273.15
    
    # Alinear grilla si hace falta
    if (!same_geom(r_tmin_c, r_tmax_c)) {
      r_tmax_c <- terra::resample(r_tmax_c, r_tmin_c, method = "bilinear")
    }
    
    # Caps con clamp (sin lógicas)
    tmin_cap <- terra::clamp(r_tmin_c, lower = TBASE, upper = TUPPER, values = TRUE)
    tmax_cap <- terra::clamp(r_tmax_c, lower = -Inf,  upper = TUPPER, values = TRUE)
    
    # Ordenar para asegurar Tmin' <= Tmax' (puro álgebra, sin comparaciones)
    s <- tmin_cap + tmax_cap
    d <- abs(tmax_cap - tmin_cap)
    tmin_ord <- (s - d) / 2
    tmax_ord <- (s + d) / 2
    
    # GDD diario (simple) y piso 0
    tmean_cap <- (tmin_ord + tmax_ord) / 2
    gdd <- tmean_cap - TBASE
    gdd <- terra::ifel(gdd < 0, 0, gdd)
    
    # Preservar tiempo/nombres
    time(gdd)  <- time(r_tmin_c)
    names(gdd) <- names(r_tmin_c)
    
    # Salida
    out_dir  <- file.path(dir_output, "gdds", region, sprintf("%d", y))
    out_file <- sprintf("gdd_base%d_upper%d_gs_%d_%d.nc", TBASE, TUPPER, y, y + 1L)
    out_path <- file.path(out_dir, out_file)
    write_out(gdd, out_path)
  }
}

# --- Ejecutar GDD ---
process_region_gdd("ARG")
process_region_gdd("SDA")

message("GDDs listos en: ", file.path(dir_output, "gdds"))
