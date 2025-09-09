library(terra)
library(ncdf4)
library(fs)

dir_base <- "./data/ERA5/shift_rasters"

# inspección rasters 
if (TRUE){
  # Lee vector Date desde var o dimensión de tiempo (time/valid_time/etc.)
  nc_time_vector_any <- function(nc_path, nlyr_hint = NULL) {
    nc <- nc_open(nc_path); on.exit(nc_close(nc), add = TRUE)
    cand_names <- c("time","valid_time","time_counter","t","step","forecast_reference_time")
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
    
    # Fallback: reconstruir por nombre ..._YYYY.nc si no hay units/vals
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
  
  # Imprime resumen "geoespacial" del raster y un vistazo al tiempo
  print_raster_summary <- function(path) {
    cat("\n=== RASTER (terra) ===\n", path, "\n")
    r <- rast(path)
    print(r)
    cat("Capas:", nlyr(r), "\n")
    cat("Resolución:", res(r), "\n")
    cat("CRS:", crs(r), "\n")
    # Min/Max de la 1ª capa (rápido)
    rg <- global(r[[1]], range, na.rm=TRUE)
    cat("Rango (capa 1):", as.numeric(rg[1,1]), "→", as.numeric(rg[1,2]), "\n")
    # Tiempo: intenta leerlo; si no, muestra NA
    tm <- try(time(r), silent=TRUE)
    if (inherits(tm,"try-error") || all(is.na(tm))) {
      cat("time(r): no definido por terra → intento con ncdf4...\n")
      d <- try(nc_time_vector_any(path, nlyr(r)), silent=TRUE)
      if (!inherits(d,"try-error")) {
        cat("Fechas (head):", paste(head(d,3), collapse=", "), "\n")
        cat("Fechas (tail):", paste(tail(d,3), collapse=", "), "\n")
      } else {
        cat("No pude extraer fechas (ncdf4). Error:", as.character(d), "\n")
      }
    } else {
      cat("Fechas (head):", paste(head(tm,3), collapse=", "), "\n")
      cat("Fechas (tail):", paste(tail(tm,3), collapse=", "), "\n")
    }
    invisible(r)
  }
  
  # Imprime resumen "NetCDF" (dims, vars, units, missing, tiempo)
  print_nc_summary <- function(path) {
    cat("\n=== NETCDF (ncdf4) ===\n", path, "\n")
    nc <- nc_open(path); on.exit(nc_close(nc), add=TRUE)
    cat("Dimensiones:\n")
    for (nm in names(nc$dim)) {
      d <- nc$dim[[nm]]
      cat(sprintf("  - %s: len=%d  units=%s\n", nm, d$len, ifelse(is.null(d$units),"", d$units)))
    }
    cat("Variables:\n")
    for (nm in names(nc$var)) {
      v <- nc$var[[nm]]
      units <- ncatt_get(nc, nm, "units")$value
      longn <- ncatt_get(nc, nm, "long_name")$value
      miss1 <- ncatt_get(nc, nm, "missing_value")$value
      miss2 <- ncatt_get(nc, nm, "_FillValue")$value
      cat(sprintf("  - %s  (long_name=%s; units=%s; missing=%s %s)\n",
                  nm, ifelse(is.null(longn),"",longn),
                  ifelse(is.null(units),"",units),
                  ifelse(is.null(miss1),"",as.character(miss1)),
                  ifelse(is.null(miss2),"",paste0("/ _FillValue=",miss2))))
    }
    # Tiempo (si existe como var o dim)
    cand <- intersect(c("time","valid_time","time_counter","t","step","forecast_reference_time"),
                      c(names(nc$var), names(nc$dim)))
    if (length(cand)) {
      nm <- cand[1]
      units <- try(ncatt_get(nc, nm, "units")$value, silent = TRUE)
      if (inherits(units,"try-error") || is.null(units)) units <- nc$dim[[nm]]$units
      cat("Tiempo:\n")
      cat("  nombre:", nm, " | units:", units, "\n")
      vals <- try(if (nm %in% names(nc$var)) ncvar_get(nc, nm) else nc$dim[[nm]]$vals, silent = TRUE)
      if (!inherits(vals,"try-error")) {
        cat("  valores (head):", paste(head(vals,3), collapse=", "), "\n")
        cat("  valores (tail):", paste(tail(vals,3), collapse=", "), "\n")
      }
    } else {
      cat("Tiempo: no encontrado como var/dim\n")
    }
  }
  
  p_tmin <- file.path(dir_base, "temps/agg/ARG/2008/2m_temperature_min_2008.nc")
  p_tmax <- file.path(dir_base, "temps/agg/ARG/2008/2m_temperature_max_2008.nc")
  p_prec <- file.path(dir_base, "precs/acum/ARG/2008/total_precipitation_acum_2008.nc")
  
  # Resumen geoespacial + tiempo inferido
  print_raster_summary(p_tmin)
  print_raster_summary(p_tmax)
  print_raster_summary(p_prec)
  
  # Resumen NetCDF (dims, vars, units, tiempo bruto)
  print_nc_summary(p_tmin)
  print_nc_summary(p_tmax)
  print_nc_summary(p_prec)
}

# rasters gs
if(TRUE){
  # --- CONFIG ---
    dir_output <- "./data/ERA5/output_gs"
  
  # --- Helpers tiempo (tu lógica + algunos añadidos mínimos) ---
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
      time(r) <- d                # <<<< antes: setZ(...)
      names(r) <- format(d, "%Y-%m-%d")
    }
    r
  }
  
  
  # --- Definición de ventanas GS ---
  gs_window <- function(region, y_start) {
    if (region == "ARG") {
      start <- as.Date(sprintf("%d-09-15", y_start))
    } else if (region == "SDA") {
      start <- as.Date(sprintf("%d-10-15", y_start))
    } else {
      stop("Región no soportada: ", region)
    }
    end <- start + 124  # 125 días totales
    list(start = start, end = end)
  }
  
  # --- I/O discovery ---
  years_available <- function(root_subdir, region) {
    # root_subdir: e.g., "temps/agg" o "precs/acum"
    base <- file.path(dir_base, root_subdir, region)
    if (!dir_exists(base)) return(integer(0))
    ys <- dir_ls(base, type = "directory", recurse = FALSE)
    as.integer(basename(ys))
  }
  
  build_paths <- function(var_type, region, year) {
    # var_type: list con campos:
    #   subdir ("temps/agg" o "precs/acum")
    #   filename(year) -> nombre base del nc
    # returns full path
    file.path(dir_base, var_type$subdir, region, sprintf("%d", year), var_type$filename(year))
  }
  
  # --- Variables a procesar ---
  var_defs <- list(
    tmin = list(
      subdir   = "temps/agg",
      filename = function(y) sprintf("2m_temperature_min_%d.nc", y),
      outname  = function(y0, y1) sprintf("2m_temperature_min_gs_%d_%d.nc", y0, y1)
    ),
    tmax = list(
      subdir   = "temps/agg",
      filename = function(y) sprintf("2m_temperature_max_%d.nc", y),
      outname  = function(y0, y1) sprintf("2m_temperature_max_gs_%d_%d.nc", y0, y1)
    ),
    tp = list(
      subdir   = "precs/acum",
      filename = function(y) sprintf("total_precipitation_acum_%d.nc", y),
      outname  = function(y0, y1) sprintf("total_precipitation_acum_gs_%d_%d.nc", y0, y1)
    )
  )
  
  # --- Escritura NetCDF ---
  write_nc <- function(r, out_path) {
    dir_create(path_dir(out_path), recurse = TRUE)
    writeRaster(r, out_path, overwrite = TRUE, filetype = "CDF")
    message(">> Escrito: ", out_path)
  }
  
  write_out <- function(r, out_path_nc) {
    dir.create(dirname(out_path_nc), recursive = TRUE, showWarnings = FALSE)
    
    # 1) Intentar escribir a NetCDF
    ok <- try({
      terra::writeRaster(r, out_path_nc, overwrite = TRUE, filetype = "CDF")
    }, silent = TRUE)
    
    if (!inherits(ok, "try-error")) {
      message(">> Escrito (NetCDF): ", out_path_nc)
      return(invisible(out_path_nc))
    }
    
    # 2) GeoTIFF (multi-banda)
    out_path_tif <- sub("\\.nc$", ".tif", out_path_nc)
    ok2 <- try({
      terra::writeRaster(r, out_path_tif, overwrite = TRUE, filetype = "GTiff")
    }, silent = TRUE)
    
    if (inherits(ok2, "try-error")) {
      stop("No pude escribir ni como NetCDF ni como GeoTIFF. Error original: ",
           as.character(ok))
    } else {
      message(">> Driver NetCDF no disponible. Escrito (GeoTIFF): ", out_path_tif)
      return(invisible(out_path_tif))
    }
  }
  
  
  # --- Proceso por región/variable ---
  process_region_var <- function(region, var_key) {
    v <- var_defs[[var_key]]
    if (is.null(v)) stop("var_key desconocido: ", var_key)
    
    ys <- sort(years_available(v$subdir, region))
    if (length(ys) == 0) {
      message("No hay años para ", region, " / ", v$subdir)
      return(invisible(NULL))
    }
    
    for (y in ys) {
      # Necesitamos Y y Y+1
      pY  <- build_paths(v, region, y)
      pY1 <- build_paths(v, region, y + 1L)
      if (!file_exists(pY) || !file_exists(pY1)) {
        message("Saltando ", region, " ", var_key, " GS ", y, "-", y+1, ": falta ", ifelse(!file_exists(pY), basename(pY), basename(pY1)))
        next
      }
      
      # Carga y adjunta tiempo
      rY  <- attach_time(pY)
      rY1 <- attach_time(pY1)
      
      # Chequeo básico de grilla (misma resolución/extensión/CRS)
      if (!all.equal(ext(rY), ext(rY1)) || !all.equal(res(rY), res(rY1)) || crs(rY) != crs(rY1)) {
        # Y+1 a grilla de Y
        rY1 <- resample(rY1, rY, method = "bilinear")
      }
      
      # Bind temporal
      r_all <- c(rY, rY1)  # concatena por capas (tiempo)
      d_all <- time(r_all)
      
      # Ventana GS
      w <- gs_window(region, y)
      sel <- d_all >= w$start & d_all <= w$end
      if (!any(sel)) {
        message("Sin capas para ventana GS en ", region, " ", var_key, " ", y, "-", y+1)
        next
      }
      
      r_gs <- r_all[[sel]]
      time(r_gs) <- d_all[sel]
      names(r_gs) <- format(d_all[sel], "%Y-%m-%d")
      
      # Salida: espejo de estructura pero bajo output_gs
      #   p.ej.: output_gs/temps/agg/ARG/2008/2m_temperature_min_gs_2008_2009.nc
      out_dir  <- file.path(dir_output, v$subdir, region, sprintf("%d", y))
      out_file <- v$outname(y, y + 1L)
      out_path <- file.path(out_dir, out_file)
      
      write_out(r_gs, out_path)
    }
  }
  
  # loop para ARG y SDA
  regions <- c("ARG", "SDA")
  vars    <- c("tmin", "tmax", "tp")
  
  for (reg in regions) {
    for (vk in vars) {
      message("Procesando ", reg, " / ", vk, " ...")
      process_region_var(reg, vk)
    }
  }
  
  message("done, saved in: ", dir_output)
  
}
