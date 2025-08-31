# Instala paquetes necesarios
pkgs <- c(
  "data.table",
  "terra",        # lectura/escritura rasters
  "exactextractr",# zonal stats rápido
  "sf",           # vectores
  "ggplot2",
  "lintr"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos='https://cloud.r-project.org')
message("Paquetes listos")