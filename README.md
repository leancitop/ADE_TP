# TP – Variabilidad climática interanual ARG - SDA (QGIS)

Este repositorio contiene el **TP de Análisis de Datos Espaciales** enfocado en mapear la **estabilidad interanual** de las precipitaciones y temperaturas en Argentina y Sudáfrica para compararlas entre las distintas localidades.

## 📦 Estructura

```
.
├── data/
│   ├── raw/         # descargas originales (CHIRPS/WorldClim/SoilGrids, etc.)
│   ├── processed/   # datos derivados (agregados anuales/estacionales, CV)
│   └── external/    # shapes externos (regiones, límites)
├── qgis/            # proyecto .qgz y estilos .qml
├── scripts/
│   └── r/           # scripts en R
├── notebooks/       # exploraciones opcionales
├── docs/            # propuesta, apuntes de metodología
├── reports/
│   └── figures/     # mapas exportados
├── .github/workflows/ # CI liviano (lint R)
├── .gitignore
├── .gitattributes   # Git LFS para rasters/pesados
└── README.md
```

## 🛠️ Setup rápido

1. **Clonar** este repo y crear un proyecto en **QGIS**.
2. **Activar Git LFS** (localmente y en GitHub):  
   ```bash
   git lfs install
   ```
3. **Instalar paquetes R** (si vas a correr los scripts de apoyo):
   ```r
   source("scripts/r/setup_packages.R")
   ```
4. Descargar insumos (ejemplos en `scripts/r/01_download_data.R`) y guardar en `data/raw/`.

> Tip: No subas rasters pesados al repo si no es necesario. Si hace falta compartirlos, usá LFS o un release.

## 🧭 Flujo sugerido

1. **Descarga** y **agregación temporal** de precipitación (1981–2023).
2. **Cálculo del CV** (anual y/o estacional) por celda.
3. **Zonal statistics?** para polígonos de interés (especificando por region de cultivo).
4. **Comparativa** ARG vs SDA + breve reporte.

## 🧑‍🤝‍🧑 Colaboración

- Estrategia **GitHub Flow**: ramas feature → PR → revisión → merge a `main`.
- Nombrado de ramas: `feat/`, `fix/`, `docs/`, `data/`.
- Issues con etiquetas: `data`, `qgis`, `analysis`, `infra`.