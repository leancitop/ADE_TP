# TP – Variabilidad climática interanual ARG - SDA (QGIS)


[Presentación clase 3/9](https://docs.google.com/presentation/d/1VTRw9QUakMVLEsftMyfyLYX8-KAeYydUitM8XbEUIjA/edit?usp=sharing)

[CANVA PRESENTACION 10/9](https://www.canva.com/design/DAGyfZbsXho/vgBQDu5mQcKzk3rLsQrP2A/edit?utm_content=DAGyfZbsXho&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton)

Este repositorio contiene el **TP de Análisis de Datos Espaciales** enfocado en mapear la **estabilidad interanual** de las precipitaciones y temperaturas en Argentina y Sudáfrica para compararlas entre las distintas localidades.

## 📦 Estructura

```
.
├── data/
│   ├── raw/         # descargas originales (CHIRPS/WorldClim/SoilGrids, etc.)
│   ├── processed/   # datos derivados (agregados anuales/estacionales, CV)
│   └── external/    # shapes externos (regiones, límites)
├── qgis/            # proyecto .qgz y gpkg
├── scripts/
│   └── r/           # scripts en R
├── .gitignore
└── README.md
```
