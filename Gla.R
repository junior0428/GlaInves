install.packages("ggplot2")
#versiones distintas
library(rlang)
library(mapedit)
library(raster)
library(mapview)
library(dplyr)
library(geojsonio)
library(rgee)
library(sp)
library(sf)
library(leaflet.extras2) # para el operador |
library(ggplot2)

# Direccionar carpeta
setwd("E:/HP I5 DORADA/DISCO C/TRABAJOS EXTERNOS/INVESTIGACION GLACIARES/INSUMO")

#Inicializar Earth Engine
ee_Initialize("junior")

# Definir area de estudio
ar <- st_read("Quill/Subcuenca_Quillcay.shp")
plot(ar)
ar_ee <- ar %>% sf_as_ee()
Map$centerObject(ar_ee)
Map$addLayer(ar_ee)


#Funcion de factor de escala para imagenes Surface Reflectance (SR) Landsat 5
applyScaleFactorsl5 <- function(image) {
  opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  thermalBand <- image$select('ST_B6')$multiply(0.00341802)$add(149.0)
  return(image$addBands(opticalBands, NULL, TRUE)$
           addBands(thermalBand, NULL, TRUE))
}

#Funcion de factor de escala para imagenes Surface Reflectance (SR) Landsat 8
applyScaleFactorsl89 <- function(image) {
  opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  thermalBand <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
  return(image$addBands(opticalBands, NULL, TRUE)$
           addBands(thermalBand, NULL, TRUE))
}
#Landsat 5 TM Collection 2 reflectancia superficial corregida atmosféricamente.

# Año 1985 
img1985 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1985-05-01', '1985-08-01')$
  filterBounds(draw_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)$
  map(applyScaleFactorsl5)$
  median()$
  clip(draw_ee)

# Año 1990 
img1991 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1990-01-01', '1991-01-01')$
  filterBounds(draw_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)$
  map(applyScaleFactorsl5)$
  median()$
  clip(draw_ee)

# Año 1998 
img1998 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1998-01-01', '1999-02-01')$
  filterBounds(draw_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)$
  map(applyScaleFactorsl5)$
  median()$
  clip(draw_ee)

# Parametros de visualizacion
visparal5 <- list(
  bands = c('SR_B5', 'SR_B4', 'SR_B3'),
  min = 0.1,
  max = 0.3
)
Map$addLayer(img1991, visparal5, 'img1991')|
  Map$addLayer(img1998, visparal5, 'img1998')
#Landsat 8 OLI/TIRS Collection 2 reflectancia superficial corregida atmosféricamente

# Año 2015 
img2016 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$
  filterDate('2017-01-01', '2018-01-01')$
  filterBounds(draw_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)$
  map(applyScaleFactorsl89)$
  median()$
  clip(draw_ee)

# Parametros de visualizacion
visparal89 <- list(
  bands = c('SR_B6', 'SR_B5', 'SR_B4'),
  min = 0.1,
  max = 0.3
)
Map$addLayer(img2016, visparal89, 'img2016')

# Landsat 9 OLI-2/TIRS-2 Colección 2 Reflectancia superficial corregida atmosféricamente.

# Año 2021 
img2021 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$
  filterDate('2021-01-01', '2022-01-01')$
  filterBounds(draw_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)$
  map(applyScaleFactorsl89)$
  median()$
  clip(draw_ee)
Map$addLayer(img2016, visparal89, 'img2016')|
  Map$addLayer(img2021, visparal89, 'img2021')

# Funcion para el calculo del NDSI sin agua
ndsi <- img2016$normalizedDifference(c("SR_B3", "SR_B6"))

# Calculating NDSInw
NDSInw <- img2016$expression(
  "(NIR - SWIR1 - 0.05) / (NIR + SWIR1)", list(
    "NIR" = img2016$select("SR_B5"),
    "SWIR1" = img2016$select("SR_B6")
  )
)
Map$addLayer(ndsi, list(palette=c("blue", "white"), min=0, max=1))|
Map$addLayer(NDSInw, list(palette=c("blue", "white"), min=0, max=1))
funNDSI <- function(variables) {
  
}
