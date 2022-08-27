install.packages("googledrive")
#versiones distintas
library(googledrive)
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
setwd("E:/HP I5 DORADA/DISCO C/TRABAJOS EXTERNOS/INVESTIGACION GLACIARES")

#Inicializar Earth Engine
ee_Initialize("junior", drive = T)

# Definir area de estudio
ar <- st_read("INSUMO/Quill/Subcuenca_Quillcay.shp")
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

# Año 1986 
img1986 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1986-05-01', '1986-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl5)$
  median()$
  clip(ar_ee)

# Año 1994 
img1994 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1994-05-01', '1994-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl5)$
  median()$
  clip(ar_ee)

# Año 1999 
img1999 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('1999-05-01', '1999-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl5)$
  median()$
  clip(ar_ee)

# Año 2007 
img2007 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$
  filterDate('2007-05-01', '2007-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl5)$
  median()$
  clip(ar_ee)

# Parametros de visualizacion
visparal5 <- list(
  bands = c('SR_B5', 'SR_B4', 'SR_B3'),
  min = 0.1,
  max = 0.3
)

Map$addLayer(img1986, visparal5, 'img1986')|
Map$addLayer(img1994, visparal5, 'img1994')

Map$addLayer(img1994, visparal5, 'img1994')|
Map$addLayer(img1999, visparal5, 'img1999')

Map$addLayer(img1999, visparal5, 'img1999')|
Map$addLayer(img2007, visparal5, 'img2007')


#Landsat 8 OLI/TIRS Collection 2 reflectancia superficial corregida atmosféricamente

# Año 2014 
img2014 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$
  filterDate('2014-05-01', '2014-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl89)$
  median()$
  clip(ar_ee)


# Parametros de visualizacion
visparal89 <- list(
  bands = c('SR_B6', 'SR_B5', 'SR_B4'),
  min = 0.1,
  max = 0.3
)
Map$addLayer(img2014, visparal89, 'img2014')

# Landsat 9 OLI-2/TIRS-2 Colección 2 Reflectancia superficial corregida atmosféricamente.

# Año 2021 
img2021 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$
  filterDate('2021-05-01', '2021-09-01')$
  filterBounds(ar_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 15)$
  map(applyScaleFactorsl89)$
  median()$
  clip(ar_ee)

  Map$addLayer(img2021, visparal89, 'img2021')

# function for calculating snow area

funsnow <- function(img, green, nir, swir1) {
  # Calculating Water Index not snow
  ndwins <- img$expression(
    "(GREEN-2*NIR)/(GREEN + NIR)", list(
      "GREEN" = img$select(green), 
      "NIR" = img$select(nir)
    )
  )
  ndwins_gt <- ndwins$gt(0)
  ndwins_mask <- ndwins_gt$updateMask(ndwins_gt)
  
  # Calculating Snow Index not water
  ndsinw <- img$expression(
    "(NIR - SWIR1 - 0.05)/(NIR + SWIR1)", list(
      "NIR" = img$select(nir),
      "SWIR1" = img$select(swir1)
    )
  )
  ndsinw_gt <- ndsinw$gt(0.4)
  ndsinw_mask <- ndsinw_gt$updateMask(ndsinw_gt)
  
  # Remove water areas of NDSInw 
  snow <- ndsinw_mask$updateMask(ndwins_mask$unmask()$Not())$selfMask()
  
  return(snow)
}

snow_1986 <- funsnow(img = img1986, green = "SR_B2", nir = "SR_B4", swir1 = "SR_B5")$rename("snow")
snow_1994 <- funsnow(img = img1994, green = "SR_B2", nir = "SR_B4", swir1 = "SR_B5")$rename("snow")
snow_1999 <- funsnow(img = img1999, green = "SR_B2", nir = "SR_B4", swir1 = "SR_B5")$rename("snow")
snow_2007 <- funsnow(img = img2007, green = "SR_B2", nir = "SR_B4", swir1 = "SR_B5")$rename("snow")
snow_2014 <- funsnow(img = img2014, green = "SR_B3", nir = "SR_B5", swir1 = "SR_B6")$rename("snow")
snow_2021 <- funsnow(img = img2021, green = "SR_B3", nir = "SR_B5", swir1 = "SR_B6")$rename("snow")
Map$addLayer(snow_1986, list(palette="blue"), "snow_1986")|
Map$addLayer(snow_2021, list(palette="red"), "snow_2021")


#functions to Calculating area km2
funarea <- function(imgsnow) {
  #Multiplicacion matricial a la clasificacion
  areaimage <- imgsnow$multiply(ee$Image$pixelArea())
  
  #sumamos todos los valores de la región 
  area <- areaimage$reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = ar_ee$geometry(),
    scale = 30,
    maxPixels = 1e9
  )
  
  #Obtencion de las area de cambio en Km2
  clasAreaKm2 <- ee$Number(area$get("snow"))$divide(1e6)
  
  return(ee$Number$getInfo(clasAreaKm2))
}

funarea(snow_1986)
funarea(snow_1994)
funarea(snow_1999)
funarea(snow_2007)
funarea(snow_2014)
funarea(snow_2021)

areasnow <- c(41.47, 38.55, 35.16, 32.89, 31.39, 29.93)

#Reducer raster data to vector and  Earth engine to Local

funlocal <- function(ras, dic, nom) {
  # Reduce to vector from raster date
  vect <- ras$reduceToVectors(
    reducer = ee$Reducer$countEvery(),
    geometry = ar_ee, 
    scale = 30,
    maxPixels = 1e12
  )
  
  # Vector data from Earth Engine to local
  loc <- ee_as_sf(x = vect, dsn = dic, via = "drive", container = nom)
  return(loc)
}

loc_1986 <- funlocal(snow_1986, "RESULTADOS/snow_1986.shp", "snow_1986")
loc_1994 <- funlocal(snow_1994, "RESULTADOS/snow_1994.shp", "snow_1994")
loc_1999 <- funlocal(snow_1999, "RESULTADOS/snow_1999.shp", "snow_1999")
loc_2007 <- funlocal(snow_2007, "RESULTADOS/snow_2007.shp", "snow_2007")
loc_2014 <- funlocal(snow_2014, "RESULTADOS/snow_2014.shp", "snow_2014")
loc_2021 <- funlocal(snow_2021, "RESULTADOS/snow_2021.shp", "snow_2021")

# Function to mapping with ggplot2 
funggplot <- function(loc, Área, titl ) {
  ggpl <- ggplot()+
    geom_sf(data = loc, aes(col = Área,  values = "blue"),
            pch = 21,    
            fill = "#00FFFF", # Color del borde
            cex = 0)+
    theme_bw()+
    labs(x='Logitud', y='Latitud', title = titl, subtitle = "Microcuenca de Quilcay", fontface = "bold")+
    scale_fill_discrete(name = "Area")+
    theme(plot.title = element_text(size=15))+
    geom_sf(data = ar, fill = 'transparent')+
    scale_color_manual(values="blue")
  
  return(ggpl)
}
funggplot(loc = loc_1986, Área = "41.47 Km2", titl = "Cobertura de nieve 1986")
funggplot(loc = loc_1994, Área = "38.55 Km2", titl = "Cobertura de nieve 1994")
funggplot(loc = loc_1999, Área = "35.16 Km2", titl = "Cobertura de nieve 1999")
funggplot(loc = loc_2007, Área = "32.89 Km2", titl = "Cobertura de nieve 2007")
funggplot(loc = loc_2014, Área = "31.39 Km2", titl = "Cobertura de nieve 2014")
funggplot(loc = loc_2021, Área = "29.93 Km2", titl = "Cobertura de nieve 2021")
