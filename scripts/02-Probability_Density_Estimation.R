#'@author  Fernando Prudencio
rm(list = ls())
library(sf)
library(dplyr)
library(raster)
library(reshape)
library(MASS)# función kde2D

fires <- read_sf('F:/4-temp/04-IGP_pp/02-Shp//EST_UBIC.shp')
peru  <- read_sf('F:/02-MasterGIS/Theme_n2/01-DataSet/02-shp/Peru.shp')

#------ Obteniendo coordenadas ------#
lon <- st_coordinates(fires)[,1]
lat <- st_coordinates(fires)[,2]

kde <- kde2d(lon, lat, n = c(267, 400), h = .5, lims = c(-82, -68, -19, 1))
image(kde)
plot(peru, add=T, col=NA)

#------ Convirtiendo el kde a raster ------#

kde_raster <- kde %>% raster() %>% mask(peru)
kde_raster[kde_raster < 0.02] <- NA
kde_raster[kde_raster >= 0.02] <- 1

writeRaster(kde_raster, 'F:/4-temp/04-IGP_pp/01-raster/Est_KDE_5km_EMD_h0.1.tif', overwrite=T)

#------ Convertiendo raster a polígono ------#
kde_sf <- rasterToPolygons(kde_raster, dissolve = T) %>% disaggregate() %>% st_as_sf() %>%
                                                    mutate(id = 1:15, area = st_area(d)/1000000) %>% 
                                                    filter(id == 15)

write_sf(kde_sf, 'F:/4-temp/04-IGP_pp/02-Shp/Region_KDE_5km_EMD_h0.1.shp')
