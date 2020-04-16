#' @title
#' Rainfall seasonal behavior by elevation interval and cluster region
#'
#' @description
#' this script plots the seasonal behavior of the estimated rainfall
#'   (remote sensing) by elevation interval from homogeneous regions (clusters)
#'
#' @author Fernando Prudencio
#'
#' @data
#' 'k.regions', number (location) of cluster region
#' 'region', homogeneous regions built from a cluster analysis
#' 'pp', gridded rainfall climatological data built by Aybar et al. (2019)
#' 'dem', SRTM digital elevation model resampled to 10km

rm(list = ls())

#' Installing packages
pkg <- c(
  "sf", "tidyverse", "ncdf4", "raster", "lattice", "stringr",
  "arules", "reshape2", "RColorBrewer", "magrittr", "latticeExtra"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' Loading packages
library(sf)
library(tidyverse)
library(arules)
library(stringr)
library(ncdf4)
library(raster)
library(RColorBrewer)
library(reshape2)
library(magrittr)
library(lattice)
library(latticeExtra)

#' Changing language
Sys.setlocale(category = "LC_ALL", locale = "english")

#' Defining constants
k.regions <- c(8)

#' Reading vectorial data
lyrs <- rgdal::ogrListLayers(
  "data/vector/cluster_region/cluster_region.gpkg"
)

region <- st_read(
  dsn = "data/vector/cluster_region/cluster_region.gpkg",
  layer = lyrs[1], quiet = T, as_tibble = T
) %>%
  group_by(gridcode) %>%
  summarise(nfeature = length(gridcode)) %>%
  dplyr::filter(gridcode %in% k.regions)

#' Reading raster data
pp <- brick("data/raster/pp/pisco/clim/PISCOp_v2.1.1_clim_1981-2017.nc")
dem <- raster("data/raster/dem/SRTM_10km_resampled_to_PISCO.nc")
stack.data <- stack(pp, dem)

#' Building dataframe to plot heatmap
df <- stack.data %>%
  crop(region) %>%
  mask(region) %>%
  getValues() %>%
  as_tibble() %>%
  drop_na()

names(df) <- c(sprintf("%.*s", 3, month.name), "elev")

df %<>%
  mutate(category = cut(elev, breaks = c(seq(1000, 4800, 200), 5200))) %>%
  group_by(category) %>%
  summarise_each(funs(mean)) %>%
  dplyr::filter(elev >= 1500) %>%
  mutate(
    minelev = seq(1400, 4800, 200),
    maxelev = seq(1600, 5000, 200),
    inter = sprintf("%s %s %s %s %s", "(", minelev, " - ", maxelev, "]")
  ) %>%
  dplyr::select(inter, sprintf("%.*s", 3, month.name)) %>%
  melt(id.vars = "inter") %>%
  mutate(inter = as.factor(inter)) %>%
  as_tibble()

names(df)[2:3] <- c("mes", "pp")

#' Defining the color palette
hm.palette <- colorRampPalette(brewer.pal(9, "RdBu"), space = "Lab")

#' Saving plot
png("exports/rainfall_by_elevation_heatmap_cluster8.png",
  width = 17, height = 15, units = "cm", res = 500
)

#' Ploting heatmap
levelplot(pp ~ mes * inter,
  data = df,
  main = list("Seasonal Rainfall by Elevation",
    cex = 1.5,
    font = 1
  ),
  xlab = "",
  ylab = list("Elevation interval\n[mm]",
    cex = 1.2,
    font = 1
  ),
  col.regions = hm.palette,
  colorkey = list(
    at = seq(0, 320, 10),
    # space = "bottom", #location of legend
    labels = list(at = seq(0, 320, 60))
  ),
  # panel = panel.levelplot.points, #show points into grid
  cex = 1.2,
  aspect = "iso",
  scale = list(
    x = list(rot = 45, cex = 0.8, font = 1),
    y = list(cex = 0.8, font = 1)
  ),
  # contour = T, #show contour of data interpolated
  region = T,
  border = "black",
  par.settings = list(
    panel.background = list(col = "white"),
    axis.line = list(lwd = 1)
  )
) #+
# layer_(panel.2dsmoother(..., n = 200)) #smooth or interpolate data

#' Closing the saved of plot
dev.off()