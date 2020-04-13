#' @title
#' Rainfall seasonal behavior by cluster region
#'
#' @description
#' this script plots the seasonal behavior of the estimated rainfall
#'   (remote sensing) from homogeneous regions (clusters)
#'
#' @author Fernando Prudencio
#'
#' @data
#' 'region', homogeneous regions built from a cluster analysis
#' 'pp', gridded rainfall climatological data built by Aybar et al. (2019)
#' 'dem', SRTM digital elevation model resampled to 10km

rm(list = ls())

#' Installing packages
pkg <- c(
  "sf", "tidyverse", "ncdf4", "raster", "ggplot2", "stringr",
  "arules", "reshape2", "RColorBrewer", "magrittr"
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
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(magrittr)

#' Changing language
Sys.setlocale(category = "LC_ALL", locale = "english")

#' Defining constants
k.regions <- c(6)

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
  as_tibble()

names(df)[2:3] <- c("mes", "pp")

hm.palette <- colorRampPalette(brewer.pal(9, "YlGnBu"), space = "Lab")

heatmap.plt <- ggplot(df, aes(mes, inter)) +
  geom_raster(aes(fill = pp)) +
  scale_fill_gradientn(
    colours = hm.palette(100),
    limits = c(0, 320)
  ) +
  theme_bw() +
  ggtitle(
    "Seasonal Rainfall by Elevation",
    subtitle = "from 1981 to 2017\nCLUSTER 8"
  ) +
  labs(x = "", y = "Elevation interval [m]\n", fill = "[mm]") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
    plot.title    = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    legend.title  = element_text(size = 12),
    axis.text.x   = element_text(size = 12),
    axis.text.y   = element_text(size = 12),
    axis.title    = element_text(size = 20)
  )
heatmap.plt
ggsave(heatmap.plt,
  filename = "exports/rainfall_by_elevation_heatmap_cluster8.png",
  width = 17, height = 15, units = "cm", dpi = 500
)