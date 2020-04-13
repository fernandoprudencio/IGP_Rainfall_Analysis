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
pkg <- c("sf", "tidyverse", "ncdf4", "raster", "ggplot2", "stringr")

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
library(stringr)
library(ncdf4)
library(raster)
library(ggplot2)

#' Changing language
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#' Defining constants
k.regions <- c(6)

#' Creating PISCO's date
date <- seq(as.Date('1981-01-01'), as.Date('2018-02-01'), by = 'month')

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
pp  <- brick("data/raster/pp/pisco/PISCOp_v2.1.1.nc")
dem <- raster("data/raster/dem/SRTM_10km_resampled_to_PISCO.nc")

xtr.vls <- raster::extract(pp, region, fun = mean, na.rm = T) %>% as.vector()

df <- tibble(date, pp = xtr.vls) %>%
  dplyr::filter(str_sub(date, 1, 4) < 2018) %>%
  mutate(month = str_sub(date, 6, 7)) %>%
  group_by(month) %>%
  summarise(
    ppmean = mean(pp, na.rm = T),
    ppmax = max(pp, na.rm = T),
    ppmin = min(pp, na.rm = T)
  ) %>%
  dplyr::select(-month) %>%
  mutate(
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")
  ) %>%
  cbind(
    tibble(date, pp2000 = xtr.vls) %>%
      dplyr::filter(str_sub(date, 1, 4) == 2000) %>%
      dplyr::select(pp2000),
    tibble(date, pp2005 = xtr.vls) %>%
      dplyr::filter(str_sub(date, 1, 4) == 2005) %>%
      dplyr::select(pp2005),
    tibble(date, pp2010 = xtr.vls) %>%
      dplyr::filter(str_sub(date, 1, 4) == 2010) %>%
      dplyr::select(pp2010),
    tibble(date, pp2016 = xtr.vls) %>%
      dplyr::filter(str_sub(date, 1, 4) == 2016) %>%
      dplyr::select(pp2016)
  )

ssnl.plt <- ggplot(df, aes(date, ppmean)) +
  geom_line(colour = "black", size = .8) +
  theme_bw() +
  ylab(label = "[mm]") +
  xlab(label = "") +
  ggtitle("Seasonal Rainfall", subtitle = "from 1981 to 2017") +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 20)
  ) +
  scale_x_date(
    date_labels = "%b",
    breaks = "1 month"
  ) +
  scale_y_continuous(
    breaks = seq(0, 300, 25), limits = c(0, 300)
  ) +
  geom_line(aes(date, ppmax),
    colour = "red", linetype = "dashed", size = .8
  ) +
  geom_line(aes(date, ppmin),
    colour = "red", linetype = "dashed", size = .8
  ) +
  geom_line(aes(date, pp2000),
    colour = rgb(233, 228, 91, maxColorValue = 255), size = .8
  ) +
  geom_line(aes(date, pp2005),
    colour = rgb(160, 114, 14, maxColorValue = 255), size = .8
  ) +
  geom_line(aes(date, pp2010),
    colour = "snow3", size = .8
  ) +
  geom_line(aes(date, pp2016),
    colour = "springgreen3", size = .8
  ) +
  geom_point(aes(date, pp2000),
    colour = rgb(233, 228, 91, maxColorValue = 255), size = 2
  ) +
  geom_point(aes(date, pp2005),
    colour = rgb(160, 114, 14, maxColorValue = 255), size = 2
  ) +
  geom_point(aes(date, pp2010), colour = "snow3", size = 2) +
  geom_point(aes(date, pp2016), colour = "springgreen3", size = 2)

ggsave(ssnl.plt,
  filename = "exports/Seasonal_Rainfall_cluster8.png",
  width = 15, height = 15, units = "cm", dpi = 500
)