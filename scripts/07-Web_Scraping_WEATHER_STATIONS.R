#' @title
#' Web Scraping of Senamhi's rainfall data
#'
#' @description
#' this script obtains information of rainfall data of weather stations
#'
#' @author Fernando Prudencio
#' 
#' @data
#' 

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
library(tidyverse)
library(stringr)
library(rvest)

#' Reading file list
list.files("data/table/BD_southamerican//data_realtime_PERU/meteo/",
  pattern = ".csv"
)

#' Imput URL of Senamhi's web site
senamhi <-
  "https://www.senamhi.gob.pe/mapas/mapa-estaciones/_dat_esta_tipo.php?estaciones="

link <-
  list.files("data/table/BD_southamerican/data_realtime_PERU/meteo/",
    pattern = ".csv"
  ) %>% str_sub(3, -5)

for (i in 1:length(link)) {
  print(i)
  url <- sprintf(
    "%s%s", senamhi, bd[i]
  )

  data = read_html(url) %>%
    html_nodes("body") %>%
    html_nodes("tr")

  info = data[2] %>%
    html_nodes("td") %>%
    html_text()

  ubigeo = data[3] %>%
    html_nodes("td") %>%
    html_text()

  coord = data[4] %>%
    html_nodes("td") %>%
    html_text()

  est = str_split(info, ",") %>%
    sapply("[", 1) %>%
    str_sub(38, -2)

  type = str_split(info, ",") %>%
    sapply("[", 2) %>%
    str_sub(2, -14)

  cod  = bd[i] %>% as.character()
  dep  = ubigeo[2]
  prov = ubigeo[4]
  dist = ubigeo[6]

  lat.s = coord[2] %>%
    str_split("'") %>%
    sapply("[", 2) %>%
    as.numeric()
  lat.m = coord[2] %>%
    str_split("'") %>%
    sapply("[", 1) %>%
    str_split("°") %>%
    sapply("[", 2) %>%
    as.numeric()
  lat.d = coord[2] %>%
    str_split("°") %>%
    sapply("[", 1) %>%
    as.numeric()

  lon.s = coord[4] %>%
    str_split("'") %>%
    sapply("[", 2) %>%
    as.numeric()
  lon.m = coord[4] %>%
    str_split("'") %>%
    sapply("[", 1) %>%
    str_split("°") %>%
    sapply("[", 2) %>%
    as.numeric()
  lon.d = coord[4] %>%
    str_split("°") %>%
    sapply("[", 1) %>%
    as.numeric()

  lat = lat.d + lat.m / 60 + lat.s / 3600
  lon = lon.d + lon.m / 60 + lon.s / 3600
  alt = coord[6] %>% as.numeric()

  if (i == 1) {
    df = tibble(cod, est, type, dep, prov, dist, lat, lon, alt)
  } else {
    df = rbind(df, tibble(cod, est, type, dep, prov, dist, lat, lon, alt))
  }
}

#' Saving data
save(df, file = "data/table/BD_with_filterQA/LISTA_EST_RT.RData")
write.csv(df, "data/table/BD_with_filterQA/LISTA_EST_RT_v2.csv")