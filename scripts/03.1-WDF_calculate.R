#' @title
#' Wet Day Frequency of  gauge data
#'
#' @description
#' this script calculates the WDF of gauge data
#'
#' @author Fernando Prudencio
#' 
#' @data
#' 

rm(list = ls())

#' Installing packages
pkg <- c(
  "sf", "tidyverse", "spatialEco", "reshape2", "stringr"
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

#' Load packages
library(tidyverse)
library(sf)
library(spatialEco)
library(reshape2)
library(stringr)

#' Change to English language
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#' Loading functions
source('scripts/functions.R')

#' Loading color palette
source("scripts/color_palette.R")

#' LOAD CONSTANTS
k.prdo <- c('1980-01-01', '2013-12-31')
k.date.day <- seq(as.Date('1928-11-02'), as.Date('2015-10-31'), by = 'day')
k.date.year <- seq(as.Date('1980-01-01'), as.Date('2013-12-31'), by = 'year')
k.date.plt <- seq(as.Date('2020-03-01'), as.Date('2020-12-31'), by = 'day')
k.years <- c(1980:2013)
k.dry.yr <- c(2005, 2010)
k.regions <- c(8)
k.threshold <- 0.1

#' READ VECTORIAL DATA
#'   read cluster region
lyrs.cls <- rgdal::ogrListLayers(
  "data/vector/cluster_region/cluster_region.gpkg"
)

sf.region <- st_read(
  dsn = "data/vector/cluster_region/cluster_region.gpkg",
  layer = lyrs.cls[1], quiet = T, as_tibble = T
) %>%
  group_by(gridcode) %>%
  summarise(nfeature = length(gridcode)) %>%
  dplyr::filter(gridcode %in% k.regions)

#'   read raingauge location
lyrs.gaug <- rgdal::ogrListLayers(
  "data/vector/station/senamhi_weather_stations.gpkg"
)

sf.gauge.lct <- st_read(
  dsn = "data/vector/station/senamhi_weather_stations.gpkg",
  layer = lyrs.gaug[2], quiet = T, as_tibble = T
)

#' READ TABLE DATA
tbl.gauge.lct <-
  read.csv("data/table/BD_with_filterQA/LISTA_ESTACIONES.csv",
           header = T, sep = ";"
  ) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  as_tibble()

tbl.gauge.data <- read.csv("data/table/BD_with_filterQA/BD_Pp.csv",
                           header = T, sep = ";"
) %>%
  mutate(date = k.date.day) %>%
  as_tibble() %>%
  mutate_all(~na_if(., -99.9)) %>%
  filter(date >= k.prdo[1] & date <= k.prdo[2])

#' SELECT RAINGAUGE INTO REGION CLUSTER
sf.gauge.rg <- sf.gauge.lct %>%
  point.in.poly(sf.region, sp = T) %>%
  st_as_sf() %>%
  drop_na() %>%
  mutate(cod = as.character(CODIGO))


#' CALCULATE AMOUNT OF NODATA VALUES FOR EACH RAINGAUGE
miss.val.amoun <- tbl.gauge.data %>%
  dplyr::select(sf.gauge.rg$cod) %>%
  is.na() %>%
  colSums()

#' DATAFRAME WITH A LIST OF RAINGAUGE WITH LESS THAN 10% OF MISSING VALUES
df.miss.val <- tibble(
  id = c(1:length(miss.val.amoun)),
  miss.per = miss.val.amoun * 100 / nrow(tbl.gauge.data),
  cod = sf.gauge.rg$cod
) %>%
  filter(miss.per < 10)

#' DATAFRAME WITH MEAN RAINGAUGE DATA BY CLUSTER REGION WITH MORE THAN 10% OF
#'   MISSING VALUES. FURTHER, WITHOUT JANUARY AND FEBRUARY
df.rgn.data <- tibble(
  date = tbl.gauge.data$date,
  pp.mean = tbl.gauge.data %>%
    dplyr::select(df.miss.val$cod) %>%
    apply(1, FUN = function(x) mean(x, na.rm = T))
) %>%
  mutate(wdf = if_else(pp.mean > k.threshold, 1, 0)) %>%
  filter(!(substr(date, 6, 7) %in% c("01", "02")))

#' DATAFRAME WITH AVERAGE ACCUMULATED WDF BY CLUSTER REGION
for (i in k.years) {
  if (i == k.years[1]) {
    df.wdf.ac <- df.rgn.data %>%
      filter(substr(date, 1, 4) == i) %>%
      mutate(wdf.ac = cumsum(wdf)) %>%
      dplyr::select(wdf.ac) %>%
      as_tibble()
    names(df.wdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  } else {
    df.wdf.ac <- df.wdf.ac %>%
      cbind(
        df.rgn.data %>%
          filter(substr(date, 1, 4) == i) %>%
          mutate(wdf.ac = cumsum(wdf)) %>%
          dplyr::select(wdf.ac)
      ) %>%
      as_tibble()
    names(df.wdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  }
}

#' DATAFRAME WITH AVERAGE ACCUMULATED DDF BY CLUSTER REGION, DURING DRY YEARS
#'   AND NORMAL YEARS
df.wdf.ac.norm <- df.wdf.ac %>%
  dplyr::select(sprintf("yr.%s", k.years[!(k.years %in% k.dry.yr)]))

df.wdf.ac.dry <- df.wdf.ac %>%
  dplyr::select(sprintf("yr.%s", k.dry.yr)) %>%
  mutate(
    wdf.max = df.wdf.ac.norm %>% apply(1, max, na.rm = T),
    wdf.min = df.wdf.ac.norm %>% apply(1, min, na.rm = T),
    wdf.mean = df.wdf.ac.norm %>% apply(1, mean, na.rm = T),
    date = k.date.plt
  ) %>%
  as_tibble()

#' PLOT ACCUMULATED DDF BY YEAR
wdf.yr <- tibble(
  wdf = apply(df.wdf.ac, 2, max),
  date = k.date.year
) %>%
  mutate(
    ubic.dry.05 = ifelse(str_sub(date, 1, 4) == "2005", wdf, NA),
    ubic.dry.10 = ifelse(str_sub(date, 1, 4) == "2010", wdf, NA)
  )

blue <- rgb(62, 87, 173, maxColorValue = 255)

plt.wdf.yr <- ggplot(wdf.yr, aes(date, wdf)) +
  geom_hline(
    yintercept = wdf.yr$ubic.dry.05[!is.na(wdf.yr$ubic.dry.05)],
    linetype = "dashed", alpha = 1,
    color = "blue", size = 1
  ) +
  geom_hline(
    yintercept = wdf.yr$ubic.dry.10[!is.na(wdf.yr$ubic.dry.10)],
    linetype = "dashed", alpha = 1,
    color = "black", size = 1
  ) +
  geom_line(colour = "gray", size = 1.4) +
  geom_point(colour = "gray", size = 2.5, shape = 15) +
  geom_point(aes(date, ubic.dry.05), color = "blue", size = 3, shape = 15) +
  geom_point(aes(date, ubic.dry.10), color = "black", size = 3, shape = 15) +
  labs(y = "Wet Day Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text.x = element_text(size = 15, angle = 0),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17),
    panel.grid = element_line(size = 0.3, color = "gray", linetype = "dashed"),
    #panel.grid.minor = element_blank(),
    panel.border = element_rect(size = 1)
  ) +
  scale_x_date(date_labels = "%Y", breaks = "5 year", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(120, 300, 20),
    limits = c(121, 300),
    expand = c(0, 0)
  )

ggsave(
  plot = plt.wdf.yr,
  sprintf("exports/wdf_yr_region_n%s_thr_0.1_1980-2013.png", k.regions),
  width = 18, height = 9, units = "cm", dpi = 1000
)

#' BOXPLOT ACCUMULATED DDF

month.lbl <- tibble(month = sprintf("%.02d", 3:12), lbl = month.abb[3:12])

df.wdf.month <- df.wdf.ac %>%
  mutate(date = k.date.plt, month = str_sub(date, 6, 7)) %>%
  dplyr::select(-date) %>%
  melt(id.var = "month") %>%
  as_tibble() %>%
  group_by(month, variable) %>%
  summarise(wdf = max(value)) %>%
  mutate(
    variable = str_sub(variable, -4, -1),
    outlier = ifelse(is_outlier(wdf), variable, NA),
    outlier2 = ifelse(wdf == max(wdf), variable, NA),
    ubic.otlr = ifelse(wdf == max(wdf), wdf, NA),
    txt.dry.05 = ifelse(variable == "2005", variable, NA),
    txt.dry.10 = ifelse(variable == "2010", variable, NA),
    ubic.dry.05 = ifelse(variable == "2005", wdf, NA),
    ubic.dry.10 = ifelse(variable == "2010", wdf, NA)
  ) %>%
  ungroup() %>%
  left_join(month.lbl, by = "month")

names(df.wdf.month)[2] <- c("year")

boxplt.wdf <- ggplot(df.wdf.month, mapping = aes(month, wdf)) +
  geom_boxplot(
    alpha = 0, outlier.size = NULL, width = 0.5,
    fatten = 1.5, lwd = .8, color = "gray"
  ) +
  geom_jitter(
    shape = 16,
    size = 0.8, color = "gray",
    position = position_jitter(0.2)
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4, colour = "red",
    fill = "red"
  ) +
  scale_x_discrete(label = month.lbl$lbl) +
  scale_y_continuous(
    breaks = seq(0, 300, 20),
    limits = c(-5, 300),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = outlier2),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = T, color = "gray"
  ) +
  geom_point(
    aes(month, ubic.otlr), color = "gray", size = 1.5
  ) +
  geom_text(
    aes(label = txt.dry.05),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = F, color = "blue"
  ) +
  geom_text(
    aes(label = txt.dry.10),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = F, color = "black"
  ) +
  geom_point(
    aes(month, ubic.dry.05), color = "blue", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.10), color = "black", size = 1.5
  ) +
  labs(
    title = "Monthly WDF distribution", subtitle = "from 1980 to 2013",
    y = "Wet Day Frequency"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.ticks.length = unit(.15, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1)
  )

ggsave(
  plot = boxplt.wdf,
  sprintf("exports/wdf_month_region_n%s_thr_0.1_1980-2013.png", k.regions),
  width = 12, height = 16, units = "cm", dpi = 1000
)