#'@author  Fernando Prudencio
#'this script CALCULATES DRY DAY FREQUENCY 'DDF'
rm(list = ls())
pkg <- c('tidyverse', 'sf', 'spatialEco')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

#'LOADING LIBRARIES
library(tidyverse)
library(sf)
library(spatialEco)

#'LOADING PARAMETERS
prdo <- c('2000-01-01', '2013-12-31')
dryYR <- c(2005, 2010, 2016)
climReg <- c('Andes west slope', 'Andes east slope')

#'READING TABLE DATA
station <- read.csv('data/table/LISTA_ESTACIONES.csv', header = T, sep = ';') %>% mutate(CODIGO = as.character(CODIGO))
pp_data <- read.csv('data/table/BD_Pp.csv', header = T, sep = ';') %>%
              mutate(date = seq(as.Date('1928-11-02'), as.Date('2015-10-31'), by = 'day')) %>% as_tibble()
pp_data[pp_data == -99.9] <- NA

#'READING VECTORIAL DATA
region <- read_sf('data/shp/bastian_region/bastian_region.shp') %>% filter(Region %in% climReg)

#'SELECTING STATIONS INTO ANDES REGION
#station_sf <- st_as_sf(station, coords = c('LON', 'LAT'), crs = 4326) %>%
#                  mutate(into = st_within(geometry, region$geometry) %>% lapply(FUN = function(x) { length(x) == 1}))
station_sf <- st_as_sf(station, coords = c('LON', 'LAT'), crs = 4326) %>%
                    point.in.poly(region, sp = T) %>% st_as_sf() %>% filter(!is.na(Region))

#'CALCULATING THE PERCENTAGE OF NODATA VALUES FOR EACH STATION
MVper <- pp_data %>% filter(date >= prdo[1] & date <= prdo[2]) %>% dplyr::select(-date) %>%
          is.na() %>% colSums()

#MAKING DATAFRAME OF STATIONS INTO ANDES REGION AND LESS THAN OR EQUAL TO 10% OF NODATA VALUES
df <- tibble(id = c(1:293), missVAL = MVper*100/nrow(pp_data), cod = names(pp_data)[-1]) %>%
        filter(missVAL < 10 & cod %in% station_sf$CODIGO)

#'
df2 <- pp_data %>% filter(date >= prdo[1] & date <= prdo[2]) %>% dplyr::select(date, df$cod)
df3 <- tibble(date = df2$date, meanPP = apply(df2 %>% dplyr::select(-date), 1, FUN = function(x) mean(x, na.rm = T)) ) %>% 
          mutate(ddf = if_else(meanPP < 0.1, 1, 0)) %>% filter(!( substr(date,6,7) %in% c('01','02') ))# & !( substr(date,1,4) %in% dryYR )

for (i in 2000:2013) {
  if (i == 2000) {
    df_end = df3 %>% filter(substr(date,1,4) == i) %>% mutate(ddfAC = cumsum(ddf)) %>% dplyr::select(ddfAC)
  }else{
    df_end = cbind(df_end, df3 %>% filter(substr(date,1,4) == i) %>% mutate(ddfAC = cumsum(ddf)) %>% dplyr::select(ddfAC))
  }
}

names(df_end) <- paste('yr', c(2000:2013), sep = '')
x <- df_end %>% dplyr::select( paste('yr', c(2000:2004, 2006:2009, 2011:2013), sep = '') )

ymax <- x %>% apply(1, max, na.rm = T)
ymin <- x %>% apply(1, min, na.rm = T)

z <- df_end %>%
        dplyr::select( paste('yr', c(2005,2010), sep = '') ) %>%
          mutate(ymax, ymin, date = seq(as.Date('2000-03-01'), as.Date('2000-12-31'), by = 'day')) %>% as_tibble()

X <- ggplot(z, aes(x = date, y = yr2005)) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.3) +
        geom_line(colour = 'black', size = 1) +
        theme_bw() + ylab(label = 'Dry Day Frequency') +  xlab(label = '') +
        ggtitle('Temporal evolution of Dry Day Frequency',
                subtitle = paste('from 2000 to 2013', sep = '')) + 
        theme(plot.title    = element_text(size = 18, hjust = 0),
              plot.subtitle = element_text(size = 15, hjust = 0),
              axis.text.x   = element_text(size = 12, angle = 0, hjust = -0.5),
              axis.text.y   = element_text(size = 12),
              axis.title    = element_text(size = 15)) +
        scale_x_date(date_labels = "%b", breaks = '1 month',expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0,90,10), limits = c(0,90), expand = c(0,0)) +
        geom_line(aes(x = date, y = yr2010), colour = 'blue', size = 1) +
        annotate("rect", xmin=as.Date('2000-06-08'), xmax=as.Date('2000-08-20'), ymin = 0, ymax = 90, alpha = 0.1, fill = 'red') 
X
ggsave(plot = X, 'exports/ddf_Andes_Region_2000-2013.png', width = 14, height = 15, units = "cm", dpi = 1000)
