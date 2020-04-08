#' @title
#' Analysis cluster of rainfall data
#'
#' @description
#' this script analyzes the seasonal behavior of the estimated rainfall
#'  (remote sensing) and groups pixels in homogeneous regions
#'
#' @author Fernando Prudencio
#' 
#' @data
#' 'anp', area naturales protegidas
#' 'fires', registro nacional de incendios 2000-2019 

rm(list = ls())

#' Installing packages
pkg <- c("sf", "tidyverse", "ncdf4", "raster", "cluster", "fpc", "factoextra")

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
library(ncdf4)
library(raster)
library(cluster)
library(fpc)
library(factoextra)

#' Reading vectorial data
lyrs <- rgdal::ogrListLayers(
  "data/vector/bastian_region/climatic_regions.gpkg"
)

region <- st_read(
  dsn = "data/vector/bastian_region/climatic_regions.gpkg",
  layer = lyrs[1], quiet = T, as_tibble = T
) %>%
  dplyr::filter(Region %in% c(
    "Pacific coast", "Andes west slope",
    "Andes east slope"
  ))

#' Reading raster data
pp  <- brick("data/raster/pp/pisco/clim/PISCOp_v2.1.1_clim_1981-2017.nc")
dem <- raster("data/raster/dem/SRTM_10km_resampled_to_PISCO.nc")
region <-
  raster("data/raster/cluster_k2_dist_EUCLIDEAN_part2_PERU_without_PCA.tif")
region[region != 2] <- NA
plot(region)

#' Stacking rainfall climatology and elevation
stck.data <- stack(pp, dem) %>% mask(region)

#' Building table before to the cluster analysis
df <- coordinates(stck.data) %>%
  cbind(getValues(stck.data)) %>%
  as_tibble() %>%
  drop_na()

names(df) <- c("lon", "lat", sprintf("%.*s", 3, month.name), "elev")

#' Partitioning Around Medoids (PAM)
#'   Calculating the number of optimal cluster
factoextra::fviz_nbclust(stzn.df, pam, method = "silhouette")
k.nbclust <- 8

#'   Standardization of the data
stzn.df <- scale(df) %>% as_tibble()
#'
#' ===========================================
#' ==================== Or ===================
#' ===========================================
#'
#'   Principal Coomponent Analysis
stzn.df <- prcomp(df, scale. = T, center = T)
stzn.df <- stzn.df$x[, 1:15] %>% as_tibble()

#'   Calculating distance matrix
dist.df <- cluster::daisy(stzn.df, metric = "euclidean")
#euclidean gower manhattan

#'   Data grouping
clus.df <- pam(dist.df, k.nbclust, diss = T)
#mdos <- clus.df$medoids
#clus.df <- kmeans(dist.df, k.nbclust, iter.max = 5)

#' Building the results to gridded data
xy.coord <- coordinates(stck.data) %>%
  as.data.frame() %>%
  rename("lon" = "x", "lat" = "y")

grid.df <- xy.coord %>%
  left_join(
    cbind(df %>% dplyr::select(lon, lat),
      clus = clus.df$clustering
    ),
    by = c("lon", "lat")
  ) %>%
  as_tibble()

names(grid.df) = c("x", "y", "z")

data.crs  <- crs(stck.data)
data.res  <- res(stck.data)
data.grid <- rasterFromXYZ(grid.df, data.res, data.crs, digits = 0)

writeRaster(data.grid,
  "data/raster/cluster_k8_dist_EUCLIDEAN_all_PERU_with_PCA_15cpt.tif",
  overwrite = T
)# Pacific_and_westANDES_eastANDES