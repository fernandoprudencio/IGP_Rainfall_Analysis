#' @title
#' Analysis cluster of gridded rainfall data
#'
#' @description
#' this script analyzes the seasonal behavior of the estimated rainfall
#'   (remote sensing) and groups pixels in homogeneous regions
#'
#' @author Fernando Prudencio
#' 
#' @data
#' 'region', climat region built by Bastian et al. (2017)
#' 'pp', gridded rainfall climatological data built by Aybar et al. (2019)
#' 'dem', SRTM digital elevation model resampled to 10km

rm(list = ls())

#' Installing packages
pkg <- c(
  "sf", "tidyverse", "ncdf4", "raster", "cluster", "fpc", "factoextra",
  "ggplot2"
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
library(ncdf4)
library(raster)
library(cluster)
library(fpc)
library(factoextra)
library(ggplot2)

#' Loading color palette
source("scripts/color_palette.R")

#' Reading vectorial data
lyrs <- rgdal::ogrListLayers(
  "data/vector/bastian_region/climatic_regions.gpkg"
)

region <- st_read(
  dsn = "data/vector/bastian_region/climatic_regions.gpkg",
  layer = lyrs[1], quiet = T, as_tibble = T
)# %>% #===== it's optional =====#
#  dplyr::filter(Region %in% c(
#    "Pacific coast", "Andes west slope",
#    "Andes east slope"
#  ))

#' Reading raster data
pp  <- brick("data/raster/pp/pisco/clim/PISCOp_v2.1.1_clim_1981-2017.nc")
dem <- raster("data/raster/dem/SRTM_10km_resampled_to_PISCO.nc")

#' Stacking rainfall climatology and elevation
stck.data <- stack(pp, dem) %>% mask(region)

#' Building table before to the cluster analysis
df <- coordinates(stck.data) %>%
  cbind(getValues(stck.data)) %>%
  as_tibble() %>%
  drop_na()

names(df) <- c("lon", "lat", sprintf("%.*s", 3, month.name), "elev")

#' Partitioning Around Medoids (PAM)
#'   1| standardization of data
stzn.df <- scale(df)
#'
#' ===========================================
#' ==================== Or ===================
#' ===========================================
#'
#'   1| Principal Coomponent Analysis
stzn.df <- prcomp(df, scale. = T, center = T)
stzn.df <- stzn.df$x[, 1:15] %>% as_tibble() # define number of components

#'   2| calculating the best number of clusters
#'     2.1| building ASW (Average Silhouette Width) plot
aws.plt <- factoextra::fviz_nbclust(stzn.df,
  pam,
  method = "silhouette"
) +
  theme_bw() +
  geom_vline(xintercept = 8, linetype = 2)
#'     2.2| saving plot
ggsave(aws.plt,
  filename = "exports/aws_plt.png",
  width = 10, height = 8, units = "cm", dpi = 500
)
save(aws.plt, file = "data/rdata/aws_plt.RData")

#'     2.3| building WSS (Within Sum of Square) plot
wss.plt <- factoextra::fviz_nbclust(stzn.df,
  pam,
  method = "wss"
) +
  theme_bw() +
  geom_vline(xintercept = 8, linetype = 2)
#'     2.4| saving plot
ggsave(wss.plt,
  filename = "exports/wss_plt.png",
  width = 10, height = 8, units = "cm", dpi = 500
)
save(wss.plt, file = "data/rdata/wss_plt.RData")

#'     2.5| building gap_stat (Gap Statistics) plot
gap.stat.plt <- factoextra::fviz_nbclust(stzn.df,
  pam,
  method = "gap_stat"
) +
  theme_bw() +
  geom_vline(xintercept = 8, linetype = 2)
#'     2.6| saving plot
ggsave(gap.stat.plt,
  filename = "exports/gap_stat_plt.png",
  width = 10, height = 8, units = "cm", dpi = 500
)
save(gap.stat.plt, file = "data/rdata/gap_stat_plt.RData")

#'     2.7| the best number of clusters
k.nbclust <- 8

#'   3| Calculating distance matrix
#'   ===== it's optional =====
#'     3.1| choose between euclidean, gower or manhattan metric
dist.df <- cluster::daisy(stzn.df, metric = "euclidean")

#'     3.2| ploting distance matrices
dist.mtrx.plt <- fviz_dist(dist.df)

#'   4| Cluster analysis by Partitioning Around Medoids
#'     if you insert distance matrix (dist.df), it's necessary that you add
#'     "diss = T" argument into pam() function
clus.df <- pam(stzn.df, k.nbclust, metric = "euclidean")

#' Plot average silhouette value and build table with number of
#'   negative values by cluster
#'     1| building average silhouette value plot
sltte.plt <- factoextra::fviz_silhouette(clus.df,
  palette = cb_palette,
  label = FALSE,
  print.summary = TRUE
) + theme_bw() + coord_flip() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 1)
  )
#'     2| saving average silhouette value plot
ggsave(sltte.plt,
  filename = "exports/average_silhouette_value_plot.png",
  width = 18, height = 15, units = "cm", dpi = 500
)

#'     1| building number of negative value table
sltee.nv <- clus.df$silinfo[[1]] %>%
  as_tibble() %>%
  filter(sil_width < 0) %>%
  group_by(cluster) %>%
  summarise(neg_val = length(sil_width))

#' Plot clusters from the first two components of PCA
#'   1| building cluster plot
clus.plt <- factoextra::fviz_cluster(clus.df,
  ggtheme = theme_bw(),
  ellipse.type = "t",
  palette = cb_palette,
  geom = "point",
  star.plot = TRUE
)
#'   2| saving cluster ploting
ggsave(clus.plt,
  filename = "exports/rainfall_data_clusters.png",
  width = 18, height = 15, units = "cm", dpi = 500
)

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
  "data/raster/cluster_k8_dist_EUCLIDEAN_all_PERU_without_PCA_v2.tif",
  overwrite = T
)