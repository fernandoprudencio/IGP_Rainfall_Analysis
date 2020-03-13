#'@author  Fernando Prudencio
#'this script does a cluster analysis
rm(list = ls())
pkg <- c('sf', 'tidyverse', 'raster', 'cluster')
sapply(pkg, function(x){if(x %in% rownames(installed.packages()) == FALSE){install.packages(x)}})

library(sf)
library(tidyverse)
library(raster)
library(cluster)
library(ncdf4)

#'READINF VECTORIAL DATA
region <- read_sf('data/shp/bastian_region/bastian_region.shp')

#'READING RASTER DATA
pp <- brick('D:/02-IGP/03-Proyectos_IGP/04-IGP_pp/data/raster/pp/pisco/clim/PISCOp_v2.1.1_clim_1981-2017.nc')
dem <- raster('D:/02-IGP/03-Proyectos_IGP/04-IGP_pp/data/raster/dem/SRTM_500_resampled_PISCO.nc')


#'STACKING CLIMATOLOGY RAINFALL AND ELEVATION
#stck <- stack(pp, dem) %>% mask(region %>% filter(Region %in% c('Andes west slope', 'Andes east slope')))
stck <- stack(pp, dem) %>% mask(region %>% filter(Region %in% c('Amazon sub-Andes','Amazon lowland')))
stck <- stack(pp, dem) %>% mask(region)

#'MAKING TABLE FOR CLUSTER ANALYSIS
df <- coordinates(stck) %>% cbind(getValues(stck)) %>% as_tibble() %>% drop_na()
names(df) <- c('lon','lat', month.name,'elev')

#'PARTITIONING AROUND MEDOIDS - PAM
#'CALCULATING THE NUMBER OF OPTIMAL CLUSTERS
#'Standardization of the data
df_end <- scale(df) %>% as_tibble()

#'Principal Component Analysis - PCA
#df_pca <- prcomp(df_end, scale = T, center = T)
df_pca <- prcomp(df_end)

#'Sum of squares within clusters
for(i in 2:20){
  print(i)
  #i = 2
  n = pam(df_end, i)#df_pca$x[,1:2]
  if (i == 2) { wss = n$silinfo$avg.width }else{ wss = c(wss, n$silinfo$avg.width) }
}
df_pl <- data.frame(ncluster = 2:20, wss)
summary(df_pl$wss)
plt <- ggplot(df_pl, aes(ncluster, wss)) +
  geom_point(shape = 21, fill = 'black', color = 'black', size = 3.5) +
  geom_line(linetype = "dashed", size = .8) + theme_bw() +
  ylab(label = 'WWS\n') +  xlab(label = '\nNumber of Clusters') +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0,20,2), expand = c(.05,.05)) +
  scale_y_continuous(breaks = seq(0.35,0.65,0.05), limits = c(0.35,0.65), expand = c(0,0)) +
  #scale_y_continuous(breaks = seq(0.365,0.445,0.01), limits = c(0.365,0.445), expand = c(0,0)) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.35, ymax = .65, alpha = 0.1, fill="red")
plt
ggsave(plt, filename = 'exports/optm_wss_ndwi.png',
       width = 15, height = 15, units = "cm", dpi = 500) 


#'Silhouette value
#'Distance matrix
dtca <- daisy(df_end)
for(j in 2:13){#12:17
  #j = 2
  print(j)
  n = pam(df_end, j)
  if (j == 2) { asw = list(silhouette(n$cluster, dtca))}else{
    asw[[j-1]] = silhouette(n$cluster, dtca) }
}

library(factoextra)
t <- 16
ti <- 1
p1 <-  fviz_silhouette(asw[[1]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p2 <-  fviz_silhouette(asw[[2]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title  = element_text(size = ti), legend.position = 'none')

p3 <-  fviz_silhouette(asw[[3]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p4 <-  fviz_silhouette(asw[[4]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p5 <-  fviz_silhouette(asw[[5]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p6 <-  fviz_silhouette(asw[[6]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p7 <-  fviz_silhouette(asw[[7]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = ti),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = t), legend.position = 'none')

p8 <-  fviz_silhouette(asw[[8]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title  = element_text(size = ti), legend.position = 'none')

p9 <-  fviz_silhouette(asw[[9]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p10 <-  fviz_silhouette(asw[[10]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p11 <-  fviz_silhouette(asw[[11]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')

p12 <-  fviz_silhouette(asw[[12]]) + theme_bw() + coord_flip() +
  ylab(label = '') +  xlab(label = '') +
  theme(axis.text.x = element_text(size = t),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_text(size = ti), legend.position = 'none')
library(ggpubr)
final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 4, nrow = 3)#, p7, p8, p9, p10, p11, p12
final_plot

for (k in 1:12) {
  print(which(asw[[k]][,3] < 0) %>% length())
}

# Criterio de Calinski-Harabasz
library(fpc)
ch <- numeric()
for(h in 2:20){
  print(h)
  res = pam(df_end, h)
  ch[h-1] = calinhara(df_end, res$clustering)
}
df_pl <- data.frame(ncluster = 2:20, ch)
plt <- ggplot(df_pl, aes(ncluster, ch)) +
  geom_point(shape = 21, fill = 'black', color = 'black', size = 3.5) +
  geom_line(linetype="dashed", size = .8) + theme_bw() +
  ylab(label = 'Índice Calinski-Harabasz\n') +  xlab(label = '\nNúmero de Clusters') +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0,20,2), expand = c(.05,.05)) +
  scale_y_continuous(breaks = seq(20,300,20), limits = c(20,300), expand = c(0,0)) +
  #scale_y_continuous(breaks = seq(160,255,10), limits = c(160,255), expand = c(0,0)) +
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 20, ymax = 300, alpha = 0.1, fill="red")
plt
ggsave(plt, filename = 'F:/4-temp/04-IGP_pp/08-Figuras/optm_CHarabasz_group02.png',
       width = 15, height = 15, units = "cm", dpi = 500) 
#

res = pam(df_end, 8)
#'CONSTRUCCIÓN DE LOS RESULTADOS
xy = coordinates(stck) %>% as.data.frame()
names(xy) <- c('lon','lat')
dfs = xy %>% left_join(cbind(df %>% dplyr::select(lon, lat), clus = res$clustering), by = c('lon','lat'))
names(dfs) = c("x","y","z")
EVI_CRS = crs(stck)
EVI_RES = res(stck)
EVIraster = rasterFromXYZ(dfs, EVI_RES, EVI_CRS, digits=0)
writeRaster(EVIraster, 'D:/02-IGP/03-Proyectos_IGP/04-IGP_pp/data/raster/cluster_k8_withtout_PCA_Amazon.tif', overwrite=T)


