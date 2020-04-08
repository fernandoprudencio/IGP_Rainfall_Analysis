rm(list = ls())
#'
library(tidyverse)
data("USArrests")      # Load the data set
df <- scale(USArrests) # Scale the data or data standardization
colnames(df)
head(df, n = 3)        # View the firt 3 rows of the data
#'
#' COMPUTING PAM IN R
#'
#'   The function pam() [cluster package] and pamk() [fpc package] can be
#'     used to compute PAM.
#'   The function pamk() does not require a user to decide the number of
#'     clusters K.
#'
library(cluster)
library(fpc)
cluster::pam(x, k, metric = "euclidean", stand = FALSE)
#' Where
#'   x: possible values includes:
#'     Numeric data matrix or numeric data frame: each row corresponds to an
#'       observation, and each column corresponds to a variable.
#'     Dissimilarity matrix: in this case x is typically the output of daisy()
#'       or dist()
#'   k: The number of clusters.
#'   metric: the distance metrics to be used. Available options are
#'     “euclidean” and “manhattan”.
#'   stand: logical value; if true, the variables (columns) in x are
#'     standardized before calculating the dissimilarities. Ignored when x is
#'     a dissimilarity matrix.
#'
#' To create a beautiful graph of the clusters generated with the pam()
#'   function, will use the factoextra package.
#'
#' Installing required packages:
install.packages("factoextra")
#' Loading the packages:
library(factoextra)
#'
#' ESTIMATING THE OPTIMAL NUMBER OF CLUSTERS
#'
#'   To estimate the optimal number of clusters, we’ll use the average
#'     silhouette method.
#'   The idea is to compute PAM algorithm using different values of clusters k.
#'     Next, the average clusters silhouette is drawn according to the number
#'     of clusters. The average silhouette measures the quality of a clustering.
#'     A high average silhouette width indicates a good clustering. The optimal
#'     number of clusters k is the one that maximize the average silhouette over
#'     a range of possible values for k (Kaufman and Rousseeuw [1990]).
#'   The R function fviz_nbclust() [factoextra package] provides a convenient
#'     solution to estimate the optimal number of clusters.
#'
factoextra::fviz_nbclust(df, pam, method = "silhouette") +
  theme_classic()
#' From the plot, the suggested number of clusters is 2. In the next section,
#'   we’ll classify the observations into 2 clusters.
#'
#' COMPUTING PAM CLUSTERING
#' 
#' The R code below computes PAM algorithm with k = 2:
pam.res <- cluster::pam(df, 2)
print(pam.res)
#' The printed output shows:
#'   the cluster medoids: a matrix, which rows are the medoids and columns are
#'     variables
#'   the clustering vector: A vector of integers (from 1:k) indicating the
#'     cluster to which each point is allocated
#'
#' If you want to add the point classifications (results) to the original data,
#'   use this:
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)
#'
#' ACCESSING TO THE RESULTS OF THE PAM() FUNCTION
#' 
#'   The function pam() returns an object of class pam which components include:
#'
#'     medoids: Objects that represent clusters
#'     clustering: a vector containing the cluster number of each object
#'
#' These components can be accessed as follow:
pam.res$medoids
#' Cluster numbers
head(pam.res$clustering)
#'
#' VISUALIZING PAM CLUSTERS
#'
#' To visualize the partitioning results, we’ll use the function fviz_cluster()
#'   [factoextra package]. It draws a scatter plot of data points colored by
#'   cluster numbers. If the data contains more than 2 variables, the Principal
#'   Component Analysis (PCA) algorithm is used to reduce the dimensionality of
#'   the data. In this case, the first two principal dimensions are used to
#'   plot the data.
#'
factoextra::fviz_cluster(pam.res,
  palette = c("#00AFBB", "#FC4E07"), # color palette
  ellipse.type = "t", # Concentration ellipse
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_classic()
<<<<<<< HEAD
)
#'
#' CLUSTERING DISTANCE MEASURES
#'
#' The classification of observations into groups requires some methods for
#'   computing the distance or the (dis)similarity between each pair of
#'   observations. The result of this computation is known as a dissimilarity
#'   or distance matrix.
#'
#' There are many methods to calculate this distance information. In this
#'   article, we describe the common distance measures and provide R codes
#'   for computing and visualizing distances.
#'
#' METHODS FOR MEASURING DISTANCES
#'   The choice of distance measures is a critical step in clustering. It
#'     defines how the similarity of two elements (x, y) is calculated and it
#'     will influence the shape of the clusters.
#'
#'   The classical methods for distance measures are Euclidean and Manhattan
#'     distances
#'
#'   Other dissimilarity measures exist such as correlation-based distances,
#'     which is widely used for gene expression data analyses. Correlation-based
#'     distance is defined by subtracting the correlation coefficient from 1.
#'     Different types of correlation methods can be used,
#'
#' Subset of the data
set.seed(123)
ss <- sample(1:50, 15)   # Take 15 random rows
df <- USArrests[ss, ]    # Subset the 15 rows
df.scaled <- scale(df)   # Standardize the variables
#'
#' COMPUTING EUCLIDEAN DISTANCE
#' 
#' There are many R functions for computing distances between pairs of
#'   observations:
#' 
#' 1. dist() R base function [stats package]: Accepts only numeric data as
#'   an input.
#' 2. get_dist() function [factoextra package]: Accepts only numeric data as
#'   an input. Compared to the standard dist() function, it supports
#'   correlation-based distance measures including “pearson”, “kendall” and
#'   “spearman” methods.
#' 3. daisy() function [cluster package]: Able to handle other variable types
#'   (e.g. nominal, ordinal, (a)symmetric binary). In that case, the Gower’s
#'   coefficient will be automatically used as the metric. It’s one of the
#'   most popular measures of proximity for mixed data types. For more details,
#'   read the R documentation of the daisy() function (?daisy).
#'
#' To compute Euclidean distance, you can use the R base dist() function,
#'   as follow:
#'
dist.eucl <- dist(df.scaled, method = "euclidean")
# Distancia entre variables
dist.eucl <- dist(t(df.scaled), method = "euclidean")
#'
#'
#' COMPUTING CORRELATION BASED DISTANCES
#'
#'   Correlation-based distances are commonly used in gene expression data
#'     analysis. The function get_dist()[factoextra package] can be used to
#'     compute correlation-based distances. Correlation method can be either
#'     pearson, spearman or kendall.
#'
dist.cor <- factoextra::get_dist(df.scaled, method = "pearson")
# Display a subset
round(as.matrix(dist.cor)[1:3, 1:3], 1)
#'
#'
#' COMPUTING DISTANCES FOR MIXED DATA
#' 
#'   The function daisy() [cluster package] provides a solution
#'     (Gower’s metric) for computing the distance matrix, in the situation
#'     where the data contain no-numeric columns.
#'   The R code below applies the daisy() function on flower data which
#'     contains factor, ordered and numeric variables:
library(cluster)
data(flower)
head(flower, 3)
#' Data structure
str(flower)
#' Distance matrix
dd <- cluster::daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)
#'
#'
#' VISUALIZING DISTANCE MATRICES
#' 
#'   A simple solution for visualizing the distance matrices is to use the
#'     function fviz_dist() [factoextra package]. Other specialized methods,
#'     such as agglomerative hierarchical clustering or heatmap will be
#'     comprehensively described in the dedicated courses.
#'
factoextra::fviz_dist(dist.eucl)
factoextra::fviz_dist(dd)
#'   Red: high similarity (ie: low dissimilarity) | Blue: low similarity
=======
)
>>>>>>> master
