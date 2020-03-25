data("USArrests")      # Load the data set
df <- scale(USArrests) # Scale the data
colnames(df)
head(df, n = 3)        # View the firt 3 rows of the data
#' La función pam() [ paquete de clúster ] y pamk() [ paquete fpc ]
#'   se pueden usar para calcular PAM.
#' La función pamk() no requiere que un usuario decida el número de
#'   clústeres K.
library(cluster)
library(fpc)
pam(x, k, metric = "euclidean", stand = FALSE)
#' x      : los valores posibles incluyen:
#'   Matriz de datos numéricos o marco de datos numéricos: cada fila
#'     corresponde a una observación, y cada columna corresponde a una
#'     variable.
#'   Matriz de disimilitud: en este caso, x es típicamente la salida
#'     de las funciones daisy() o dist()
#' k      : el número de grupos
#' metric : las métricas de distancia que se utilizarán. Las opciones
#'   disponibles son "euclidiana" y "manhattan"
#' stand  : valor lógico
#'   Si es verdadero, las variables (columnas) en x se estandarizan
#'     antes de calcular las diferencias. Ignorado cuando x es una matriz
#'     de disimilitud.
#' 
#' 
#' Para crear un hermoso gráfico de los clústeres generados con la
#'   función pam(), utilizará el paquete factoextra.
install.packages("factoextra")
library(factoextra)
#' Estimando el número de óptimo de grupos
#' El número óptimo de grupos k es el que maximiza la silueta promedio
#'   en un rango de valores posibles para k (Kaufman y Rousseeuw 1990).
#' La función de R, fviz_nbclust() [ paquete factoextra ] proporciona
#'   una solución conveniente para estimar el número óptimo de clústeres.
factoextra::fviz_nbclust(df, pam, method = "silhouette") +
  theme_classic()
#' El siguiente código R calcula el algoritmo PAM con k = 2
pam.res <- pam(df, 2)
print(pam.res)
#' La salida impresa muestra:
#'   Los medoides del clúster: una matriz, cuyas filas son los medoides y
#'     las columnas son variables
#'   El vector de agrupamiento: Un vector de enteros (de 1: k) que indica
#'     el grupo al que se asigna cada punto
#' 
#' Si desea agregar las clasificaciones de puntos a los datos originales,
#'   use esto:
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)








