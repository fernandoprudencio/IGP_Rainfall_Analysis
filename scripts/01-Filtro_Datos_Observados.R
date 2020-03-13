#'@author  Fernando P.
#'
rm(list = ls())
library(dplyr)
library(ggplot2)
#----- EDITANDO TABLA QUE ENLISTA LAS ESTACIONES POR DEPARTAMENTO,
# NOMBRE DE ESTACION Y CODIGO
list_data <- read.csv('F:/4-temp/04-IGP_pp/BD_Peru_Clima/Datos.csv', sep = ';')
cod_est   <- list_data$COD_EST

for (i in 1:length(cod_est)) {
  n    = 8 - nchar(cod_est[i])
  cod_est[i] = paste(substr('00000000',1,n), cod_est[i], sep = '')
}

list_data <- list_data %>% mutate(COD_EST = cod_est, cod = paste(Prefijo, COD_EST,sep = '')) %>% dplyr::select(-Prefijo,-COD_EST)

#----- 
#lista <- substr(list.files('F:/4-temp/04-IGP_pp/BD/', pattern = '.txt$'),1,10)
lista <- list.files('F:/4-temp/04-IGP_pp/BD/', pattern = '.txt$',full.names = T)
header <- c('YEAR','MONTH','DAY','Pp','Tmax','Tmin')
#-------------------------------  

for (i in 1:293) {
  #i=1
  print(i)
  #------------------------------------------------------------------------------------------------------------------#
  if (ncol(read.table(lista[i])) == 6) {
    table = read.table(lista[i], col.names = header[1:6], fileEncoding = "UTF-8-BOM") %>% mutate(date = paste(YEAR, MONTH, DAY, sep = '-') %>% as.Date())
    date2 = seq(table$date[1], table$date[nrow(table)], by = 'day') }
  if (ncol(read.table(lista[i])) == 4) {
    table = read.table(lista[i], col.names = header[1:4], fileEncoding = "UTF-8-BOM") %>% mutate(date = paste(YEAR, MONTH, DAY, sep = '-') %>% as.Date())
    date2 = seq(table$date[1], table$date[nrow(table)], by = 'day') }
  #-----------------------------------------------------------------------------------------------------------------#
  if (any(table$date != date2)) {cat("Hay un error coincidencia de fechas\n")}
  #-------------------------- CONSISTENCIA DE DATOS DE Pp ----------------------------------------------------------#
  if (any(table$Pp == '')) {cat("HAY CELDAS DE Pp VACIAS\n")
    table %>% filter(Pp == '')
    print(table %>% filter(is.na(Pp)))
    print(lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  if (!any(table$Pp == -99.9)) {cat("NO HAY VALORES DE Pp = -99.9\n")}
  if (any(table$Pp < 0 & table$Pp != -99.9)) {cat("HAY VALORES DE Pp NEGATIVOS DIFERENTES A -99.9\n")
    #print(k%>%filter(Pp < 0 & Pp != -99.9))
    print(table %>% filter(Pp < 0 & Pp != -99.9))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  if (any(table$Pp > 100)) {cat("HAY VALORES DE Pp MAYORES A 100\n")
    print(table %>% filter(Pp > 100))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  #--------------------------- CONSISTENCIA DE DATOS DE Tmax ---------------------------------------------------------#
  if (any(table$Tmax == '')) {cat("HAY CELDAS DE Tmax VACIAS\n")
    table %>% filter(Tmax == '')
    print(table %>% filter(is.na(Tmax)))
    print(lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  if (!any(table$Tmax == -99.9)) {cat("NO HAY VALORES DE Tmax = -99.9\n")}
  if (any(table$Tmax < 0 & table$Tmax != -99.9)) {cat("HAY VALORES DE Tmax NEGATIVOS DIFERENTES A -99.9\n")
    #print(k%>%filter(Tmax < 0 & Tmax != -99.9))
    print(table %>% filter(Tmax < 0 & Tmax != -99.9))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  #--------------------------- CONSISTENCIA DE DATOS DE Tmin --------------------------------------------------------#
  if (any(is.na(table$Tmin))) {cat("HAY CELDAS DE Tmin VACIAS\n")
    table %>% filter(Tmin == '')
    print(table %>% filter(is.na(Tmin)))
    print(lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
  if (!any(table$Tmin == -99.9)) {cat("NO HAY VALORES DE Tmin = -99.9\n")}
  if (any(table$Tmin < -30 & table$Tmin != -99.9)) {cat("HAY VALORES DE Tmin NEGATIVOS DIFERENTES A -99.9\n")
    #print(k%>%filter(Tmin < 0 & Tmin != -99.9))
    print(table %>% filter(Tmin < -30 & Tmin != -99.9))
    print(list_data %>% filter(cod == lista[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,10)) )}
}
#-----------------------------------------------------------------------------------------------------------------#









