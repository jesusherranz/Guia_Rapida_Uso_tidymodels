################################################################################
################################################################################
##
## PRESENTACIÓN: Guía Rápida para el uso de tidymodels
##
## SCRIPT: 3 Guía tidymodels. Partición inicial
##
## LUGAR:  Grupo de Usuarios de R de Madrid 
##         24 de Septiembre de 2025
##
## Autor:  Jesús Herranz Valera
##
################################################################################
################################################################################

rm(list=ls(all=TRUE)) 

################################################################################
## Librerías               
library(tidymodels)
library(tidyverse)

################################################################################
## Lectura del fichero con read_delim del paquete readr
xx_sob <- read_delim("D://Solubility.csv", delim = ";")

## Especificaciones de las Muestras de Training y Testing
set.seed(222) ## Se fija una semilla, para reproducir los datos
sob_split <- initial_split(xx_sob, prop = 0.70, strata = Solubility)
sob_split

## Se crean las muestras de Training y Testing 
xx_sob_train <- training(sob_split)
xx_sob_test <- testing(sob_split)

dim(xx_sob_train)
dim(xx_sob_test)

quantile(xx_sob$Solubility)
quantile(xx_sob_train$Solubility)
quantile(xx_sob_test$Solubility)

## Salva los data frames en un RData
save( xx_sob_train, xx_sob_test, file = "D://Solubility Data.RData" )


