################################################################################
################################################################################
##
## PRESENTACIÓN: Guía Rápida para el uso de tidymodels
##
## SCRIPT: 1 Guía tidymodels. Ejemplo Preprocesamiento 
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
## Se cargan los datos, muestras de training y testing
load("D://Solubility Data.RData")
ls()
dim(xx_sob_train)
dim(xx_sob_test)  

## 1.- Se crea la receta
sol_rec <-
  recipe( Solubility ~ . , data=xx_sob_train ) %>%
  step_nzv(all_predictors(), freq_cut = 95/5, unique_cut = 10) %>%
  step_corr(all_predictors(), threshold = 0.8 ) %>%  
  step_normalize(all_numeric_predictors())
sol_rec

## 2.- Se prepara la receta
sol_obj <- prep(sol_rec, training = xx_sob_train)

## 3.- Se aplica la receta a los dos data frames
xx_sob_proc_train <- bake(sol_obj, xx_sob_train)
xx_sob_proc_test <- bake(sol_obj, xx_sob_test)

## Chequeamos
dim(xx_sob_train)
dim(xx_sob_proc_train)
dim(xx_sob_proc_test)

## Chequeamos
mean(xx_sob_train$MolWeight)
sd(xx_sob_train$MolWeight)

mean(xx_sob_proc_train$MolWeight)
sd(xx_sob_proc_train$MolWeight)

mean(xx_sob_proc_test$MolWeight)
sd(xx_sob_proc_test$MolWeight)

