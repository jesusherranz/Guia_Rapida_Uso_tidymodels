################################################################################
################################################################################
##
## PRESENTACIÓN: Guía Rápida para el uso de tidymodels
##
## SCRIPT: 2 Guía tidymodels. Modelo Regresión Lineal
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

## Especificación del modelo
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")
lm_model  

## Construcción del modelo usando fórmulas
lm_form_fit <-
  lm_model %>%
  fit( Solubility ~ FP001 + FP004 + MolWeight + NumAtoms, data=xx_sob_train ) 
lm_form_fit
class(lm_form_fit)

## Construcción del modelo usando x / y
lm_xy_fit <-
  lm_model %>%
  fit_xy(x = xx_sob_train %>% select(FP001, FP004, MolWeight, NumAtoms), 
         y = xx_sob_train %>% select(Solubility) )
lm_xy_fit
class(lm_xy_fit)
class(lm_form_fit %>% extract_fit_engine())

## Coeficientes del modelo
tidy(lm_form_fit)

## Predicción en Testing
predict(lm_form_fit, new_data=xx_sob_test)


