################################################################################
################################################################################
##
## PRESENTACI�N: Gu�a R�pida para el uso de tidymodels
##
## SCRIPT: 2 Gu�a tidymodels. Modelo Regresi�n Lineal
##
## LUGAR:  Grupo de Usuarios de R de Madrid 
##         24 de Septiembre de 2025
##
## Autor:  Jes�s Herranz Valera
##
################################################################################
################################################################################

rm(list=ls(all=TRUE)) 

################################################################################
## Librer�as               
library(tidymodels)
library(tidyverse)

################################################################################
## Se cargan los datos, muestras de training y testing
load("D://Solubility Data.RData")
ls()
dim(xx_sob_train)
dim(xx_sob_test) 

## Especificaci�n del modelo
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")
lm_model  

## Construcci�n del modelo usando f�rmulas
lm_form_fit <-
  lm_model %>%
  fit( Solubility ~ FP001 + FP004 + MolWeight + NumAtoms, data=xx_sob_train ) 
lm_form_fit
class(lm_form_fit)

## Construcci�n del modelo usando x / y
lm_xy_fit <-
  lm_model %>%
  fit_xy(x = xx_sob_train %>% select(FP001, FP004, MolWeight, NumAtoms), 
         y = xx_sob_train %>% select(Solubility) )
lm_xy_fit
class(lm_xy_fit)
class(lm_form_fit %>% extract_fit_engine())

## Coeficientes del modelo
tidy(lm_form_fit)

## Predicci�n en Testing
predict(lm_form_fit, new_data=xx_sob_test)


