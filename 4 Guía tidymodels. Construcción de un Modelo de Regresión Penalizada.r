################################################################################
################################################################################
##
## PRESENTACIÓN: Guía Rápida para el uso de tidymodels
##
## SCRIPT: 4 Guía tidymodels. Construcción de un Modelo de Regresión Penalizada
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
dim(xx_sob_train)
dim(xx_sob_test)  


################################################################################
## Paralelización

num_cores <- parallel::detectCores()
num_cores

if (!grepl("mingw32", R.Version()$platform)) {
   library(doMC)
   registerDoMC(cores = num_cores - 1)
} else {
   library(doParallel)
   cl <- makePSOCKcluster(num_cores - 1)
   registerDoParallel(cl)
}

 
################################################################################
## Especificaciones
   
## 1.- Especificaciones del recipe
normalized_rec <- 
   recipe(Solubility ~ ., data = xx_sob_train) %>% 
   step_normalize(all_numeric_predictors())    

## 2.- Especificaciones del modelo lineal con elastic net
enet_spec <- 
   linear_reg(penalty = tune(), mixture = tune()) %>% 
   set_engine("glmnet") %>%
   set_mode("regression")   
   
penalty()
mixture()
   
## 3.- Se crea el workflow   
wflow <- workflow() %>%
  add_model(enet_spec) %>% 
  add_recipe(normalized_rec)   
wflow 

## 4.- Especificaciones de la técnica de remuestreo
cv_split <- vfold_cv(xx_sob_train, strata = Solubility, v = 10, repeats = 10)
cv_split
   
## 5.- Se crea un grid 
enet_grid <- grid_regular(penalty(), mixture(),
                          levels = list(penalty = 100, mixture = 11) )   
enet_grid


################################################################################ 
## Se ejecuta la optimización de parámetros

keep_pred <- control_grid(save_pred = TRUE) ## salva las predicciones

tune_result <- wflow %>% 
  tune_grid(cv_split, grid = enet_grid, control = keep_pred,
            metrics = metric_set(rmse, rsq))

## Plot 
autoplot(tune_result, metric = "rmse") +
  scale_color_viridis_d(direction = -1) +
  ylim(0.65, 0.80) +
  theme(legend.position = "top")


## Se analizan los resultados
tune_result %>% 
  collect_metrics()

## Los mejores modelos
show_best(tune_result, metric="rmse")
show_best(tune_result, metric="rsq")
## El mejor modelo
select_best(tune_result, metric="rmse")

## Parámetros del modelo con mínimo RMSE
tune_best <- tune_result %>% select_best(metric = "rmse")
tune_best$penalty  
tune_best$mixture
  
## Parámetros del modelo con mínimo RMSE de la Regla 1-SE  
tune_best_SE <- tune_result %>% select_by_one_std_err(mixture, metric = "rmse")
tune_best_SE$penalty  
tune_best_SE$mixture


################################################################################
## Modelo final
################################################################################

## Se crea al workflow final
final_wflow <-
  wflow %>% 
  finalize_workflow( select_best(tune_result, metric="rmse") )

## Se crea al model final
enet_fit <-
  final_wflow %>%
  fit(xx_sob_train)
enet_fit
  
## Coeficientes del modelo final  
tidy(enet_fit) %>% print(n=4)  
tidy(enet_fit) %>% filter( estimate != 0 ) %>% print(n=4)    
  
## Modelo final "glmnet"
out_glmnet <- extract_fit_engine(enet_fit)
class(out_glmnet)  
  
  
################################################################################
## Predicciones y evaluación en Testing
################################################################################

## Predicciones en la muestra de testing
pred_enet_df <- predict(enet_fit, new_data = xx_sob_test)
pred_enet_df %>% print(n=4)

pred_enet_df <- bind_cols(pred_enet_df, xx_sob_test %>% select(Solubility))
pred_enet_df %>% print(n=4)

## Medidas de capacidad predictiva 
rmse(pred_enet_df, truth = Solubility, estimate = .pred )  
rsq(pred_enet_df, truth = Solubility, estimate = .pred )  
  
all_metrics <- metric_set(rmse, rsq, mae)   
all_metrics(pred_enet_df, truth = Solubility, estimate = .pred )  

## Gráfico de diagnóstico, en la muestra de testing
dev.new()
ggplot( pred_enet_df, aes(x = Solubility, y = .pred)) + 
  geom_abline(lty = 2) + 
  geom_point() +
  coord_obs_pred() +
  theme_bw()  


################################################################################
## Cross-Validated Predictions. Predicciones del proceso de tune
################################################################################


## Extraer las Cross-Validated Predictions 
assess_res <- collect_predictions(tune_result) 
assess_res %>% print(n=4)
884 * 1100 * 10    ## 884 obs. en training, 1100 parámetros, 10 repeticiones de CV

## Predicciones con summarize ( 1 valor por observación )
assess_res_summ <- collect_predictions(tune_result, summarize=TRUE) 
assess_res_summ %>% print(n=4)

## Cross-validated predictions del modelo con parámetros óptimos
assess_res_summ_best <-
  assess_res_summ %>% 
  filter( penalty == tune_best$penalty, 
          mixture == tune_best$mixture )
assess_res_summ_best %>% print(n=4)

## RMSE
rmse(assess_res_summ_best, truth = Solubility, .pred )  

## Gráfico de diagnóstico
dev.new()
assess_res_summ_best %>%
  ggplot(aes(x = Solubility, y = .pred)) +
  geom_point(alpha=0.2) +
  geom_abline(color="red") +
  coord_obs_pred() +
  ylab("Predicted") +
  theme_bw()


################################################################################
## Cerramos los clusters
################################################################################
stopCluster(cl)    


