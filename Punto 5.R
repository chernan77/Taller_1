## -----------------------------------------------------------------------------
##
## Punto 5: Predicting earnings . In the previous sections, you estimated some 
## specifications with inference in mind. In this subsection, we will evaluate 
## the predictive power of these specifications.
##
## -----------------------------------------------------------------------------

## Set Up:

# Set directory

setwd("C:/Users/IPACOLPC066/Documents/GitHub/Taller_1") # Cambiar por usuario

# Set the CRAN mirror to use for package installation
options(repos = "https://cran.rstudio.com/")

# Install and load necessary packages

library(tidyverse)
library(rvest)
library(purrr)
library(dplyr)
library(writexl)
library(kableExtra)
library(knitr)
library(flextable)
library(ggplot2)
library(boot)
library(lmtest)
library(car)
library(xtable)
library(DT)
library(jpeg)
require(pacman)
library(stargazer)
library(writexl)
library(readxl)

p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

# Cargar base de datos

path_base<-"Base_final.xlsx"
Tabla_4 <- read_excel(path_base, sheet = "Sheet 1")

#Usar la tabla con filtros: Tabla_4
Tabla_4$lw_hora <- log(Tabla_4$w_hora)

Tabla_4<- Tabla_4 %>%
  mutate(across(c(Educ, depto, Estrato), as.factor)) %>%
  mutate(Horas_trabajadas2=Horas_trabajadas^2) %>%
  mutate(Edad2=Edad^2) 

Tabla_4$Sector <- ifelse(Tabla_4$formal == 1 & Tabla_4$informal == 0, 1, 0)

# a. Split Sample:--------------------------------------------------------------
set.seed(123)
sample_split <- initial_split(Tabla_4, prop = .7)

# Create test and train data frames 
train <- training(sample_split)
test  <- testing(sample_split)


# b. Report and compare the predictive performance in terms of the RMSE:--------

# Especificaciones de modelos utilizados:
model1 <- recipe(lw_hora~ Edad + Edad2, data = train)
model2 <- recipe(lw_hora~ Sexo, data = train)
model3 <- recipe(lw_hora~ Edad + Edad2 + Sexo, data = train)
model4 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas , data = train)  %>%
  step_dummy(all_factor_predictors())
model5 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato, data = train)  %>%
  step_dummy(all_factor_predictors())
model6 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ, data = train)  %>%
  step_dummy(all_factor_predictors())
model7 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector, data = train)  %>%
  step_dummy(all_factor_predictors())
model8 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ+ informal + Sector + Size_empresa, data = train)  %>%
  step_dummy(all_factor_predictors())
model9 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + maxEducLevel + Sector + Size_empresa, data = train)  %>%
  step_dummy(all_factor_predictors())
model10 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector + Size_empresa + exp , data = train)  %>%
  step_dummy(all_factor_predictors())
model10_2 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector + Size_empresa , data = train)  %>%
  step_dummy(all_factor_predictors())
model10_3 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector + Size_empresa + Educ , data = train)  %>%
  step_dummy(all_factor_predictors())
model10_4 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector + Size_empresa + maxEducLevel , data = train)  %>%
  step_dummy(all_factor_predictors())

model7_2 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Estrato) %>%
  step_dummy(all_factor_predictors())
model7_3 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:informal) %>%
  step_dummy(all_factor_predictors())
model7_4 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())

#List models
modelos<-list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model10_2, model10_3, model10_4, model7_2, model7_3, model7_4)

# Create loop to fit with workflows

fit_model <- function(x, df=train) {
  linear_model <- linear_reg() # Modelo original es lineal
  
  work_flow <- workflow() %>%  #Creación de los workflows
    add_recipe(x) %>% 
    add_model(linear_model) #Add recipes to tidymodels
  
  fit_model <- work_flow %>% #Fit models
    fit(data = df)
  
  fit_model
}

modelos <- lapply(modelos, function(x){fit_model(x, train)})

# Create loop to test
predict_from_workflow <- function(w, df_test=test) {
  predictions <- predict(w, new_data = df_test) %>% 
    bind_cols(df_test)
  
  predictions
}

rmse_from_predict <- function(pred) {
  test_rmse <- rmse(pred, truth = lw_hora, estimate = .pred)
  test_rmse$.estimate
}

predictions <- lapply(modelos, function (w){predict_from_workflow(w, test)})

rmse <- lapply(list_predictions, function (pred){rmse_from_predict(pred)})

rmse_df <- data.frame(list_rmse) 
rmse_df <- data.frame(
  'Workflow' = c('model1', 'model2', 'model3', 'model4', 'model5', 'model6', 'model7', 'model8', 'model9','model10', 'model10_2', 'model10_3','model10_4', 'model7_2', 'model7_3', 'model7_4'),
  'RMSE' = c('model1', 'model2', 'model3', 'model4', 'model5', 'model6', 'model7', 'model8', 'model9','model10', 'model10_2', 'model10_3','model10_4', 'model7_2', 'model7_3', 'model7_4')
)


# Elegir los modelos para menor RMSE:
workflows_loocv <- rmse_df$Workflow[order(rmse_df$RMSE)[1:2]]

## D. LOOCV:

loocv_model1 <- vector("numeric", length = nrow(Tabla_4))

for (i in seq_len(nrow(Tabla_4))) {
  loocv_data <- Tabla_4[-i, ]
  loocv_fit <- modelos[[2]] %>% fit(data = loocv_data)
  pred <- predict(loo_fit, new_data = slice(Tabla_4, i))$.pred
  loocv_model1[i] <- pred
  print(paste0("Iteration: ",i))
}

loocv_prediction <-bind_cols(Tabla_4$lw_hora, loocv_model1)

loocv_rmse <- rmse(temp, truth = ...1, estimate = ...2)

loocv_rmse$.estimate
