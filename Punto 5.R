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

# a. Split Sample:--------------------------------------------------------------
set.seed(123)
sample_split <- initial_split(Tabla_4, prop = .7)

# Create test and train data frames 
train <- training(sample_split)
test  <- testing(sample_split)

# b. Report and compare the predictive performance in terms of the RMSE:--------

# Modelos:
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
model8 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ+ informal + Sector + Tamaño_empresa, data = train)  %>%
  step_dummy(all_factor_predictors())
model10 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector + Tamaño_empresa + exp , data = train)  %>%
  step_dummy(all_factor_predictors())

model7_2 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Estrato) %>%
  step_dummy(all_factor_predictors())
model7_3 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:informal) %>%
  step_dummy(all_factor_predictors())
model7_3 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + informal, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())

#List models
modelos<-list(model1, model2, model3, model4, model5, model6, model7, model8, model10, model7_2, model7_3)

# Create loop to fit
fit_model <- function(x, df=train) {
  linear_model <- linear_reg()
  
  work_flow <- workflow() %>% 
    add_recipe(x) %>% 
    add_model(linear_model)
  
  fit_model <- work_flow %>% 
    fit(data = df)
  
  fit_model
}

list_workflows <- lapply(modelos, function(x){fit_model(x, train)})

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

list_predictions <- lapply(list_workflows, function (w){predict_from_workflow(w, test)})
list_rmse <- lapply(list_predictions, function (pred){rmse_from_predict(pred)})


rmse_df <- data.frame(list_rmse)
