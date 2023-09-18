
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
model8 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa, data = train)  %>%
  step_dummy(all_factor_predictors())
model9 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_dummy(all_factor_predictors())
model10 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector +Educ + Size_empresa + exp , data = train)  %>%
  step_dummy(all_factor_predictors())


model11 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Estrato) %>%
  step_dummy(all_factor_predictors())

model12 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Sector) %>%
  step_dummy(all_factor_predictors())


model13 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())

Tabla_4$Horas_trabajadas2 <- Tabla_4$Horas_trabajadas^2 # ConstrucciC3n de la variable Horas trabajadas al cuadrado

model14 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp + oficio, data = train)  %>%
  step_interact(terms = ~ Sexo:Oficio + Oficio:Educ) %>%
  step_dummy(all_factor_predictors())

model15 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp + oficio + Horas_trabajadas2 , data = train)  %>%
  step_interact(terms = ~ Sexo:Oficio + Oficio:Educ) %>%
  step_dummy(all_factor_predictors())

#List models
modelos<-list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14)


# Create loop to fit with workflows

fit_model <- function(x, df=train) {
  linear_model <- linear_reg() # Modelo original es lineal
  
  work_flow <- workflow() %>%  #Creaci??n de los workflows
    add_recipe(x) %>% 
    add_model(linear_model) 
  
  fit_final_model <- work_flow %>% #Fit models
    fit(data = df)
  
  fit_final_model
}

workflows_finales<-lapply(modelos, function(x){fit_model(x, train)})

# Create loop to test

predicciones_wf<- function(w, df_test=test) {
  predicciones_finales <- predict(w, new_data = df_test) %>% 
    bind_cols(df_test) %>% 
  
  predicciones_finales
}

rmse_predicciones <- function(pred) {
  rmse_test <- rmse(pred, truth = lw_hora, estimate = .pred)
  rmse_test$.estimate
}

predictions_totales <- lapply(modelos, function (w){predicciones_wf(w, test)})
rmse_totales <- lapply(predictions_totales, function (pred){rmse_predicciones(pred)})



rmse_df <- data.frame(rmse_totales) 

# Elegir los modelos para menor RMSE:
workflows_loocv <- rmse_df$Workflow[order(rmse_df$RMSE)[1:2]]

## D. LOOCV:

loocv_model1 <- vector("numeric", length = nrow(Tabla_4))
loocv_model2 <- vector("numeric", length = nrow(Tabla_4))

for (i in seq_len(nrow(Tabla_4))) {
  loocv_data <- Tabla_4[-i, ]
  loocv_fit <- modelos[[2]] %>% fit(data = loocv_data)
  pred <- predict(loocv_fit, new_data = slice(Tabla_4, i))$.pred
  loocv_model1[i] <- pred
  print(paste0("Iteration: ", i))
}

loocv_prediction <- data.frame(lw_hora = Tabla_4$lw_hora, loocv_model1 = loocv_model1)

loocv_rmse <- rmse(data = loocv_prediction, truth = lw_hora, estimate = loocv_model1)

loocv_rmse

