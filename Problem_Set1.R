# Taller 1: Big Data y Machine Learning para Econonomia Aplicada
# Cargamos los paquetes

#install.packages("rvest")
#install.packages("xml2")
#install.packages("purrr")
#install.packages("writexl")
#install.packages("tidyverse")
#install.packages("kableExtra")
#install.packages("knitr")
#install.packages("flextable")
#install.packages("officer")
#install.packages("ggplot2")
#install.packages("boot")
#install.packages("lmtest")
#install.packages("car")
#install.packages("dplyr")
#install.packages("xtable")
#install.packages("DT")
#install.packages("jpeg")

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
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

#Importar data de los demas Chunk
urls <- c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")

Import_data <- function(urls) {
  webpage <- read_html(urls)
  
  # Realiza el web scraping según la estructura de la página
  table2 <- webpage %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  
  return(table2)
  
}

Tablas <- map(urls, Import_data)
dataframes <- map(Tablas, as.data.frame)
Tabla_Total <- bind_rows(dataframes)

# Limpieza de datos 

#Tabla con datos de los individuos mayores a 18 años
Tabla_2 <- Tabla_Total %>% filter(age > 18)

#Variable Dependiente: y_salary_m_hu
# Renormbrar esta variable

Tabla_2 <- Tabla_2 %>% rename(w_hora=y_salary_m_hu)

# determinar que % de NA tiene w_hora
Porc_NA <- mean(is.na(Tabla_2$w_hora))* 100

cat("El porcentaje de NA en la columna 'w_hora' es:",Porc_NA, "%\n")

# La variable w_hora contiene un % de NA=59.3207, este % se adoptará como criterio para  
# eliminar Las Columnas que tienen un % NA > 59.3207

#Se eliminar las filas con NA para la columna w_hora
Tabla_3 <- Tabla_2[!is.na(Tabla_2$w_hora),]

#Conservar aquellas columnas cuyo % de NA es menor al 30%
Criterio <- 30

Tabla_4 <- Tabla_3 %>%
  select(where(~(sum(is.na(.)) / length(.)) * 100 <= Criterio))

# Identificar las columnas con NA
Col_NA <- colnames(Tabla_4)[colSums(is.na(Tabla_4)) > 0]
cat("Las siguientes columnas contienen valores NA:\n")
cat(Col_NA, "\n")

# determinar que % de NA tiene maxEducLevel
Porc_NA1 <- mean(is.na(Tabla_4$maxEducLevel))* 100

# Calcular la moda de la variable maxEducLevel e Imputarla en esta columna
Moda_MLE <- as.character(names(sort(table(Tabla_4$maxEducLevel), decreasing = TRUE)[1]))
Tabla_4$maxEducLevel[is.na(Tabla_4$maxEducLevel)] <- Moda_MLE

# Seleccionar las variables de Remuneraciones Monetarias Extras
Tabla_4 <- Tabla_4 %>%
  mutate(RME = p6510s1 + p6545s1+p6580s1+p6630s1a1+p6630s2a1+p6630s3a1+p6630s4a1
         +p6630s6a1)
summary(Tabla_4$RME)
Porc0_RME <- (sum(Tabla_4$RME == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RME))) * 100

# Seleccionar las variables de Remuneraciones en Especie
Tabla_4 <- Tabla_4 %>%
  mutate(RES = p6585s1a1+p6585s2a1+p6585s3a1+p6585s4a1+p6590s1+p6600s1+p6610s1+p6620s1)
summary(Tabla_4$RES)
Porc0_RES <- (sum(Tabla_4$RES == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RES))) * 100

# Imputar el valor 72 a los valores > 72 y 24 a los valores < 24
Tabla_4$totalHoursWorked[Tabla_4$totalHoursWorked > 72 | Tabla_4$totalHoursWorked < 24] <- 72

# Analisis Descriptivo de los Datos:

#Selección de Variables a Analizar
Tabla_4 <- Tabla_4 %>%
  select(estrato1, sex, age,p6210, p6210s1, p6240, p6426,RES, RME, 
         hoursWorkUsual, p6870, p6920,pet, impa, isa, ie, iof1, iof2, iof3h, iof3i, 
         iof6,ingtotob, ingtot, depto, maxEducLevel, college, cotPension, wap, ocu, 
         dsi, pea, inac, totalHoursWorked, formal, informal, cuentaPropia, microEmpresa, 
         sizeFirm, y_salary_m,  w_hora, y_ingLab_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)

# Renombrar las variables
Tabla_4 <- Tabla_4 %>% rename(Educ=p6210)
Tabla_4 <- Tabla_4 %>% rename(Edad=age)
Tabla_4 <- Tabla_4 %>% rename(Sexo=sex)
Tabla_4 <- Tabla_4 %>% rename(n_esc_apr=p6210s1)
Tabla_4 <- Tabla_4 %>% rename(c_ocup=p6240)
Tabla_4 <- Tabla_4 %>% rename(exp=p6426)
Tabla_4 <- Tabla_4 %>% rename(n_hsem=hoursWorkUsual)
Tabla_4 <- Tabla_4 %>% rename(Tamaño_empresa=p6870) # empleados por empresa
Tabla_4 <- Tabla_4 %>% rename(c_cotiz=p6920)
Tabla_4 <- Tabla_4 %>% rename(n_ito=ingtotob)
Tabla_4 <- Tabla_4 %>% rename(Horas_trabajadas=totalHoursWorked)
Tabla_4 <- Tabla_4 %>% rename(n_it=ingtot)
Tabla_4 <- Tabla_4 %>% rename(c_mne=maxEducLevel)
Tabla_4 <- Tabla_4 %>% rename(Estrato=estrato1)

# Revisión de la estadistica descriptiva de algunas variables

Tabla_Stat <- Tabla_4  %>% select(Horas_trabajadas, 
                                  Educ, 
                                  Edad, 
                                  exp,
                                  Tamaño_empresa,
                                  Estrato)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Variables Included in the Selected Data Set")


Tabla_4 <- Tabla_4 %>%
  mutate(Nivel_Educativo = case_when(
    Educ == 1 ~ "Ninguno",
    Educ == 2 ~ "Preescolar",
    Educ == 3 ~ "Básica primaria",
    Educ == 4 ~ "Básica secundaria",
    Educ == 5 ~ "Media",
    Educ == 6 ~ "Superior o universitaria",
    Educ == 9 ~ "No sabe",
    TRUE ~ as.character(Educ)  # Si no coincide con ninguna condición, mantener el valor original como texto
  ))


# Crear una tabla de contingencia segmentada por nivel educativo
Tabla_Educ <- Tabla_4 %>%
  group_by(Nivel_Educativo) %>%
  summarize(
    Relative_Salary = (sum(w_hora) / sum(Tabla_4$w_hora)) * 100,  # Relación relativa de ingreso en porcentaje
    Relative_Salary = (sum(y_salary_m) / sum(Tabla_4$y_salary_m)) * 100,  # Relación relativa de salario en porcentaje
    Relative_IngresoT = (sum(n_it) / sum(Tabla_4$n_it)) * 100  # Relación relativa de ingreso total en porcentaje
  )
colnames(Tabla_Educ) <- c("Nivel Educativo", "Salario por hora", "Salario Mensual","Ingreso Total")
Tabla_Educ

# Dar formato a la tabla con kableExtra
Tabla_Educ <- Tabla_Educ %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_Educ

#### Analisis descriptivo del ingreso
Salario <- Tabla_4%>%
  select(y_salary_m,
         w_hora, y_ingLab_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)
skim(Salario)


#### Analisis descriptivo del salario
Ingresos <- Tabla_4%>%
  select(impa, isa, ie, iof1, iof2, iof3h, iof3i, iof6,n_ito,n_it,RES,RME)
skim(Ingresos)

# Crear una variable dummy del sector, formal o informal
Tabla_4$Sector <- ifelse(Tabla_4$formal == 1 & Tabla_4$informal == 0, 1, 0)

## Regression de w_hora en función de las variables seleccionadas:
# Siguiendo el Modelo de Mincer
# Transformación logaritmica de w_hora

Tabla_4$lw_hora <- log(Tabla_4$w_hora)

Tabla_4$exp2 <- Tabla_4$exp^2
Tabla_4$Edad2 <- Tabla_4$Edad^2
Tabla_4$exp2 <- Tabla_4$exp^2
# Vector de Covariables

#Regresión por MCO
mod <- lm.fit <- lm(lw_hora ~ Educ + exp + exp2 + Sexo + Edad + Horas_trabajadas + Tamaño_empresa + Sector + Estrato, data = Tabla_4)
summary(lm.fit)

stargazer(mod,type="text", omit.stat=c("ser","f","adj.rsq"))

Tabla_5 <- Tabla_4

Tabla_5 <- Tabla_5 %>%
  mutate(Sexo = ifelse(Sexo == 0, "Mujer", "Hombre"))

#Tabla_4$Sexo <- ifelse(Tabla_4$Sexo == 0, "Mujer", "Hombre")
Tabla_Sexo <- Tabla_5 %>%
  group_by(Sexo) %>%
  summarize(Ingreso_Promedio = mean(w_hora),
            Salario_Promedio = mean(y_salary_m),
            Ingreso_Laboral = mean(y_ingLab_m_ha),
            Ingreso_Total = mean(y_total_m))
colnames(Tabla_Sexo) <- c("Sexo", "Salario Real por Hora","Salario Nominal Mensual","Ingreso Laboral","Ingreso Total" )
Tabla_Sexo

# Dar formato a la tabla con kableExtra
Tabla_Sexo <- Tabla_Sexo %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_Sexo

######---------------REGRESION 2 ---------------------------------------##########

#Ajusta el modelo de regresión no lineal:
Mod2 <- lm(lw_hora ~ Edad + Edad2, data = Tabla_4)
stargazer(Mod2, type="text", omit.stat=c("ser","f","adj.rsq"))

library(boot)
Edad_Mod2 <-function(data,index){
  
  Mod2 <-lm(lw_hora ~ Edad + Edad2, data = Tabla_4, subset = index)
  
  Coefs<-Mod2$coefficients
  
  b1<-Coefs[1] 
  b2<-Coefs[2]
  b3<-Coefs[3] 
  
  Edad_Maxima <- round(b2/(-2*b3)) #La eddad maxima es 46 años
  
  return(Edad_Maxima)
}

set.seed(123)
Res_Edad <- boot(data=Tabla_4, Edad_Mod2,R=1000)
Res_Edad

Tabla_Edad_Bootstrap = Res_Edad$t

Interval_ <- quantile(Tabla_Edad_Bootstrap,c(0.025,0.975))
print(Interval_)

# Secuencia de edades para el gráfico
Edad_seq <- seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 1000)

# Perfil estimado de ingresos usando los coeficientes
Perfil_Ingreso <- exp(predict(Mod2, newdata = data.frame(Edad = Edad_seq, Edad2 = Edad_seq^2)))

# Calcular los intervalos de confianza para el perfil de ingresos
Interv_Conf <- predict(Mod2, newdata = data.frame(Edad = Edad_seq, Edad2 = Edad_seq^2), interval = "confidence")

# Extraer los límites inferior (lwr) y superior (upr) de los intervalos de confianza
lwr <- exp(Interv_Conf[, "lwr"])
upr <- exp(Interv_Conf[, "upr"])

library(jpeg)
jpeg(file = "C:/Output R/Taller1/Graph1.jpeg", width = 900, height = 600)
# Crear el gráfico
plot(Edad_seq, Perfil_Ingreso, type = "l", xlab = "Edad", ylab = "Salario por Hora Estimado", main = "Perfil Estimado de Edad-Ingresos")

# Agregar líneas para los intervalos de confianza
lines(Edad_seq, lwr, col = "red", lty = 2)
lines(Edad_seq, upr, col = "red", lty = 2)

# Puedes calcular la edad máxima encontrando la edad donde el ingreso es máximo
Edad_Max <- Edad_seq[which.max(Perfil_Ingreso)]
text(Edad_Max, max(Perfil_Ingreso), "Edad Maxima", pos = 3, col = "blue")
dev.off() 

###### SEGUNDO PARTE
#(a) Begin by estimating and discussing the unconditional wage gap:
# log(w) = 1 + 2Female + u (3)
#where Female is an indicator that takes one if the individual in the sample is
#identied as female.

### Crear la variable mujer
Tabla_4$mujer <- ifelse(Tabla_4$Sexo == 0, 1, 0)
brecha_salarial <- lm(lw_hora ~ mujer, data = Tabla_4)
stargazer(brecha_salarial, type="text", digits=5, omit.stat=c("ser","f","adj.rsq"))

# Regresion log(w_hora) sobre las demas variables
Reg_bs1<-lm(lw_hora ~ mujer + Edad +Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data =Tabla_4)
stargazer(Reg_bs1,type="text",digits=5, omit.stat=c("ser","f","adj.rsq"))

#1) Regresion var=mujer sobre las demas variables (Reg1)
Tabla_4 <-Tabla_4 %>% mutate(mujer_Resid=lm(mujer~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas,Tabla_4)$residuals)

#2) Regresion log(w_hora) sobre las demas variables excepto mujer (Reg2)
Tabla_4 <-Tabla_4 %>% mutate(lw_hora_Resid=lm(lw_hora~ Edad + Edad2+ Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas,Tabla_4)$residuals) #Residuals of mpg~foreign

#3) Regresion de los residuos de la Reg1 sobre los residuos de la Reg2
Reg_bs2<-lm(lw_hora_Resid ~ mujer_Resid,Tabla_4)
stargazer(Reg_bs1,Reg_bs2,type="text",digits=5, omit.stat=c("ser","f","adj.rsq")) 

B <- 1000  

# Matrices para almacenar los resultados de bootstrap_modl1 y bootstrap_model2
coef_bootstrap_modl1 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de bootstrap_modl1
coef_bootstrap_model2 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de bootstrap_model2

# Realizar el proceso de Bootstrap
for (i in 1:B) {
  # Crear una muestra bootstrap sin valores NA
  sample_indices <- sample(1:nrow(Tabla_4), replace = TRUE)
  sample_data <- Tabla_4[sample_indices, ]
  
  
  # Ajustar brecha_salarial1 en la muestra bootstrap
  bootstrap_modl1 <- lm(lw_hora ~ mujer + Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)
  
  # Ajustar brecha_salarial2 en la muestra bootstrap
  sample_data<- sample_data %>%
    mutate(mujerResidF  = lm(mujer ~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)$residuals)
  
  sample_data<- sample_data %>%
    mutate(lw_horaResidF = lm(lw_hora ~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)$residuals)
  
  bootstrap_model2 <- lm(lw_horaResidF ~ mujerResidF, data = sample_data)
  
  # Almacenar los coeficientes estimados y errores estándar
  coef_bootstrap_modl1[i, 1] <- coef(bootstrap_modl1)["mujer"]
  coef_bootstrap_modl1[i, 2] <- summary(bootstrap_modl1)$coefficients["mujer", "Std. Error"]
  
  coef_bootstrap_model2[i, 1] <- coef(bootstrap_model2)["mujerResidF"]
  coef_bootstrap_model2[i, 2] <- summary(bootstrap_model2)$coefficients["mujerResidF", "Std. Error"]
}

# Calcular los intervalos de confianza Bootstrap para los coeficientes
interval_modl1 <- quantile(coef_bootstrap_modl1, c(0.025, 0.975))
interval_model2 <- quantile(coef_bootstrap_model2, c(0.025, 0.975))

# Imprimir los intervalos de confianza para bootstrap_modl1
cat("Intervalo de confianza Bootstrap para el coeficiente de mujer en bootstrap_modl1:", interval_modl1[1], "-", interval_modl1[2], "\n")

# Imprimir los intervalos de confianza para bootstrap_model2
cat("Intervalo de confianza Bootstrap para el coeficiente de mujerResidF en bootstrap_model2:", interval_model2[1], "-", interval_model2[2], "\n")

# Visualizar histogramas de coeficientes estimados
# Establecer una ventana gráfica dividida
par(mfrow = c(1, 2))  # 1 fila, 2 columnas


# Dibujar el histograma para bootstrap_modl1
Hist_1 <- hist(coef_bootstrap_modl1[, 1], main = "Histograma de Coeficientes - bootstrap_modl1", xlab = "Coeficiente")
#jpeg(file = "Hist_1.jpeg", width = 800, height = 600)
plot(Hist_1)
dev.off()  # Finalizar la exportación
#ruta_exportacion <- "C:/Output R/Taller1/Hist1.jpeg"

# Dibujar el histograma para bootstrap_model2
Hist_2 <- hist(coef_bootstrap_model2[, 1], main = "Histograma de Coeficientes - bootstrap_model2", xlab = "Coeficiente")


# Restaurar la configuración gráfica original
par(mfrow = c(1, 1))  # Restaurar a 1 fila, 1 columna

print(coef_bootstrap_modl1)
print(coef_bootstrap_model2)

