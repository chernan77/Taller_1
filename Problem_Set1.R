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
