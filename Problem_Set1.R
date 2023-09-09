# Taller 1: Big Data y Machine Learning para Econonomia Aplicada
# Cargamos los paquetes

install.packages("rvest")
install.packages("xml2")
install.packages("purrr")
install.packages("writexl")
install.packages("tidyverse")
install.packages("kableExtra")
install.packages("knitr")
install.packages("flextable")
install.packages("officer")
install.packages("ggplot2")
install.packages("boot")
install.packages("lmtest")
install.packages("car")
install.packages("dplyr")
install.packages("xtable")
install.packages("DT")
install.packages("jpeg")

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
  
  # Realiza el web scraping del sitio web donde esta la información
  table2 <- webpage %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  
  return(table2)
  
}

Tablas <- map(urls, Import_data)
dataframes <- map(Tablas, as.data.frame)
Tabla_Total <- bind_rows(dataframes)

