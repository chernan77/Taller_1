
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
install.packages("jpeg")
install.packages("openxlsx")
install.packages("readxl")
install.packages("cowplot")
install.packages("modeest")


library(modeest)
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
library(readxl)
library(openxlsx)
library(cowplot)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

#Importar data
#####---Encuesta GEIH de Bogota from the 2018 "Medición de Pobreza Monetaria y Desigualdad Report---######
# 2. Data: 

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

####------------Limpieza de Datos----------------------------------####
####---------------------------------------------------------------###


Tabla_2 <- Tabla_Total %>% filter(age > 18) #Excluir datos de individuos menores a 18 años

Tabla_2 <- Tabla_2 %>% filter(dsi == 0) # Excluir los desempleados
Tabla_2 <- Tabla_2 %>% filter(pea == 1) # Excluir la población económicamente inactiva

Tabla_2 <- Tabla_2 %>% rename(w_hora=y_salary_m_hu) # Renombrar la variable Dependiente

Porc_NA <- mean(is.na(Tabla_2$w_hora))* 100 # determinar que % de NA tiene w_hora

cat("La variable w_hora contiene un % de NA=59.3207' es:",Porc_NA, "%\n")

Tabla_3 <- Tabla_2[!is.na(Tabla_2$w_hora),] #Se eliminarán las filas con NA para columna w_hora

#Conservar aquellas columnas cuyo % de NA es menor al 30%
Criterio <- 30

Tabla_4 <- Tabla_3 %>%
  select(where(~(sum(is.na(.)) / length(.)) * 100 <= Criterio))

# Identificar las columnas con NA
Col_NA <- colnames(Tabla_4)[colSums(is.na(Tabla_4)) > 0]
cat("Columnas que contienen valores NA:\n")
cat(Col_NA, "\n")


Porc_NA1 <- mean(is.na(Tabla_4$maxEducLevel))* 100 # % de NA en maxEducLevel

# Calcular la moda en maxEducLevel e Imputarla en esta columna
Moda_MLE <- as.character(names(sort(table(Tabla_4$maxEducLevel), decreasing = TRUE)[1]))
Tabla_4$maxEducLevel[is.na(Tabla_4$maxEducLevel)] <- Moda_MLE

# Seleccionar variables de Remuneraciones Monetarias Extras
Tabla_4 <- Tabla_4 %>%
  mutate(RME = p6510s1 + p6545s1+p6580s1+p6630s1a1+p6630s2a1+p6630s3a1+p6630s4a1
         +p6630s6a1)
summary(Tabla_4$RME)
Porc0_RME <- (sum(Tabla_4$RME == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RME))) * 100

# Seleccionar variables de Remuneraciones en Especie
Tabla_4 <- Tabla_4 %>%
  mutate(RES = p6585s1a1+p6585s2a1+p6585s3a1+p6585s4a1+p6590s1+p6600s1+p6610s1+p6620s1)
summary(Tabla_4$RES)
Porc0_RES <- (sum(Tabla_4$RES == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RES))) * 100

# Imputar el valor 72 a los valores > 72
Tabla_4$totalHoursWorked[Tabla_4$totalHoursWorked > 72] <- 72

#Imputar la moda para que aquellos individuos con exp > 65
Tabla_4$p6426[Tabla_4$p6426 > 65] <- 24

# Renombrar las variables
Tabla_4 <- Tabla_4 %>% rename(c_mne=p6210)
Tabla_4 <- Tabla_4 %>% rename(Edad=age)
Tabla_4 <- Tabla_4 %>% rename(Sexo=sex)
Tabla_4 <- Tabla_4 %>% rename(n_esc_apr=p6210s1)
Tabla_4 <- Tabla_4 %>% rename(c_ocup=p6240)
Tabla_4 <- Tabla_4 %>% rename(exp=p6426)
Tabla_4 <- Tabla_4 %>% rename(n_hsem=hoursWorkUsual)
Tabla_4 <- Tabla_4 %>% rename(Tamaño_empresa=p6870) # No. de Empleados por empresa
Tabla_4 <- Tabla_4 %>% rename(c_cotiz=p6920)
Tabla_4 <- Tabla_4 %>% rename(Horas_trabajadas=totalHoursWorked)
Tabla_4 <- Tabla_4 %>% rename(Ing_Total=ingtot)
Tabla_4 <- Tabla_4 %>% rename(Educ1=maxEducLevel)
Tabla_4 <- Tabla_4 %>% rename(Estrato=estrato1)
Tabla_4 <- Tabla_4 %>% rename(Ingreso_Mon_1=impa)
Tabla_4 <- Tabla_4 %>% rename(Ingreso_Mon_2=isa)
Tabla_4$Educ <- as.integer(Tabla_4$Educ1)

# Analisis Descriptivo de los Datos:

#Selección de Variables a Analizar
Tabla_4 <- Tabla_4 %>%
  select(Estrato, Sexo, Edad, Educ, n_esc_apr, exp,RES, RME, 
         n_hsem, Tamaño_empresa, c_cotiz,pet, Ingreso_Mon_1, Ingreso_Mon_2,
         Ing_Total, depto, c_mne, college, cotPension,
         dsi, pea, inac, Horas_trabajadas, formal, informal, cuentaPropia, microEmpresa, 
         sizeFirm, y_salary_m,  w_hora, y_ingLab_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)

# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- Tabla_4  %>% select(Horas_trabajadas, 
                                  Educ, 
                                  Edad, 
                                  exp,
                                  Tamaño_empresa,
                                  Estrato)
stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Descriptivas Variables Seleccionadas")


Tabla_4 <- Tabla_4 %>%
  mutate(Nivel_Educativo = case_when(
    Educ == 1 ~ "Ninguna",
    Educ == 2 ~ "Preescolar",
    Educ == 3 ~ "Primaria Incompleta",
    Educ == 4 ~ "Primaria Completa",
    Educ == 5 ~ "Secundaria Incompleta",
    Educ == 6 ~ "Secundaria Completa",
    Educ == 7 ~ "Universitaria",
    TRUE ~ as.character(Educ) 
  ))

# Tabla segmentada por nivel educativo
Tabla_Educ <- Tabla_4 %>%
  group_by(Nivel_Educativo) %>%
  summarize(
    Relative_W_hora = (sum(w_hora) / sum(Tabla_4$w_hora)) * 100,  
    Relative_Salary = (sum(y_salary_m) / sum(Tabla_4$y_salary_m)) * 100, 
    Relative_IngresoT = (sum(Ing_Total) / sum(Tabla_4$Ing_Total)) * 100 
  )
colnames(Tabla_Educ) <- c("Nivel Educativo", "Salario por hora", "Salario Mensual","Ingreso Total")
Tabla_Educ

# Formato a la tabla con kableExtra
Tabla_Educ <- Tabla_Educ %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_Educ

#### Analisis descriptivo del ingreso
Ingresos <- Tabla_4%>%
  select(y_salary_m, Ingreso_Mon_1, Ingreso_Mon_2, Ing_Total, RES, RME,
         w_hora, y_ingLab_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)
colnames(Ingresos) <- c("Salario Mensual", "Ingreso Monetario 1","Ingreso Monetario 2",
                        "Ingreso Total", "Rem. Adicionales en Especie", "Rem. Monetarias Adicionales",
                        "Salario por hora", "Ingreso Laboral Mensual", "Ingreso Laboral por Hora", "Ingreso Salarial Total",
                        "Ingreso Total por Hora")
skim(Ingresos)


# Crear rangos de edades
Tabla_4 <- Tabla_4 %>%
  mutate(RangoEdad = cut(Edad, breaks = c(18, 25, 30, 40, 50, 60, 90),
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-90")))

# Resumir ingresos por rango de edades
Tabla_ingresos_edad <- Tabla_4 %>%
  group_by(RangoEdad) %>%
  summarise(Salario_Hora = mean(w_hora),
            Sal_Mensual = mean(y_salary_m),
            Ingreso_Total = mean(Ing_Total),
            Nivel_Educ = round(mean(Educ)),
            Estrat = round(mlv(Estrato)),
            Tam_empresa = round(mlv(Tamaño_empresa)),
            CantidadPersonas = n())
colnames(Tabla_ingresos_edad) <- c("Rango de Edad", "Salario por hora", "Salario Mensual","Ingreso Total","Nivel Educativo Medio","Estrato","Tamaño Empresas", "Numero de Individuos")
Tabla_ingresos_edad

# Dar formato a la tabla con kableExtra
Tabla_ingresos_edad <- Tabla_ingresos_edad %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_ingresos_edad

## Regresión de log(w_hora) en función de las variables seleccionadas:
## Modelo de Mincer

# Transformación de variables para el Modelo
Tabla_4$Sector <- ifelse(Tabla_4$formal == 1 & Tabla_4$informal == 0, 1, 0) # dummy del sector formal e informal
Tabla_4$lw_hora <- log(Tabla_4$w_hora) # Transformación Logaritmica del salario por hora
Tabla_4$exp2 <- Tabla_4$exp^2  # Construcción de la variable experiencia al cuadrado
Tabla_4$Edad2 <- Tabla_4$Edad^2 # Construcción de la variable Edad al cuadrado

Media_w_hora <- mean(Tabla_4$w_hora)

Tabla_5 <- Tabla_4

Tabla_5 <- Tabla_5 %>%
  mutate(Sexo = ifelse(Sexo == 0, "Mujer", "Hombre"))

Tabla_Sexo <- Tabla_5 %>%
  group_by(Sexo) %>%
  summarize(Ingreso_Promedio = mean(w_hora),
            Salario_Promedio = mean(y_salary_m),
            Ingreso_Laboral = mean(y_ingLab_m_ha),
            Ingreso_Total = mean(y_total_m))
colnames(Tabla_Sexo) <- c("Sexo", "Salario Real por Hora","Salario Nominal Mensual","Ingreso Laboral","Ingreso Total" )
Tabla_Sexo <- as.data.frame(Tabla_Sexo)
#Tabla_S <- "C:/Output R/Taller_1/Taller_1/Tabla_S1.xlsx"
#write_xlsx(Tabla_Sexo, path = Tabla_S)

### Regresión 1
Mod <- lm.fit <- lm(lw_hora ~ Educ + exp + exp2 + Sexo + Edad + Horas_trabajadas + Tamaño_empresa + Sector + Estrato, data = Tabla_4)
Mod_stargazer <- stargazer(Mod,type="text", omit.stat=c("ser","f","adj.rsq"),  digits = 3)
Mod_stargazer <- as.data.frame(Mod_stargazer)
#Reg <- "C:/Output R/Taller_1/Taller_1/Mod_stargazer.xlsx"
#write_xlsx(Mod_stargazer, path = Reg )

# Significancia Económica parámetros
Coef <- Mod$coefficients
SE0 <- (exp(Coef)-1)*100
Sig_Economica <- round((SE0/Media_w_hora)*100, digits = 3)
Sig_Economica <- as.data.frame(Sig_Economica)
#T1 <- "C:/Output R/Taller_1/Taller_1/T1_Se.xlsx"
#write_xlsx(Sig_Economica, path = T1 )


######-------------------------REGRESION 2 ------------------------##########
##--------------------Bootstrap------------------------------###

#Ajusta el modelo de regresión no lineal:
Mod2 <- lm(lw_hora ~ Edad + Edad2, data = Tabla_4)
Mod2_stargazer <- stargazer(Mod2, type="text", omit.stat=c("ser","f","adj.rsq"))
Mod2_stargazer <- as.data.frame(Mod2_stargazer)
#Reg <- "C:/Output R/Taller_1/Taller_1/Mod2_stargazer.xlsx"
#write_xlsx(Mod2_stargazer, path = Reg )

# Significancia Económica parámetros
Coefs <- Mod2$coefficients
SE1 <- (exp(Coefs)-1)*100
Sig_Economica1 <- round(SE1/Media_w_hora*100, digits = 3)
Sig_Economica1 <- as.data.frame(Sig_Economica1)
#T2 <- "C:/Output R/Taller_1/Taller_1/T2_Se.xlsx"
#write_xlsx(Sig_Economica1, path = T2)

library(boot)
Edad_Mod2 <-function(data,index){
  
  Mod2 <-lm(lw_hora ~ Edad + Edad2, data = Tabla_4, subset = index)
  
  Coefs<-Mod2$coefficients
  
  b1<-Coefs[1] 
  b2<-Coefs[2]
  b3<-Coefs[3] 
  
  Edad_Maxima <- round(b2/(-2*b3)) #La edad máxima es 46 años
  
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

# Extraer los lC-mites inferior (lwr) y superior (upr) de los intervalos de confianza
lwr <- exp(Interv_Conf[, "lwr"])
upr <- exp(Interv_Conf[, "upr"])

Link_C <- "C:/Output R/Taller_1/Taller_1/graph1.jpeg"
jpeg(file = Link_C, width = 900, height = 600)
plot(Edad_seq, Perfil_Ingreso, type = "l", xlab = "Edad", ylab = "Salario por Hora Estimado", main = "Perfil Estimado de Edad-Ingresos")  # Vuelve a crear el grC!fico dentro de png()
lines(Edad_seq, lwr, col = "red", lty = 2)
lines(Edad_seq, upr, col = "red", lty = 2)
Edad_Max <- Edad_seq[which.max(Perfil_Ingreso)]
text(Edad_Max, max(Perfil_Ingreso), labels = "Max", pos = 1, col = "blue", cex = 1.5, srt = 0)
dev.off()  

###### --------------------SEGUNDO PARTE---------------------------############
#(a) Begin by estimating and discussing the unconditional wage gap:
# log(w) = b1+ b2Female + u (3)
#where Female is an indicator that takes one if the individual in the sample is

### Crear la variable mujer
Tabla_4$mujer <- ifelse(Tabla_4$Sexo == 0, 1, 0)
brecha_salarial <- lm(lw_hora ~ mujer, data = Tabla_4)
Modm_stargazer <- stargazer(brecha_salarial, type="text", digits=3, omit.stat=c("ser","f","adj.rsq"))
Modm_stargazer <- as.data.frame(Modm_stargazer)
Regm <- "C:/Output R/Taller_1/Taller_1/Modm_stargazer.xlsx"
write_xlsx(Modm_stargazer, path = Regm)

# Regresión log(w_hora) sobre las demas variables
Reg_bs1<-lm(lw_hora ~ mujer + Edad +Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data =Tabla_4)
stargazer(Reg_bs1,type="text",digits=3, omit.stat=c("ser","f","adj.rsq"))

#1) Regresion var=mujer sobre las demas variables (Reg1)
Tabla_4 <-Tabla_4 %>% mutate(mujer_Resid=lm(mujer~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas,Tabla_4)$residuals)

#2) Regresión log(w_hora) sobre las demas variables excepto mujer (Reg2)
Tabla_4 <-Tabla_4 %>% mutate(lw_hora_Resid=lm(lw_hora~ Edad + Edad2+ Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas,Tabla_4)$residuals) #Residuals of mpg~foreign

#3) Regresión de los residuos de la Reg1 sobre los residuos de la Reg2
Reg_bs2<-lm(lw_hora_Resid ~ mujer_Resid,Tabla_4)

Mod3_stargazer <- stargazer(Reg_bs1,Reg_bs2,type="text",digits=3, omit.stat=c("ser","f","adj.rsq")) 
Mod3_stargazer <- as.data.frame(Mod3_stargazer)
#Reg3 <- "C:/Output R/Taller_1/Taller_1/Mod3_stargazer.xlsx"
#write_xlsx(Mod3_stargazer, path = Reg3)


####-------------------------Bootstrap---------------------------------------#########

B <- 1000  

# Matrices para almacenar los resultados de bootstrap_modl1 y bootstrap_model2
coef_bootstrap_mod1 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de bootstrap_modl1
coef_bootstrap_mod2 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de bootstrap_model2

# Realizar el proceso de Bootstrap
set.seed(123)
boots_fwl <- for (i in 1:B) {
  # Crear una muestra bootstrap sin valores NA
  sample_indices <- sample(1:nrow(Tabla_4), replace = TRUE)
  sample_data <- Tabla_4[sample_indices, ]
  
  # Ajustar brecha_salarial1 en la muestra bootstrap
  bootstrap_mod1 <- lm(lw_hora ~ mujer + Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)
  
  # Ajustar brecha_salarial2 en la muestra bootstrap
  sample_data<- sample_data %>%
    mutate(mujerResid  = lm(mujer ~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)$residuals)
  
  sample_data<- sample_data %>%
    mutate(lw_horaResid = lm(lw_hora ~ Edad + Edad2 + Educ + exp + exp2 + Tamaño_empresa + Horas_trabajadas, data = sample_data)$residuals)
  
  bootstrap_mod2 <- lm(lw_horaResid ~ mujerResid, data = sample_data)
  
  # Almacenar los coeficientes estimados y errores estC!ndar
  coef_bootstrap_mod1[i, 1] <- coef(bootstrap_mod1)["mujer"]
  coef_bootstrap_mod1[i, 2] <- summary(bootstrap_mod1)$coefficients["mujer", "Std. Error"]
  
  coef_bootstrap_mod2[i, 1] <- coef(bootstrap_mod2)["mujerResid"]
  coef_bootstrap_mod2[i, 2] <- summary(bootstrap_mod2)$coefficients["mujerResid", "Std. Error"]
  
}

Mod4_stargazer <- stargazer(bootstrap_mod1,bootstrap_mod2,type="text",digits=3, omit.stat=c("ser","f","adj.rsq")) 
Mod4_stargazer <- as.data.frame(Mod4_stargazer)
#Reg4 <- "C:/Output R/Taller_1/Taller_1/Mod4_stargazer.xlsx"
#write_xlsx(Mod4_stargazer, path = Reg4)


# Calcular los intervalos de confianza Bootstrap para los coeficientes
interval_mod1 <- quantile(coef_bootstrap_mod1, c(0.025, 0.975))
interval_mod2 <- quantile(coef_bootstrap_mod2, c(0.025, 0.975))

# Imprimir los intervalos de confianza para bootstrap_modl1
cat("IC Bootstrap para el coeficiente de mujer en bootstrap_mod1:", interval_mod1[1], "-", interval_mod1[2], "\n")

# Imprimir los intervalos de confianza para bootstrap_model2
cat("IC Bootstrap para el coeficiente de mujerResid en bootstrap_mod2:", interval_mod2[1], "-", interval_mod2[2], "\n")

print(coef_bootstrap_mod1)
print(coef_bootstrap_mod2)

####################----------------------########################################
#############----------PERFIL DE INGRESOS POR HOMBRE Y MUJER---------------######


################################ Ejercicio 4.c  ###################################

# Inicializar un dataframe para almacenar los coeficientes de las regresiones

n <- 1000
# Vectores para almacenar las edades máximas y las diferencias en edades máximas
edadh <- numeric(n)
edadm <- numeric(n)
diff <- numeric(n)


for (i in 1:n) {
  # Genera una muestra bootstrap
  set.seed(123)
  sample_diff <- sample(1:nrow(Tabla_4), replace = TRUE)
  datos_diff <- Tabla_4[sample_diff, ]
  
  Mod5 <- lm(lw_hora ~ Edad + Edad2 + mujer + Edad*mujer + Edad2*mujer, data = datos_diff)
  coefs2 <- coef(Mod5)
  
  # El coeficiente correspondiente a 'Edad' en la regresiC3n lineal
  d1 <- coefs2['Edad']
  d2 <- coefs2['Edad2']
  d3 <- coefs2['Edad:mujer']
  d4 <- coefs2['Edad2:mujer']
  
  # Calcular el punto donde la derivada es igual a cero para hombres
  edad_maxh <- round(-d1 / (2 * d2))
  
  # Calcular el punto donde la derivada es igual a cero para mujeres
  edad_maxm <- round((-d1 - d3) / (2 * (d2 + d4)))
  
  # Calcular la diferencia en edades máximas entre hombres y mujeres
  difmax <- edad_maxh - edad_maxm
  
  # Almacena los resultados en los vectores
  edadh[i] <- edad_maxh
  edadm[i] <- edad_maxm
  diff[i] <- difmax
  
}

#Regresión 5
Mod5_stargazer <- stargazer(Mod5, type="text", omit.stat=c("ser","f","adj.rsq"),  digits = 3)
Mod5_stargazer <- as.data.frame(Mod5_stargazer)
#Reg5 <- "C:/Output R/Taller_1/Taller_1/Mod5_stargazer.xlsx"
#write_xlsx(Mod5_stargazer, path = Reg5)

# Significancia Económica parámetros
Coefs2 <- Mod5$coefficients
SE2 <- (exp(Coefs2)-1)*100
Sig_Economica2 <- round(SE2/Media_w_hora*100, digits = 3)
Sig_Economica2 <- as.data.frame(Sig_Economica2)
#T3 <- "C:/Output R/Taller_1/Taller_1/T3_Se.xlsx"
#write_xlsx(Sig_Economica2, path = T3 )


# Calcula el intervalo de confianza para la diferencia en edades mC!ximas
nivel_confianza <- 0.95
quantil_inf <- (1 - nivel_confianza) / 2
quantil_sup <- 1 - quantil_inf

intervalo_confianza_diferencia <- quantile(diff, c(quantil_inf, quantil_sup))

# Imprime el intervalo de confianza para la diferencia en edades máximas
print(intervalo_confianza_diferencia)

# Crear un nuevo conjunto de datos con las edades deseadas para hombres y mujeres
Edad_f = seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 1000)

# Predicciones utilizando el modelo para hombres y mujeres
predh <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 0)))
predm <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 1)))

# Convertir predh y predm en matrices

# Intervalos de confianza para las predicciones de ingresos de hombres y mujeres
int_hombres <-(predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 0), interval = "confidence"))
int_mujeres <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 1), interval = "confidence"))

# Gráfico de dispersión con colores por género y bandas de intervalo de confianza
edadh <- data.frame(
  Edad = seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 1000),
  Edad2 = seq(min(Tabla_4$Edad), max(Tabla_4$Edad)^2, length.out = 1000),
  mujer = 0)

edadm <- data.frame(
  Edad = seq(min(Tabla_4$Edad), max(Tabla_4$Edad),length.out = 1000),
  Edad2 = seq(min(Tabla_4$Edad), max(Tabla_4$Edad)^2, length.out = 1000),
  mujer = 1)

dfpredh <- data.frame(
  Edad = edadh$Edad,
  Prediccionesh = predh,
  Genero = 'Hombres'
)

dfpredm <- data.frame(
  Edad = edadm$Edad,
  Prediccionesm = predm,
  Genero = 'Mujeres'
  
)

graph_h <- ggplot(dfpredh, aes(x = Edad, y = Prediccionesh)) +
  geom_line(color = 'blue') +
  geom_ribbon(aes(ymin = int_hombres[, "lwr"], ymax = int_hombres[, "upr"]), fill = 'blue', alpha = 0.2) +
  labs(x = 'Edad', y = 'Perfil de Ingreso', title = expression(atop("Predicción del Perfil de Ingreso Hombres", ""))) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "gray"))+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray")) +
  coord_cartesian(xlim = c(15, 86), ylim = c(7.5, 9)) 
graph_h

graph_m <- ggplot(dfpredm, aes(x = Edad, y = Prediccionesm)) +
  geom_line(color = 'red') +
  geom_ribbon(aes(ymin = int_mujeres[, "lwr"], ymax = int_mujeres[, "upr"]), fill = 'red', alpha = 0.2) +
  labs(x = 'Edad', y = 'Perfil de Ingreso', title = expression(atop("Predicción del Perfil de Ingreso Mujeres", ""))) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "gray"))+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray")) +
  coord_cartesian(xlim = c(15, 86), ylim = c(7.1, 9)) 
graph_m

#Gráfica Conjunta Hombre-Mujer
plot_grid(graph_h, graph_m, ncol=2)



