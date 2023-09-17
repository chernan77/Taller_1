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
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

# Cargar base de datos