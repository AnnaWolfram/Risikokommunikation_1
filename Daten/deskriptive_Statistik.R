# Pakete installieren

#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("esquisse")
#install.packages("shiny")
# Installiere 'scales' Paket, falls es noch nicht installiert ist
#install.packages("scales")

# Lade benötigte Pakete
library(hcictools)
library(tidyverse)
rwthcolor <- hcictools::rwth.colorpalette()
library(dplyr)
library(ggplot2)
library(shiny)
library(psych)
library(knitr)
library(scales) # Das muss auch installiert werden

# Alter
summary(data_combined$age)
describe(data_combined$age)
shapiro_test_result <- shapiro.test(data_combined$age)
print(shapiro_test_result)

# Geschlecht
summary(data_combined$gender) # Ganze Zahlen
gender_percent <- prop.table(table(data_combined$gender)) * 100
gender_percent <- round(gender_percent, 2)
gender_percent    # Prozentangaben

# Bildungsabschluss
summary(data_combined$education)
education_percent <- prop.table(table(data_combined$education)) * 100
education_percent <- round(education_percent, 2)
education_percent

# Beschäftigung
summary(data_combined$occupation)
occupation_percent <- prop.table(table(data_combined$occupation)) * 100
occupation_percent <- round(occupation_percent, 2)
occupation_percent

# Art des Autos
table(data_combined$car_type)
car_type_percent <- prop.table(table(data_combined$car_type)) * 100
car_type_percent <- round(car_type_percent, 2)
car_type_percent

# Vorwissen Elektroautos
summary(data_combined$prior_knowledge)
prior_knowledge_percent <- prop.table(table(data_combined$prior_knowledge)) * 100
prior_knowledge_percent <- round(prior_knowledge_percent, 2)
prior_knowledge_percent



