# Pakete installieren

#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("esquisse")
#install.packages("shiny")

# Pakete aktivieren
library(hcictools)
library(tidyverse)
rwthcolor <- hcictools::rwth.colorpalette()
library(dplyr)
library(ggplot2)
library(shiny)
library(psych)
library(psych)
library(knitr)

# Alter

summary(data_combined$age) # Kurze Version
describe(data_combined$age) #Bessere Darstellung

shapiro_test_result <- shapiro.test(data_combined$age) # das prüft nochmal extra die Normalverteiling der Daten 
print(shapiro_test_result)
# --> nicht normalverteilt

# Geschlecht

summary(data_combined$gender)

# Bildungsabschluss

summary(data_combined$education)

# Beschäftigung

summary(data_combined$occupation)

# Art des Autos

table(data_combined$car_type)

# Vorwissen Elektroautos

summary(data_combined$prior_knowledge)


