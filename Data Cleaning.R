# Data Cleaning

#Mehr als 50 Spalten sehen

# Pakete laden

#install.packages("readxl")

library(tidyverse)
library(psych)
library(readxl)
source("qualtricshelpers.R")

# Daten einlesen

Rohdaten <- "Daten/Rohdaten_25.06.csv"
raw <- load_qualtrics_csv(Rohdaten)


# Zeilen entfernen

raw <- filter(raw, Progress >= 99)


# Spalten entfernen

raw.short <- raw[,c(-1:-4, -7:-17, -131:-132)]

#Ab hier!
generate_codebook(raw.short, Rohdaten, "Daten/codebook.csv")

codebook <- read_codebook("Daten/codebook_final.csv")

names(raw.short) <- codebook$variable

# Richtige Datentypen zuordnen ----

raw.short$age

raw.short[176,]$age = "24"

raw.short$age <- as.numeric(raw.short$age)

raw.short$gender[is.na(raw.short$gender)] <- 4

raw.short$gender %>% 
  recode(`1`= "männlich", `2` = "weiblich", `3`="divers", `4`="keine Angabe") %>% 
  as.factor() -> raw.short$gender

raw.short$education[is.na(raw.short$education)] <- 6
raw.short$education %>%
  recode(`1`="(noch) kein Schulabschluss",
         `2`="Hauptschulabschluss / Volksschulabschluss",
         `3`= "Realschulabschluss (Mittlere Reife)",
         `4`="(Fach-)Abitur",
         `5`="(Fach-)Hochschulabschluss",
         `6`="keine Angabe")

raw.short$occupation[is.na(raw.short$occupation)] <- 10
raw.short$occupation%>%
  recode(`1`="Schüler:in",
         `2`="Auszubildende/-r",
         `3`= "Student:in",
         `4`="Voll berufstätig",
         `5`="Teilweise berufstätig",
         `6`="Rentner:in, Pensionär:in",
         `7`="Arbeitslos / arbeitssuchend",
         `8`= "Hausmann / Hausfrau und / oder versorge Kinder / pflegebedürftige Personen",
         `9`= "Sonstiges",
         `10`="Keine Angabe")



raw.short$branch %>% 
  recode(`1`="Forschung und Entwicklung",
         `2`="Finanzen, Versicherungen und Immobilien",
         `3`="Lehre", 
         `4`="Medizin (Pharma und Gesundheit",
         `5`="Dienstleistungen und Handwerk",
         `6`="Freizeit", 
         `7`="Gesellschaft", 
         `8`="Agrarwissenschaft", 
         `9`="Technik", 
         `10`="Sonstiges") %>% 
  as.factor() -> raw.short$branch

# Qualitätskontrolle 

# Skalen berechnen

# Lösung abspeichern