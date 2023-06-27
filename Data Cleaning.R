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


raw.short$license[is.na(raw.short$license)] <- 5
raw.short$license %>%
  recode(`1`="PKW-Führerschein",
         `2`="Motorrad-Führerschein",
         `3`= "Kraftfahrzeug-Führerschein",
         `4`="Keinen Führerschein",
         `5`="keine Angabe",
         `12`="PKW-Führerschein & Motorrad-Führerschein",
         `13`="PKW-Führerschein & Kraftfahrzeug-Führerschein",
         `123`="PKW-Führerschein & Motorrad-Führerschein & Kraftfahrzeug-Führerschein") %>%
as.factor() -> raw.short$license



raw.short$education[is.na(raw.short$education)] <- 6
raw.short$education %>% 
  ordered(levels = c(1:6),
          labels = c(`1`="(noch) kein Schulabschluss",
                     `2`="Hauptschulabschluss / Volksschulabschluss",
                     `3`= "Realschulabschluss (Mittlere Reife)",
                     `4`="(Fach-)Abitur",
                     `5`="(Fach-)Hochschulabschluss",
                     `6`="keine Angabe")) -> raw.short$education

raw.short$occupation[is.na(raw.short$occupation)] <- 10
raw.short$occupation %>% 
  ordered(levels = c(1:10),
          labels = c(`1`="Schüler:in",
                     `2`="Auszubildende/-r",
                     `3`= "Student:in",
                     `4`="Voll berufstätig",
                     `5`="Teilweise berufstätig",
                     `6`="Rentner:in, Pensionär:in",
                     `7`="Arbeitslos / arbeitssuchend",
                     `8`= "Hausmann / Hausfrau und / oder versorge Kinder / pflegebedürftige Personen",
                     `9`= "Sonstiges",
                     `10`="Keine Angabe")) -> raw.short$occupation


raw.short$city_size[is.na(raw.short$city_size)] <- 6
raw.short$city_size %>% 
  ordered(levels = c(1:6),
          labels = c(`1`="< 5.000 Einwohner:innen",
                     `2`="5.000 - 20.000 Einwohner:innen",
                     `3`= "20.000 - 100.000 Einwohner:innen",
                     `4`="100.000 - 500.000 Einwohner:innen",
                     `5`="> 500.000 Einwohner:innen",
                     `6`="keine Angabe")) -> raw.short$city_size




raw.short$duration_license[is.na(raw.short$duration_license)] <- 5
raw.short$duration_license %>% 
  ordered(levels = c(1:5),
          labels = c(`1`="Seit weniger als einem Jahr",
                     `2`="Zwischen 1-5 Jahren",
                     `3`= "Zwischen 5-10 Jahren",
                     `4`="Seit mehr als 10 Jahren",
                     `5`="keine Angabe")) -> raw.short$duration_license


raw.short$`km_per year`[is.na(raw.short$`km_per year`)] <- 6
raw.short$duration_license %>% 
  ordered(levels = c(1:6),
          labels = c(`1`="bis 5.000 km",
                     `2`="5.001 km bis 10.000 km",
                     `3`= "10.001 km bis 15.000 km",
                     `4`="15.001 km bis 20.000 km",
                     `5`="mehr als 20.001 km",
                     `6`="keine Angabe")) -> raw.short$`km_per year`

raw.short$frequency[is.na(raw.short$frequency)] <- 6
raw.short$duration_license %>% 
  ordered(levels = c(1:6),
          labels = c(`1`="Täglich",
                     `2`="Mehrmals in der Woche",
                     `3`= "Mehrmals im Monat",
                     `4`="Mehrmals im Jahr",
                     `5`="Noch seltener / nie",
                     `6`="keine Angabe")) -> raw.short$frequency


# Qualitätskontrolle 
median(raw.short$`Duration (in seconds)`)

median(raw.short$`Duration (in seconds)`) / 3

speederlimit <- median(raw.short$`Duration (in seconds)`) / 3
raw.short <- filter(raw.short, `Duration (in seconds)` > speederlimit)


# Skalen berechnen

# Lösung abspeichern