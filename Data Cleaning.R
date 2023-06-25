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

raw.short$gender %>% 
  recode(`1`= "männlich", `2` = "weiblich", `3`="divers") %>% 
  as.factor() -> raw.short$gender

raw.short$gender

# Qualitätskontrolle 

# Skalen berechnen

# Lösung abspeichern