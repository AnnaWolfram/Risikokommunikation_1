# Data Cleaning

# Pakete laden

#install.packages("readxl")

library(tidyverse)
library(psych)
library(readxl)
source("qualtricshelpers.R")

# Daten einlesen

Rohdaten <- "Daten/CSV.Rohdaten_23.06.csv"
raw <- load_qualtrics_csv(Rohdaten)


# Zeilen entfernen

raw <- filter(raw, Progress >= 99)


# Spalten entfernen

raw.short <- raw[,c(-1:-4, -7:-17, -131:-132)]

#Ab hier!

codebook <- read_codebook("data/codebook_final.csv")

names(raw.short) <- codebook$variable

# Richtige Datentypen zuordnen ----

raw.short$age

raw.short[188,]$age = "55" 
raw.short[266,]$age = "39"
raw.short[285,]$age = "59"

raw.short$age <- as.numeric(raw.short$age)

raw.short$gender <- as.factor(raw.short$gender)

raw.short$branch <- as.factor(raw.short$branch)


# Qualitätskontrolle 

# Skalen berechnen

# Lösung abspeichern