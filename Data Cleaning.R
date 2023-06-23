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

raw.short <- raw[,c(-1:-5, -7:-8, -10:-17, -110)]
# Spalten umbenennen

# Richtige Datentypen zuordnen

# Qualitätskontrolle 

# Skalen berechnen

# Lösung abspeichern