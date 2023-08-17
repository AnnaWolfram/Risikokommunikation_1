saveRDS(data_combined, "Daten/data_combined.rds")
##Datenauswertung

#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#installed.packages("tidyverse")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
#install.packages("jmv")
#install.packages("rstatix")
#install.packages("ez")

library(hcictools)
library(tidyverse)
library(psych)
library(readxl)
library(careless)
source("qualtricshelpers.R")
library(jmv)
library(dplyr)
library(rstatix)
library(car)
library(ez)

# Hinzufügen einer Framing-Gruppe
data_combined$framing <- ifelse(!is.na(data_combined$n_control_reading), "N",
                                ifelse(!is.na(data_combined$control_question), "P", "Neutral"))

# Filtern Sie die Daten, um nur die gewünschten Gruppen zu behalten
data_filtered <- data_combined[data_combined$framing != "Neutral", ]

# Daten einlesen
data_combined <- readRDS("Daten/data_combined.rds")

# Hypothese: Einfluss von Framing positiv auf Risikowahrnehmung----

# Daten für "risk_before_mit_Neutral" auswählen und Zeitpunkt hinzufügen
risk_before_mit_Neutral <- data_filtered %>%
  select(ResponseId, Dest, Charging, Time, Accident, Price, Support, framing) %>%
  mutate(Zeit = "Vor")

# Daten für "risk_after_mit_Neutral" auswählen und Zeitpunkt hinzufügen
risk_after_mit_Neutral <- data_filtered %>%
  select(ResponseId, Dest_2, Charging_2, Time_2, Accident_2, Price_2, Support_2, framing) %>%
  mutate(Zeit = "Nach")

# Aus risk_before_mit_Neutral und risk_after_mit_Neutral alle Neutralen filtern
risk_before_neutral_filtered <- risk_before_mit_Neutral[risk_before_mit_Neutral$framing != "Neutral", ]
risk_after_neutral_filtered <- risk_after_mit_Neutral[risk_after_mit_Neutral$framing != "Neutral", ]

# Risikowahrnehmungsdifferenz berechnen
risk_diff <- data.frame(
  ResponseId = risk_before_neutral_filtered$ResponseId,
  diff_dest = risk_after_neutral_filtered$Dest_2 - risk_before_neutral_filtered$Dest,
  diff_charging = risk_after_neutral_filtered$Charging_2 - risk_before_neutral_filtered$Charging,
  diff_time = risk_after_neutral_filtered$Time_2 - risk_before_neutral_filtered$Time,
  diff_accident = risk_after_neutral_filtered$Accident_2 - risk_before_neutral_filtered$Accident,
  diff_price = risk_after_neutral_filtered$Price_2 - risk_before_neutral_filtered$Price,
  diff_support = risk_after_neutral_filtered$Support_2 - risk_before_neutral_filtered$Support,
  framing = risk_before_neutral_filtered$framing)

#Allgemeine Risikowahnehmung ----
# Summe der Differenzen für jede Person
risk_diff$overall_diff <- rowSums(risk_diff[,c("diff_dest", "diff_charging", "diff_time", "diff_accident", "diff_price", "diff_support")], na.rm = TRUE)
risk_diff$overall_diff
library(dplyr)

#Füge die overall_diff-Spalte aus risk_diff zu data_filtered hinzu, basierend auf der ResponseId-Variable
data_filtered <- data_filtered %>%
  left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")






