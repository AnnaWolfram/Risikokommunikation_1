# # Pakete laden
library(tidyverse)

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


# Mann-Whitney-U-Test für ungepaarte Sichproben

# Dest
jmv::ttestIS(
  formula = diff_dest ~ framing,
  data = risk_diff,
  vars = "diff_dest",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Charging
jmv::ttestIS(
  formula = diff_charging ~ framing,
  data = risk_diff,
  vars = "diff_charging",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Time
jmv::ttestIS(
  formula = diff_time ~ framing,
  data = risk_diff,
  vars = "diff_time",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Price
jmv::ttestIS(
  formula = diff_price ~ framing,
  data = risk_diff,
  vars = "diff_price",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Support
jmv::ttestIS(
  formula = diff_support ~ framing,
  data = risk_diff,
  vars = "diff_support",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Accident
jmv::ttestIS(
  formula = diff_accident ~ framing,
  data = risk_diff,
  vars = "diff_accident",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

#Allgemeine Risikowahnehmung ----
# Summe der Differenzen für jede Person
risk_diff$overall_diff <- rowSums(risk_diff[,c("diff_dest", "diff_charging", "diff_time", "diff_accident", "diff_price", "diff_support")], na.rm = TRUE)
risk_diff$overall_diff
library(dplyr)

# Füge die overall_diff-Spalte aus risk_diff zu data_filtered hinzu, basierend auf der ResponseId-Variable
data_filtered <- data_filtered %>%
  left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")

# overall_diff
jmv::ttestIS(
  formula = overall_diff ~ framing,
  data = risk_diff,
  vars = "overall_diff",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

