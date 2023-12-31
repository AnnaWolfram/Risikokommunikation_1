# Data Cleaning
#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
#install.packages("jmv")
#install.packages("rstatix")

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

# Daten einlesen
Rohdaten <- "Daten/Rohdaten_17.07.csv"
raw <- load_qualtrics_csv(Rohdaten)


# Zeilen entfernen
raw <- filter(raw, Progress >= 99)


# Spalten entfernen
raw.short <- raw[,c(-1:-4, -7:-8,-10:-17, -131:-132)]


# Codebook generieren
generate_codebook(raw.short, Rohdaten, "Daten/codebook.csv")
codebook <- read_codebook("Daten/codebook_final.csv")
names(raw.short) <- codebook$variable


# Richtige Datentypen zuordnen
raw.short[176,]$age = "24"

raw.short$age <- as.numeric(raw.short$age)

raw.short$gender[is.na(raw.short$gender)] <- 4
raw.short$gender %>%
  dplyr::recode_factor(`1`= "männlich", `2` = "weiblich", `3`="divers", `4`="keine Angabe") %>%
  as.factor() -> raw.short$gender

raw.short$license[is.na(raw.short$license)] <- 5
raw.short$license <- raw.short$license %>%
  dplyr::recode_factor(`1`="PKW-Führerschein",
         `2`="Motorrad-Führerschein",
         `3`= "Kraftfahrzeug-Führerschein",
         `4`="Keinen Führerschein",
         `5`="keine Angabe",
         `12`="PKW-Führerschein & Motorrad-Führerschein",
         `13`="PKW-Führerschein & Kraftfahrzeug-Führerschein",
         `123`="PKW-Führerschein & Motorrad-Führerschein & Kraftfahrzeug-Führerschein") %>%
  as.factor()

raw.short$education[raw.short$education == 6] <- NA
raw.short$education %>%
  ordered(levels = c(1:5),
          labels = c(`1`="(noch) kein Schulabschluss",
                     `2`="Hauptschulabschluss / Volksschulabschluss",
                     `3`= "Realschulabschluss (Mittlere Reife)",
                     `4`="(Fach-)Abitur",
                     `5`="(Fach-)Hochschulabschluss")) -> raw.short$education

raw.short$occupation[raw.short$occupation == 10] <- NA
raw.short$occupation %>%
  ordered(levels = c(1:9),
          labels = c(`1`="Schüler:in",
                     `2`="Auszubildende/-r",
                     `3`= "Student:in",
                     `4`="Voll berufstätig",
                     `5`="Teilweise berufstätig",
                     `6`="Rentner:in, Pensionär:in",
                     `7`="Arbeitslos / arbeitssuchend",
                     `8`= "Hausmann / Hausfrau und / oder versorge Kinder / pflegebedürftige Personen",
                     `9`= "Sonstiges")) -> raw.short$occupation

raw.short$city_size[raw.short$city_size == 6] <- NA
raw.short$city_size %>%
  ordered(levels = c(1:5),
          labels = c(`1`="< 5.000 Einwohner:innen",
                     `2`="5.000 - 20.000 Einwohner:innen",
                     `3`= "20.000 - 100.000 Einwohner:innen",
                     `4`="100.000 - 500.000 Einwohner:innen",
                     `5`="> 500.000 Einwohner:innen")) -> raw.short$city_size

raw.short$duration_license[raw.short$duration_license == 5] <- NA
raw.short$duration_license %>%
  ordered(levels = c(1:4),
          labels = c(`1`="Seit weniger als einem Jahr",
                     `2`="Zwischen 1-5 Jahren",
                     `3`= "Zwischen 5-10 Jahren",
                     `4`="Seit mehr als 10 Jahren")) -> raw.short$duration_license

raw.short$car_type[raw.short$car_type == 6] <- NA
raw.short$car_type %>%
  ordered(levels = c(1:5),
          labels = c(`1`="Diesel-Auto",
                     `2`="Benzin-Auto",
                     `3`= "Elektro-Auto",
                     `4`="Hybrid-Auto",
                     `5`="Brennstoffzellen-Auto")) -> raw.short$car_type

raw.short$car_type[raw.short$car_type == 6] <- NA
raw.short$car_type[is.na(raw.short$car_type)] <- 5
raw.short$car_type %>%
  dplyr::recode_factor(`1`="Diesel-Auto",
         `2`="Benzin-AUto",
         `3`= "Elektro-Auto",
         `4`="Hybrid-Auto",
         `5`="Brennstoffzellen-Auto") %>%
  as.factor() -> raw.short$car_type

raw.short$`km_per year`[raw.short$`km_per year` == 6] <- NA
raw.short$`km_per year` %>%
  ordered(levels = c(1:5),
          labels = c(`1`="bis 5.000 km",
                     `2`="5.001 km bis 10.000 km",
                     `3`= "10.001 km bis 15.000 km",
                     `4`="15.001 km bis 20.000 km",
                     `5`="mehr als 20.001 km")) -> raw.short$`km_per year`

raw.short$frequency[raw.short$frequency == 6] <- NA
raw.short$frequency %>%
  ordered(levels = c(1:5),
          labels = c(`1`="Täglich",
                     `2`="Mehrmals in der Woche",
                     `3`= "Mehrmals im Monat",
                     `4`="Mehrmals im Jahr",
                     `5`="Noch seltener / nie")) -> raw.short$frequency

raw.short$prior_knowledge[raw.short$prior_knowledge == 6] <- NA
raw.short$prior_knowledge %>%
  ordered(levels = c(1:5),
          labels = c(`1`="Ich könnte ihre Funktionsweise im Detail erklären.",
                     `2`="Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren.",
                     `3`="Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos.",
                     `4`="Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen.",
                     `5`="Ich habe keine Kenntnisse über Elektrofahrzeuge.")) -> raw.short$prior_knowledge

raw.short$experience[raw.short$experience == 6] <- NA
raw.short$experience %>%
  ordered(levels = c(1:5),
          labels = c(`1`="Ich nutze aktuell ein E-Auto.",
                     `2`="In der Vergangenheit habe ich E-Autos für längere Zeit genutzt.",
                     `3`="Ich bin schon öfter mit einem Elektrofahrzeug gefahren.",
                     `4`="Ich bin einmal mit einem E-Auto gefahren.",
                     `5`="Ich habe keine Erfahrung mit E-Autos.")) -> raw.short$experience

raw.short


# Qualitätskontrolle

nrow(raw.short)
raw.short <- careless_indices(raw.short,
                              speeder_analysis = "median/3",
                              likert_vector = c(27:41,43:60,71:108))
raw.short %>%
  filter(speeder_flag == FALSE) -> raw.noSpeeder #notwendig, da sonst careless nicht funktioniert

raw.short %>%
  filter(speeder_flag == FALSE) %>%
  filter(careless_longstr < 20) %>%
  filter(careless_psychsyn > 0) %>%
  filter(careless_psychant < 0) %>%
  filter(careless_mahadflag == FALSE) -> raw.short


# Proband:innen unter 18 Jahren entfernen
raw.short <- raw.short[raw.short$age >= 18, ]


# Proband:innen ohne Führerschein oder mit "keine Angabe" entfernen
raw.short <- raw.short[!(raw.short$license %in% c("Keinen Führerschein", "keine Angabe")), ]


# Skalen berechnen

schluesselliste <- list(
  Evaluation = c("evaluation_1", "evaluation_2", "-evaluation_3", "-evaluation_4"),
  Evaluation_Why = c("evaluation_why_1", "evaluation_why_2", "evaluation_why_3", "evaluation_why_4", "evaluation_why_5", "-evaluation_why_6_n"),
  Evaluation_Why_n = c("evaluation_why_n_1", "evaluation_why_n_2", "evaluation_why_n_3", "evaluation_why_n_4", "evaluation_why_n_5"),
  Dest = c("concern_dest", "impact_dest", "prob_dest"),
  Charging = c("concern_charging", "impact_charging", "prob_charging_n"),
  Time = c("concern_time", "impact_time", "prob_time"),
  Accident = c("concern_accident", "impact_accident", "prob_accident"),
  Price = c("concern_price", "impact_price", "prob_price"),
  Support = c("concern_support", "impact_support", "prob_support"),
  Driving_Climate = c("driving_climate_1", "-driving_climate_2_n", "driving_climate_3", "-driving_climate_4_n"),
  EV_Attitude = c("ev_attitude_1", "ev_attitude_2", "ev_attitude_3"),
  Personality_neuro = c("-personality_3_n", "personality_4"),
  Tech_Interaction = c("tech_interaction_1", "tech_interaction_2", "-tech_interaction_3_n", "tech_interaction_4", "tech_interaction_5", "-tech_interaction_6_n", "tech_interaction_7", "-tech_interaction_8_n", "tech_interaction_9"),
  Dest_2 = c("2concern_dest", "2impact_dest", "2prob_dest"),
  Charging_2 = c("2concern_charging", "2impact_charging", "2prob_charging"),
  Time_2 = c("2concern_time", "2impact_time", "2prob_time"),
  Accident_2 = c("2concern_accident", "2impact_accident", "2prob_accident"),
  Price_2 = c("2concern_price", "2impact_price", "2prob_price"),
  Support_2 = c("2concern_support", "2impact_support", "2prob_support"))

scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)
data <- bind_cols(raw.short, as_tibble(scores$scores))

scores$alpha

saveRDS(data, "Daten/dataFromNumeric.rds")


# Filtern von Positiv und negativ

library(dplyr)

# Erstellen einer neuen Spalte "framing"
data <- data %>%
  mutate(framing = case_when(
    control_question == 1 ~ "positiv",
    n_control_reading == 1 ~ "negativ",
    control_neutral == 1 ~ "data_framing_controll",  # Neue Bedingung für Personen ohne Text
    TRUE ~ NA_character_  # Wenn keine der Bedingungen erfüllt ist, wird NA gesetzt
  ))

# Filtern der Proband:innen mit positivem Framing
data_positiv <- data %>%
  filter(framing == "positiv")

# Filtern der Proband:innen mit negativem Framing
data_negativ <- data %>%
  filter(framing == "negativ")

# Filtern der Proband:innen ohne Text
data_noText <- data %>%
  filter(framing == "data_framing_controll")

# Spaltennamen und -reihenfolge in den drei Datensätzen sicherstellen
data_positiv <- data_positiv[, names(data_noText)]
data_negativ <- data_negativ[, names(data_noText)]

# Zusammenführen der drei Datensätze
data_combined <- bind_rows(data_positiv, data_negativ, data_noText)

# Speichern des kombinierten Datensatzes als RDS-Datei
saveRDS(data_combined, "Daten/data_combined.rds")

