saveRDS(data_combined, "Daten/data_combined.rds")

#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
#install.packages("jmv")
#install.packages("rstatix")
#install.packages("nortest")

library(nortest)
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
library(ggplot2)
#________________________________________________________________________

#Vorbereitung der Hypothesentests

# Hinzufügen einer Framing-Gruppe
data_combined$framing <- ifelse(!is.na(data_combined$n_control_reading), "N",
                                ifelse(!is.na(data_combined$control_question), "P", "Neutral"))

# Filtern der Daten
data_filtered <- data_combined[data_combined$framing != "Neutral", ]

# Daten einlesen
data_combined <- readRDS("Daten/data_combined.rds")

# Hypothese: Einfluss von Framing (positiv) auf Risikowahrnehmung----

# Daten für "risk_before_mit_Neutral" und "risk_after_mit_Neutral" auswählen und Zeitpunkt hinzufügen
risk_before_mit_Neutral <- data_filtered %>%
  select(ResponseId, Dest, Charging, Time, Accident, Price, Support, framing) %>%
  mutate(Zeit = "Vor")

risk_after_mit_Neutral <- data_filtered %>%
  select(ResponseId, Dest_2, Charging_2, Time_2, Accident_2, Price_2, Support_2, framing) %>%
  mutate(Zeit = "Nach")

# Filtern der Kontrollgruppe (neutral)
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

# Allgemeine Risikowahnehmung ----
# Summe der Differenzen für jede Person
risk_diff$overall_diff <- rowSums(risk_diff[,c("diff_dest", "diff_charging", "diff_time", "diff_accident", "diff_price", "diff_support")], na.rm = TRUE)
risk_diff$overall_diff
library(dplyr)

# overall_diff-Spalte aus risk_diff zu data_filtered hinzufügen
data_filtered <- data_filtered %>%
  left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")

#_________________________________________________________________________________

#Prüfung der Normalverteilung nach Anderson-Darling-Test bei allen relevanten Konstrukten ----

# Anderson-Darling-Test für die Variable "Dest" und "Dest_2"
ad_test_dest <- ad.test(data_filtered$Dest)
print(ad_test_dest)

ad_test_dest_2 <- ad.test(data_filtered$Dest_2)
print(ad_test_dest_2)

# Anderson-Darling-Test für die Variable "Charging" und "Charging_2"
ad_test_charging <- ad.test(data_filtered$Charging)
print(ad_test_charging)

ad_test_charging_2 <- ad.test(data_filtered$Charging_2)
print(ad_test_charging_2)

# Anderson-Darling-Test für die Variable "Price" und "Price_2"
ad_test_price <- ad.test(data_filtered$Price)
print(ad_test_price)

ad_test_price_2 <- ad.test(data_filtered$Price_2)
print(ad_test_price_2)

# Anderson-Darling-Test für die Variable "Support" und "Support_2"
ad_test_support <- ad.test(data_filtered$Support)
print(ad_test_support)

ad_test_support_2 <- ad.test(data_filtered$Support_2)
print(ad_test_support_2)

# Anderson-Darling-Test für die Variable "Accident" und "Accident_2"
ad_test_accident <- ad.test(data_filtered$Accident)
print(ad_test_accident)

ad_test_accident_2 <- ad.test(data_filtered$Accident_2)
print(ad_test_accident_2)

# Anderson-Darling-Test für die Variable "Time" und "Time_2"
ad_test_time <- ad.test(data_filtered$Time)
print(ad_test_time)

ad_test_time_2 <- ad.test(data_filtered$Time_2)
print(ad_test_time_2)


# Visuelle Inspektion der Normalverteilung 

# Dest und Dest_2
ggplot(data_filtered, aes(x = Dest)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Dest")

qqnorm(data_filtered$Dest)
qqline(data_filtered$Dest)

ggplot(data_filtered, aes(x = Dest_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Dest_2")

qqnorm(data_filtered$Dest_2)
qqline(data_filtered$Dest_2)

# Charging und Charging_2

ggplot(data_filtered, aes(x = Charging)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Charging")

qqnorm(data_filtered$Charging)
qqline(data_filtered$Charging)

ggplot(data_filtered, aes(x = Charging_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Charging_2")

qqnorm(data_filtered$Charging_2)
qqline(data_filtered$Charging_2)

# Price und Price_2

ggplot(data_filtered, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Price")

qqnorm(data_filtered$Price)
qqline(data_filtered$Price)

ggplot(data_filtered, aes(x = Price_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Price_2")

qqnorm(data_filtered$Price_2)
qqline(data_filtered$Price_2)

# Support und Support_2

ggplot(data_filtered, aes(x = Support)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Support")

qqnorm(data_filtered$Support)
qqline(data_filtered$Support)

ggplot(data_filtered, aes(x = Support_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Support_2")

qqnorm(data_filtered$Support_2)
qqline(data_filtered$Support_2)

# Accident und Accident_2

ggplot(data_filtered, aes(x = Accident)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Accident")

qqnorm(data_filtered$Accident)
qqline(data_filtered$Accident)

ggplot(data_filtered, aes(x = Accident_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Accident_2")

qqnorm(data_filtered$Accident_2)
qqline(data_filtered$Accident_2)

# Time und Time_2
ggplot(data_filtered, aes(x = Time)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Time")

qqnorm(data_filtered$Time)
qqline(data_filtered$Time)

ggplot(data_filtered, aes(x = Time_2)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Time_2")

qqnorm(data_filtered$Time_2)
qqline(data_filtered$Time_2)



