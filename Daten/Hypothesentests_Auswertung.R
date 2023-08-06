saveRDS(data_combined, "Daten/data_combined.rds")
##Datenauswertung

#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#installed.packages("tidyverse")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
install.packages("jmv")
install.packages("rstatix")
install.packages("ez")

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



# Alle Outlier prüfen ----
# Identify outliers for Dest and Dest_2
data_filtered %>%
  select(ID = ResponseId, framing, Dest, Dest_2) %>%
  pivot_longer(cols = c(Dest, Dest_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# Identify outliers for Accident and Accident_2
data_filtered %>%
  select(ID = ResponseId, framing, Accident, Accident_2) %>%
  pivot_longer(cols = c(Accident, Accident_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# Identify outliers for Price and Price_2
data_filtered %>%
  select(ID = ResponseId, framing, Price, Price_2) %>%
  pivot_longer(cols = c(Price, Price_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# Identify outliers for Support and Support_2
data_filtered %>%
  select(ID = ResponseId, framing, Support, Support_2) %>%
  pivot_longer(cols = c(Support, Support_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# Identify outliers for Time and Time_2
data_filtered %>%
  select(ID = ResponseId, framing, Time, Time_2) %>%
  pivot_longer(cols = c(Time, Time_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)


#Normalverteilung nach Anderson-Darling-Test ----

# Laden des nortest-Pakets
install.packages("nortest")
library(nortest)

# Anderson-Darling-Test für die Variable "Dest"
ad_test_dest <- ad.test(data_filtered$Dest)
print(ad_test_dest)

# Anderson-Darling-Test für die Variable "Dest_2"
ad_test_dest_2 <- ad.test(data_filtered$Dest_2)
print(ad_test_dest_2)

# Anderson-Darling-Test für die Variable "Charging"
ad_test_charging <- ad.test(data_filtered$Charging)
print(ad_test_charging)

# Anderson-Darling-Test für die Variable "Charging_2"
ad_test_charging_2 <- ad.test(data_filtered$Charging_2)
print(ad_test_charging_2)

# Anderson-Darling-Test für die Variable "Price"
ad_test_price <- ad.test(data_filtered$Price)
print(ad_test_price)

# Anderson-Darling-Test für die Variable "Price_2"
ad_test_price_2 <- ad.test(data_filtered$Price_2)
print(ad_test_price_2)

# Anderson-Darling-Test für die Variable "Support"
ad_test_support <- ad.test(data_filtered$Support)
print(ad_test_support)

# Anderson-Darling-Test für die Variable "Support_2"
ad_test_support_2 <- ad.test(data_filtered$Support_2)
print(ad_test_support_2)

# Anderson-Darling-Test für die Variable "Time"
ad_test_time <- ad.test(data_filtered$Time)
print(ad_test_time)

# Anderson-Darling-Test für die Variable "Time_2"
ad_test_time_2 <- ad.test(data_filtered$Time_2)
print(ad_test_time_2)

library(ggplot2)

ggplot(data_filtered, aes(x = Dest)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Density Plot and Histogram of Dest")

qqnorm(data_filtered$Dest)
qqline(data_filtered$Dest)

#Ab hier: Hypothesenvorbereitung und Prüfung pro Risikowahrnehmung (teilweise Doppelungen)
# Dest und Dest_2 ----

#Umwandeln der Daten in das "long" Format für Dest und Dest_2 
data_long <- data_filtered %>%
  select(ID = ResponseId, framing, Dest, Dest_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Dest", "Dest_2")))


# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Dest)
shapiro.test(data_filtered$Dest_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Dest ~ framing, data = data_filtered)
leveneTest(Dest_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test <- ezANOVA(
  data = data_long,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test$Mauchly)



# Gemischte ANOVA für Dest und Dest_2
library(afex)
mixed_anova_result <- aov_ez(data_long, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result)




# Charging und Charging_2----



# Umwandeln der Daten in das "long" Format für Charging und Charging_2
data_long_charging <- data_filtered %>%
  select(ID = ResponseId, framing, Charging, Charging_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Charging", "Charging_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Charging, data_filtered$Charging_2, main="Check for Outliers", names=c("Charging", "Charging_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Charging, Charging_2) %>%
  pivot_longer(cols = c(Charging, Charging_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Charging)
shapiro.test(data_filtered$Charging_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Charging ~ framing, data = data_filtered)
leveneTest(Charging_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test_charging <- ezANOVA(
  data = data_long_charging,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test_charging$Mauchly)

library(lme4)

# Erstelle das Modell mit einer geeigneten Verteilung für deine Daten (z.B. binomial)
model <- glmer(value ~ framing * time + (1|ID), data = data_long_charging, family = binomial)
summary(model)


 bb
# Gemischte ANOVA für Charging und Charging_2
mixed_anova_result_charging <- aov_ez(data_long_charging, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_charging)

#Time und Time_2

# Umwandeln der Daten in das "long" Format für Time und Time_2
data_long_time <- data_filtered %>%
  select(ID = ResponseId, framing, Time, Time_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Time", "Time_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Time, data_filtered$Time_2, main="Check for Outliers", names=c("Time", "Time_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Time, Time_2) %>%
  pivot_longer(cols = c(Time, Time_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Time)
shapiro.test(data_filtered$Time_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Time ~ framing, data = data_filtered)
leveneTest(Time_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test_time <- ezANOVA(
  data = data_long_time,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test_time$Mauchly)

# Gemischte ANOVA für Time und Time_2
mixed_anova_result_time <- aov_ez(data_long_time, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_time)


# Accident und Accident_2 ----

# Umwandeln der Daten in das "long" Format für Accident und Accident_2
data_long_accident <- data_filtered %>%
  select(ID = ResponseId, framing, Accident, Accident_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Accident", "Accident_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Accident, data_filtered$Accident_2, main="Check for Outliers", names=c("Accident", "Accident_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Accident, Accident_2) %>%
  pivot_longer(cols = c(Accident, Accident_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Accident)
shapiro.test(data_filtered$Accident_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Accident ~ framing, data = data_filtered)
leveneTest(Accident_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test_accident <- ezANOVA(
  data = data_long_accident,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test_accident$Mauchly)

# Gemischte ANOVA für Accident und Accident_2
mixed_anova_result_accident <- aov_ez(data_long_accident, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_accident)


#Price und Price_2 ----

# Umwandeln der Daten in das "long" Format für Price und Price_2

data_long_price <- data_filtered %>%
  select(ID = ResponseId, framing, Price, Price_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Price", "Price_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Price, data_filtered$Price_2, main="Check for Outliers", names=c("Price", "Price_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Price, Price_2) %>%
  pivot_longer(cols = c(Price, Price_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Price)
shapiro.test(data_filtered$Price_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Price ~ framing, data = data_filtered)
leveneTest(Price_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test_price <- ezANOVA(
  data = data_long_price,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test_price$Mauchly)

# Gemischte ANOVA für Price und Price_2
mixed_anova_result_price <- aov_ez(data_long_price, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_price)



#Support und Support_2----

# Umwandeln der Daten in das "long" Format für Support und Support_2
data_long_support <- data_filtered %>%
  select(ID = ResponseId, framing, Support, Support_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Support", "Support_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Support, data_filtered$Support_2, main="Check for Outliers", names=c("Support", "Support_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Support, Support_2) %>%
  pivot_longer(cols = c(Support, Support_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Support)
shapiro.test(data_filtered$Support_2)

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Support ~ framing, data = data_filtered)
leveneTest(Support_2 ~ framing, data = data_filtered)

# Testen der Sphärizität
sphericity_test_support <- ezANOVA(
  data = data_long_support,
  dv = value,
  wid = ID,
  within = time,
  between = framing,
  detailed = TRUE
)

# Ausgabe des Testergebnisses für Sphärizität
print(sphericity_test_support$Mauchly)

# Gemischte ANOVA für Support und Support_2
mixed_anova_result_support <- aov_ez(data_long_support, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_support)



















#