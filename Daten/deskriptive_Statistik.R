# Pakete installieren

#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("esquisse")
#install.packages("shiny")

# Pakete aktivieren
library(hcictools)
library(tidyverse)
rwthcolor <- hcictools::rwth.colorpalette()
library(dplyr)
library(ggplot2)
library(shiny)

# Visualisierungen

# Alter ----
library(dplyr)
library(ggplot2)

data_combined %>%
 ggplot() +
 aes(x = age) +
 geom_histogram(bins = 30L, fill = "#0C4C8A") +
 labs(x = "Alter in Jahren", 
      y = "Anzahl", 
      title = "Stdentische Stichprobe n = 201", 
      subtitle = "Altersverteilung der Stichprobe", 
      caption = " ") +
 theme_minimal()


# Geschlecht ----

library(dplyr)
library(ggplot2)

data_combined %>%
 ggplot() +
 aes(x = gender) +
 geom_bar(fill = "#112446") +
 labs(x = "Geschlecht", 
      y = "Anzahl", 
      title = "Studentische Stichprobe", 
      subtitle = "Geschlerterverteilung", 
      caption = " ") +
 theme_minimal()

# Bildungsabschluss ----

library(dplyr)
library(ggplot2)

data_combined %>%
 ggplot() +
 aes(x = education) +
 geom_bar(fill = "#112446") +
 labs(x = "Bildungsabschluss", 
      y = "Anzahl", 
      title = "Studentische Stichprobe", 
      subtitle = "Vereulung der Bildungsabschlüsse", 
      caption = " ") +
 theme_minimal()

# Beschäftigung ----

data_combined %>%
 ggplot() +
 aes(x = occupation) +
 geom_bar(fill = "#112446") +
 labs(x = "Art der Beschäftigung", 
      y = "Anzahl", 
      title = "Studentische Stichprobe", 
      subtitle = "Verteilung der Art der Beschäftigung", 
      caption = " ") +
 theme_minimal()

# Art des Autos ----
table(data_combined$car_ownership)
table(data_combined$car_type)

library(dplyr)
library(ggplot2)

data_combined %>%
  filter(!is.na(car_type)) %>%
 ggplot() +
 aes(x = car_type) +
 geom_bar(fill = "#112446") +
 labs(x = "Art des Autos", 
      y = "Anzahl", 
      title = "Studentische Stichprobe", 
      subtitle = "Verteilung der Art des Autos der Proband:innen, die ein Auto besitzen", 
      caption = " ") +
 theme_minimal()


#Vorwissen e-Autos ----

data_combined %>%
  filter(!is.na(prior_knowledge)) %>%
 ggplot() +
 aes(x = prior_knowledge) +
 geom_bar(fill = "#112446") +
 labs(x = "Vorwissen über Elektromobilität", 
      y = "Anzahl", 
      title = "Studentische Stichprobe", 
      subtitle = "Selbsteinschätzung zum Vorwissen zu Elektromobilität", 
      caption = " ") +
 theme_minimal()


# Deskriptive Statistik (ohne Visualisierung) ----

# Altersverteilung der Stichprobe
summary(data$age)

age_summary <- table(data$age)
age_summary_percent <- prop.table(age_summary) * 100
age_summary_percent <- round(age_summary_percent, 2)
age_summary_percent


# Geschlechterverteilung
gender_summary <- table(data$gender)
gender_summary_percent <- prop.table(gender_summary) * 100
gender_summary_percent <- round(gender_summary_percent, 2)
gender_summary_percent

# Bildungsabschluss
education_summary <- table(data$education)
education_summary_percent <- prop.table(education_summary) * 100
education_summary_percent <- round(education_summary_percent, 2)
education_summary_percent

# Beschäftigung
occupation_summary <- table(data$occupation)
occupation_summary_percent <- prop.table(occupation_summary) * 100
occupation_summary_percent <- round(occupation_summary_percent, 2)
occupation_summary_percent

# Wohnortgröße
city_size_summary <- table(data$city_size)
city_size_summary_percent <- prop.table(city_size_summary) * 100
city_size_summary_percent <- round(city_size_summary_percent, 2)
city_size_summary_percent

# Mobilitätsverhalten
# Besitz Führerschein
license_summary <- table(data$license)
license_summary_percent <- prop.table(license_summary) * 100
license_summary_percent <- round(license_summary_percent, 2)
license_summary_percent


### Darstellung der Deskriptiven Statistik

# Dauer Besitz Führerschein
summary(data$duration_license)

# Auto Besitz (--> Art des Autos)
car_type_summary <- table(data$car_type)
car_type_summary_percent <- prop.table(car_type_summary) * 100
car_type_summary_percent <- round(car_type_summary_percent, 2)
car_type_summary_percent

# Fahrerfahrung (KM pro Jahr + Häufigkeit der Fahrertätigkeit)
summary(data$km_per_year)
table(data$frequency)

# Fahren alleine oder in Begleitung?
companionship_summary <- table(data$companionship)
companionship_summary_percent <- prop.table(companionship_summary) * 100
companionship_summary_percent <- round(companionship_summary_percent, 2)
companionship_summary_percent

# Einstellung E-Mobilität
EV_Attitude_summary <- table(data$EV_Attitude)
EV_Attitude_summary_percent <- prop.table(EV_Attitude_summary) * 100
EV_Attitude_summary_percent <- round(EV_Attitude_summary_percent, 2)
EV_Attitude_summary_percent




