# Altersverteilung der Stichprobe
age_summary <- summary(data$age)

# Geschlechterverteilung
gender_summary <- table(data$gender)
gender_summary_percent <- prop.table(gender_summary) * 100
gender_summary_percent <- round(gender_summary_percent, 2)

# Bildungsabschluss
education_summary <- table(data$education)

# Beschäftigung
occupation_summary <- table(data$occupation)

# Wohnortgröße
city_size_summary <- table(data$city_size)

# Mobilitätsverhalten
# Besitz Führerschein
license_summary <- table(data$license)

# Dauer Besitz Führerschein
duration_license_summary <- summary(data$duration_license)

# Auto Besitz (--> Art des Autos)
car_type_summary <- table(data$car_type)

# Fahrerfahrung (KM pro Jahr + Häufigkeit der Fahrertätigkeit)
km_per_year_summary <- summary(data$km_per_year)
frequency_summary <- table(data$frequency)

# Fahren alleine oder in Begleitung?
companionship_summary <- table(data$companionship)

# Präferenz der Fortbewegungsmittel?
preferences_summary <- data %>%
  select(starts_with("preferences_mobility")) %>%
  gather(key = "question", value = "answer") %>%
  table()

# Einstellung E-Mobilität
ev_attitude_summary <- table(data$EV_Attitude)
