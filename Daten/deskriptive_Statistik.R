# Altersverteilung der Stichprobe
summary(data$age)

# Geschlechterverteilung
table(data$gender)
prop.table(gender_summary) * 100
round(gender_summary_percent, 2)

# Bildungsabschluss
table(data$education)

# Beschäftigung
table(data$occupation)

# Wohnortgröße
table(data$city_size)

# Mobilitätsverhalten
# Besitz Führerschein
table(data$license)

# Dauer Besitz Führerschein
summary(data$duration_license)

# Auto Besitz (--> Art des Autos)
table(data$car_type)

# Fahrerfahrung (KM pro Jahr + Häufigkeit der Fahrertätigkeit)
summary(data$km_per_year)
table(data$frequency)

# Fahren alleine oder in Begleitung?
table(data$companionship)

# Präferenz der Fortbewegungsmittel?
preferences_summary <- data %>%
  select(starts_with("preferences_mobility")) %>%
  gather(key = "question", value = "answer") %>%
  table()

table(data$EV_Attitude)
