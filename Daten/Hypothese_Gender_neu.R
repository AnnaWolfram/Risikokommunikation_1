# # Pakete laden
library(tidyverse)

# Hypothese: Frauen lassen sich eher beeinflussen als Männer----
# Daten einlesen und überprüfen
data_combined <- readRDS("Daten/data_combined.rds")

# Umwandlung der Variable "gender" in einen Faktor
data_combined$gender <- factor(data_combined$gender)

# Filtern der Daten nach männlichem und weiblichem Geschlecht
data_gender_filtered <- data_combined[data_combined$gender %in% c("männlich", "weiblich"), ]

# Konvertieren der gender-Variablen in einen Faktor
data_gender_filtered$gender <- factor(data_gender_filtered$gender, levels = c("männlich", "weiblich"))


#Berechnung für gesamte Risikowahrnehmung
# Berechnung der Risikowahrnehmung vor und nach dem Framing sowie die Differenz
data_gender_filtered$Risiko_pre <- rowMeans(data_gender_filtered[, c("Dest", "Charging", "Time", "Accident", "Price", "Support")])
data_gender_filtered$Risiko_post <- rowMeans(data_gender_filtered[, c("Dest_2", "Charging_2", "Time_2", "Accident_2", "Price_2", "Support_2")])
data_gender_filtered$Risiko_diff <- data_gender_filtered$Risiko_post - data_gender_filtered$Risiko_pre

# T-Test für Differenz der Risikowahrnehmung zwischen Frauen und Männer
ttest_Risiko_diff <- t.test(Risiko_diff ~ gender, data = data_gender_filtered)

# Ergebnis des T-Tests
print(ttest_Risiko_diff)


#Berechnung für die einzelnen Variablen 
# T-Tests Frauen
ttest_Dest_pre_women <- t.test(data_women$Dest)
ttest_Dest_post_women <- t.test(data_women$Dest_2)

ttest_Time_pre_women <- t.test(data_women$Time)
ttest_Time_post_women <- t.test(data_women$Time_2)

ttest_Charging_pre_women <- t.test(data_women$Charging)
ttest_Charging_post_women <- t.test(data_women$Charging_2)

ttest_Accident_pre_women <- t.test(data_women$Accident)
ttest_Accident_post_women <- t.test(data_women$Accident_2)

ttest_Price_pre_women <- t.test(data_women$Price)
ttest_Price_post_women <- t.test(data_women$Price_2)

ttest_Support_pre_women <- t.test(data_women$Support)
ttest_Support_post_women <- t.test(data_women$Support_2)


# T-Tests Männer
ttest_Dest_pre_men <- t.test(data_men$Dest)
ttest_Dest_post_men <- t.test(data_men$Dest_2)

ttest_Time_pre_men <- t.test(data_men$Time)
ttest_Time_post_men <- t.test(data_men$Time_2)

ttest_Charging_pre_men <- t.test(data_men$Charging)
ttest_Charging_post_men <- t.test(data_men$Charging_2)

ttest_Accident_pre_men <- t.test(data_men$Accident)
ttest_Accident_post_men <- t.test(data_men$Accident_2)

ttest_Price_pre_men <- t.test(data_men$Price)
ttest_Price_post_men <- t.test(data_men$Price_2)

ttest_Support_pre_men <- t.test(data_men$Support)
ttest_Support_post_men <- t.test(data_men$Support_2)


# Differenzen Frauen
data_women$Dest_diff <- data_women$Dest_2 - data_women$Dest
data_women$Time_diff <- data_women$Time_2 - data_women$Time
data_women$Charging_diff <- data_women$Charging_2 - data_women$Charging
data_women$Accident_diff <- data_women$Accident_2 - data_women$Accident
data_women$Price_diff <- data_women$Price_2 - data_women$Price
data_women$Support_diff <- data_women$Support_2 - data_women$Support

# Differenzen Männer
data_men$Dest_diff <- data_men$Dest_2 - data_men$Dest
data_men$Time_diff <- data_men$Time_2 - data_men$Time
data_men$Charging_diff <- data_men$Charging_2 - data_men$Charging
data_men$Accident_diff <- data_men$Accident_2 - data_men$Accident
data_men$Price_diff <- data_men$Price_2 - data_men$Price
data_men$Support_diff <- data_men$Support_2 - data_men$Support

# Berechnung des Durchschnitts der Differenzen für Frauen und Männer
avg_diff_women <- colMeans(data_women[, c("Dest_diff", "Time_diff", "Charging_diff", "Accident_diff", "Price_diff", "Support_diff")])
avg_diff_men <- colMeans(data_men[, c("Dest_diff", "Time_diff", "Charging_diff", "Accident_diff", "Price_diff", "Support_diff")])

# Erstellung einer Tabelle mit den Durchschnittsdifferenzen
avg_diff_table <- data.frame(
  Variable = variables,
  Durchschnitt_Frauen = avg_diff_women,
  Durchschnitt_Männer = avg_diff_men
)
print(avg_diff_table)




#