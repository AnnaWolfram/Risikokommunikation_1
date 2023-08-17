# Installiere und lade das 'gt' Paket für die Tabellengestaltung
#install.packages("gt")
#install.packages("esquisse")
library(gt)
library(psych)
library(dplyr)
library(esquisse)


# Alter ---------------------------

# Berechne die deskriptive Statistik für die Spalte 'age'
desc_stats <- describe(data_combined$age)

# Wandle die deskriptive Statistik in einen Datenrahmen um
desc_stats_df <- as.data.frame(desc_stats)

# Speichere den Datenrahmen in einer CSV-Datei (optional)
write.csv(desc_stats_df, "deskriptive_statistik.csv", row.names = FALSE)

# Erstelle eine hübsche Tabelle mit dem 'gt' Paket
gt_desc_stats <- gt(desc_stats_df) %>%
  tab_header(title = "Deskriptive Statistik für Altersangaben") %>%
  fmt_number(columns = vars(1:5), decimals = 2) # Anzahl der Dezimalstellen anpassen

# Speichere die Tabelle als HTML-Datei (optional) oder zeige sie an
# gtsave(gt_desc_stats, "deskriptive_statistik_tabelle.html")
print(gt_desc_stats)

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



# Geschlecht ---------------------------------------

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



# Bildungsabschluss ---------------------------------------

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

# Art der Beschäftigung  ---------------------------------------
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

# Art des Autos  ---------------------------------------

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

# Vorwissen Elektroautos  ---------------------------------------
# Das Problem ist, dass die Texte in prior_knowlege sehr lang sind. Deswegen kürze ich sie. Damit ich die Original Datei nicht verfälsche mache ich eine Kopie
# Erstelle eine Kopie deiner Daten, um die Änderungen vorzunehmen
data_display <- data_combined

# Ersetze die Werte in der Kopie der Variable 'prior_knowledge' durch die kürzeren Labels
data_display$prior_knowledge <- case_when(
  data_display$prior_knowledge == "Ich könnte ihre Funktionsweise im Detail erklären." ~ "Detailierte Funktionskenntnisse",
  data_display$prior_knowledge == "Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren." ~ "Klare Vorstellung von E-Autos",
  data_display$prior_knowledge == "Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos." ~ "Grundlegende Vorstellung von E-Autos",
  data_display$prior_knowledge == "Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen." ~ "Vage Bekanntheit von E-Autos",
  data_display$prior_knowledge == "Ich habe keine Kenntnisse über Elektrofahrzeuge." ~ "Keine Kenntnisse über E-Fahrzeuge",
  TRUE ~ data_display$prior_knowledge
)

data_display %>%
  filter(!is.na(prior_knowledge)) %>%
  ggplot() +
  aes(x = prior_knowledge) +
  geom_bar(fill = "#112446") +
  labs(x = "Vorwissen über Elektroautos", 
       y = "Anzahl", 
       title = "Studentische Stichprobe", 
       subtitle = "Selbsteinschätzung der Proband:innen zum Vorwissen über Elektroautos", 
       caption = " ") +
  theme_minimal()

save.image("Daten/data_display")



# Hypothese Geschlecht

colors <- c("#999999", "#333333") # Farben bestimmen

ggplot(data = data_filtered, aes(x = gender, y = overall_diff, fill = gender)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = colors) +
  labs(x = "Geschlecht", y = "Risikowahrnehmung") +
  ggtitle("Verteilung der Risikowahrnehmung zwischen den Geschlechtern")




#____________________________________________________________

#Unterschiede Risikowahrnehmung vor Framing und nach Framing für die verschiedenen Aspekte

#Füge die dest/dest_2/charging/charging_2/... aus  zu data_filtered hinzu, basierend auf der ResponseId-Variable
#data_filtered <- data_filtered %>%
 # left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")


# Filtern von Positiv und negativ

library(dplyr)

#aus Data_filtered zwei neue Datensätze (positiv und negativ) erstellenn

# Filtern der Probanden mit positivem Framing
data_filtered_positiv <- data_filtered %>%
  filter(framing == "P")

# Filtern der Probanden mit negativem Framing
data_filtered_negativ <- data_filtered %>%
  filter(data_filtered$framing == "N")

#Mediane / Mittelwerte berechnen

#Dest positiv
#mean(data_filtered_positiv$Dest)
median(data_filtered_positiv$Dest)

#mean(data_filtered_positiv$Dest_2)
median(data_filtered_positiv$Dest_2)


#Charging positiv
#mean(data_filtered_positiv$Charging)
median(data_filtered_positiv$Charging)

#mean(data_filtered_positiv$Charging_2)
median(data_filtered_positiv$Charging_2)

#Time positiv
#mean(data_filtered_positiv$Time)
median(data_filtered_positiv$Time)

#mean(data_filtered_positiv$Time_2)
median(data_filtered_positiv$Time_2)

#Accident positiv
#mean(data_filtered_positiv$Accident)
median(data_filtered_positiv$Accident)

#mean(data_filtered_positiv$Accident_2)
median(data_filtered_positiv$Accident_2)

#Price positiv
#mean(data_filtered_positiv$Price)
median(data_filtered_positiv$Price)

#mean(data_filtered_positiv$Price_2)
median(data_filtered_positiv$Price_2)

#Support positiv
#mean(data_filtered_positiv$Support)
median(data_filtered_positiv$Support)

#mean(data_filtered_positiv$Support_2)
median(data_filtered_positiv$Support_2)

#Dest negativ
#mean(data_filtered_negativ$Dest)
median(data_filtered_negativ$Dest)

#mean(data_filtered_negativ$Dest_2)
median(data_filtered_negativ$Dest_2)

#Charging negativ
#mean(data_filtered_negativ$Charging)
median(data_filtered_negativ$Charging)

#mean(data_filtered_negativ$Charging_2)
median(data_filtered_negativ$Charging_2)

#Time negativ
#mean(data_filtered_negativ$Time)
median(data_filtered_negativ$Time)

#mean(data_filtered_negativ$Time_2)
median(data_filtered_negativ$Time_2)


#Accident negativ
#mean(data_filtered_negativ$Accident)
median(data_filtered_negativ$Accident)

#mean(data_filtered_negativ$Accident_2)
median(data_filtered_negativ$Accident_2)

#Price negativ
#mean(data_filtered_negativ$Price)
median(data_filtered_negativ$Price)

#mean(data_filtered_negativ$Price_2)
median(data_filtered_negativ$Price_2)

#Support negativ
#mean(data_filtered_negativ$Support)
median(data_filtered_negativ$Support)

#mean(data_filtered_negativ$Support_2)
median(data_filtered_negativ$Support_2)
