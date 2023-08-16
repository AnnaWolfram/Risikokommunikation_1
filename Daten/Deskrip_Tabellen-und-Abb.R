# Installiere und lade das 'gt' Paket für die Tabellengestaltung
#install.packages("gt")
library(gt)
library(psych)
library(dplyr)


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

