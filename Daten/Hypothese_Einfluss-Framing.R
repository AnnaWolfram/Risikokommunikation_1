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

  risk_diff                      
                        
  # Wilcoxon-Rangsummentest für den Unterschied in "dest"
  wilcox_dest <- wilcox.test(diff_dest ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Wilcoxon-Rangsummentest für den Unterschied in "charging"
  wilcox_charging <- wilcox.test(diff_charging ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Wilcoxon-Rangsummentest für den Unterschied in "time"
  wilcox_time <- wilcox.test(diff_time ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Wilcoxon-Rangsummentest für den Unterschied in "accident"
  wilcox_accident <- wilcox.test(diff_accident ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Wilcoxon-Rangsummentest für den Unterschied in "price"
  wilcox_price <- wilcox.test(diff_price ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Wilcoxon-Rangsummentest für den Unterschied in "support"
  wilcox_support <- wilcox.test(diff_support ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # Ergebnisse anzeigen
  list(dest = wilcox_dest,
       charging = wilcox_charging,
       time = wilcox_time,
       accident = wilcox_accident,
       price = wilcox_price,
       support = wilcox_support)
  
  # Mediane für jede Gruppe und jede Kategorie berechnen
  median_diffs <- risk_diff %>%
    group_by(framing) %>%
    summarise(
      median_diff_dest = median(diff_dest, na.rm = TRUE),
      median_diff_charging = median(diff_charging, na.rm = TRUE),
      median_diff_time = median(diff_time, na.rm = TRUE),
      median_diff_accident = median(diff_accident, na.rm = TRUE),
      median_diff_price = median(diff_price, na.rm = TRUE),
      median_diff_support = median(diff_support, na.rm = TRUE)
    ) %>%
    filter(framing %in% c("P", "N"))
  
  # Ergebnisse anzeigen
  print(median_diffs)
  
  #Allgemeine Risikowahnehmung ----
  # Summe der Differenzen für jede Person
  risk_diff$overall_diff <- rowSums(risk_diff[,c("diff_dest", "diff_charging", "diff_time", "diff_accident", "diff_price", "diff_support")], na.rm = TRUE)
  
  #Mittelwert der gesamten Differenzen für N- und P-Gruppen:

overall_diff_mean <- risk_diff %>%
  filter(framing %in% c("P", "N")) %>%
  group_by(framing) %>%
  summarise(mean_overall_diff = mean(overall_diff))

#Vergleichen der Gesamtdifferenzen zwischen N- und P-Gruppen mit dem Wilcoxon-Test:
wilcox_overall <- wilcox.test(overall_diff ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))

print(wilcox_overall)

median_diff_N <- median(risk_diff$overall_diff[risk_diff$framing == "N"])
median_diff_P <- median(risk_diff$overall_diff[risk_diff$framing == "P"])

print(median_diff_N)
print(median_diff_P)
