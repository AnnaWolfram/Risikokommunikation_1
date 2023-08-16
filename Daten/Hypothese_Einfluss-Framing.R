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
 
#Z-Werte und Effektstärke
  
  # Wilcoxon-Test für die "Dest"-Variable:
  wilcox_dest_test <- wilcox.test(diff_dest ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # W-Wert für "Dest":
  W_dest <- wilcox_dest_test$statistic
  
  # p-Wert für "Dest":
  p_value_dest <- wilcox_dest_test$p.value
  
  # Z-Wert Berechnung für "Dest":
  Z_dest <- qnorm(p_value_dest / 2) # Für einen zweiseitigen Test
  if(W_dest > n1 * (n1 + n2 + 1) / 2) {
    Z_dest = -Z_dest
  }
  
  # Effektstärke Berechnung für "Dest":
  r_dest <- Z_dest / sqrt(N)
  
  cat("Z-Wert für Dest:", Z_dest, "\n")
  cat("Effektstärke (r) für Dest:", r_dest, "\n")

  # Wilcoxon-Test für die "Charging"-Variable:
  wilcox_charging_test <- wilcox.test(diff_charging ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # W-Wert für "Charging":
  W_charging <- wilcox_charging_test$statistic
  
  # p-Wert für "Charging":
  p_value_charging <- wilcox_charging_test$p.value
  
  # Z-Wert Berechnung für "Charging":
  Z_charging <- qnorm(p_value_charging / 2) # Für einen zweiseitigen Test
  if(W_charging > n1 * (n1 + n2 + 1) / 2) {
    Z_charging = -Z_charging
  }
  
  # Effektstärke Berechnung für "Charging":
  r_charging <- Z_charging / sqrt(N)
  
  cat("Z-Wert für Charging:", Z_charging, "\n")
  cat("Effektstärke (r) für Charging:", r_charging, "\n")
  
  # Wilcoxon-Test für die "Time"-Variable:
  wilcox_time_test <- wilcox.test(diff_time ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # W-Wert für "Time":
  W_time <- wilcox_time_test$statistic
  
  # p-Wert für "Time":
  p_value_time <- wilcox_time_test$p.value
  
  # Z-Wert Berechnung für "Time":
  Z_time <- qnorm(p_value_time / 2) # Für einen zweiseitigen Test
  if(W_time > n1 * (n1 + n2 + 1) / 2) {
    Z_time = -Z_time  # Die Richtung des Z-Wertes korrigieren
  }
  
  # Effektstärke Berechnung für "Time":
  r_time <- Z_time / sqrt(N)
  
  cat("Z-Wert für Time:", Z_time, "\n")
  cat("Effektstärke (r) für Time:", r_time, "\n")
  
  # Wilcoxon-Test für die "Price"-Variable:
  wilcox_price_test <- wilcox.test(diff_price ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # W-Wert für "Price":
  W_price <- wilcox_price_test$statistic
  
  # p-Wert für "Price":
  p_value_price <- wilcox_price_test$p.value
  
  # Z-Wert Berechnung für "Price":
  Z_price <- qnorm(p_value_price / 2) # Für einen zweiseitigen Test
  if(W_price > n1 * (n1 + n2 + 1) / 2) {
    Z_price = -Z_price  # Die Richtung des Z-Wertes korrigieren
  }
  
  # Effektstärke Berechnung für "Price":
  r_price <- Z_price / sqrt(N)
  
  cat("Z-Wert für Price:", Z_price, "\n")
  cat("Effektstärke (r) für Price:", r_price, "\n")
  
  # Wilcoxon-Test für die "Support"-Variable:
  wilcox_support_test <- wilcox.test(diff_support ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))
  
  # W-Wert für "Support":
  W_support <- wilcox_support_test$statistic
  
  # p-Wert für "Support":
  p_value_support <- wilcox_support_test$p.value
  
  # Z-Wert Berechnung für "Support":
  Z_support <- qnorm(p_value_support / 2) # Für einen zweiseitigen Test
  if(W_support > n1 * (n1 + n2 + 1) / 2) {
    Z_support = -Z_support  # Die Richtung des Z-Wertes korrigieren
  }
  
  # Effektstärke Berechnung für "Support":
  r_support <- Z_support / sqrt(N)
  
  cat("Z-Wert für Support:", Z_support, "\n")
  cat("Effektstärke (r) für Support:", r_support, "\n")
  
  
  
   
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
risk_diff$overall_diff
library(dplyr)

# Füge die overall_diff-Spalte aus risk_diff zu data_filtered hinzu, basierend auf der ResponseId-Variable
data_filtered <- data_filtered %>%
  left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")





  #Mittelwert der gesamten Differenzen für N- und P-Gruppen:

overall_diff_mean <- risk_diff %>%
  filter(framing %in% c("P", "N")) %>%
  group_by(framing) %>%
  summarise(mean_overall_diff = mean(overall_diff))

overall_diff_mean
#Vergleichen der Gesamtdifferenzen zwischen N- und P-Gruppen mit dem Wilcoxon-Test:
wilcox_overall <- wilcox.test(overall_diff ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))

print(wilcox_overall)

median_diff_N <- median(risk_diff$overall_diff[risk_diff$framing == "N"])
median_diff_P <- median(risk_diff$overall_diff[risk_diff$framing == "P"])

print(median_diff_N)
print(median_diff_P)

# Der bereits durchgeführte Wilcoxon-Test für die "overall_diff"-Variable:
wilcox_overall <- wilcox.test(overall_diff ~ framing, data = risk_diff, subset = framing %in% c("P", "N"))

# W-Wert für "overall_diff":
W_overall <- wilcox_overall$statistic

# p-Wert für "overall_diff":
p_value_overall <- wilcox_overall$p.value

# Z-Wert Berechnung für "overall_diff":
Z_overall <- qnorm(p_value_overall / 2) # Für einen zweiseitigen Test
if(W_overall > n1 * (n1 + n2 + 1) / 2) {
  Z_overall = -Z_overall  # Die Richtung des Z-Wertes korrigieren
}

# Effektstärke Berechnung für "overall_diff":
r_overall <- Z_overall / sqrt(N)

cat("Z-Wert für overall_diff:", Z_overall, "\n")
cat("Effektstärke (r) für overall_diff:", r_overall, "\n")


