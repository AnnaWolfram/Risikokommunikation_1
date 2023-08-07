
#Hypothese Einfluss von Erfahrung ---- 
#       Personen, die bereits Erfahrung mit Elektroautos haben (Vorwissen), 
#       lassen sich weniger beeinflussen als Personen ohne Erfahrung

#install.packages("lme4")
library(Matrix)
library(lme4)
library(dplyr)

# Daten für "risk_before" auswählen und Zeitpunkt hinzufügen
risk_before <- data_combined %>%
  select(ResponseId, Dest, Charging, Time, Accident, Price, Support) %>%
  mutate(Zeit = "Vor")

# Daten für "risk_after" auswählen und Zeitpunkt hinzufügen
risk_after <- data_combined %>%
  select(ResponseId, Dest_2, Charging_2, Time_2, Accident_2, Price_2, Support_2) %>%
  mutate(Zeit = "Nach")

# Verknüpfung von risk_before und risk_after mit data_combined
risk_before <- left_join(risk_before, data_combined[, c("ResponseId", "prior_knowledge")], by = "ResponseId")
risk_after <- left_join(risk_after, data_combined[, c("ResponseId", "prior_knowledge")], by = "ResponseId")

# Lineare Regressionen
# Berechnung der Mittelwerte von risk_before-Variablen
risk_before_mean <- data_combined %>%
  group_by(prior_knowledge) %>%
  summarize(
    Dest_mean = mean(coalesce(concern_dest, impact_dest, prob_dest), na.rm = TRUE),
    Charging_mean = mean(coalesce(concern_charging, impact_charging, prob_charging_n), na.rm = TRUE),
    Time_mean = mean(coalesce(concern_time, impact_time, prob_time), na.rm = TRUE),
    Accident_mean = mean(coalesce(concern_accident, impact_accident, prob_accident), na.rm = TRUE),
    Price_mean = mean(coalesce(concern_price, impact_price, prob_price), na.rm = TRUE),
    Support_mean = mean(coalesce(concern_support, impact_support, prob_support), na.rm = TRUE)
  )
print(risk_before_mean)

# Berechnung der Mittelwerte von risk_after-Variablen
risk_after_mean <- data_combined %>%
  group_by(prior_knowledge) %>%
  summarize(
    Dest_2_mean = mean(coalesce(`2concern_dest`, `2impact_dest`, `2prob_dest`), na.rm = TRUE),
    Charging_2_mean = mean(coalesce(`2concern_charging`, `2impact_charging`, `2prob_charging`), na.rm = TRUE),
    Time_2_mean = mean(coalesce(`2concern_time`, `2impact_time`, `2prob_time`), na.rm = TRUE),
    Accident_2_mean = mean(coalesce(`2concern_accident`, `2impact_accident`, `2prob_accident`), na.rm = TRUE),
    Price_2_mean = mean(coalesce(`2concern_price`, `2impact_price`, `2prob_price`), na.rm = TRUE),
    Support_2_mean = mean(coalesce(`2concern_support`, `2impact_support`, `2prob_support`), na.rm = TRUE)
  )
print(risk_after_mean)

# Exportieren der Mittelwerte von risk_before in eine CSV-Datei
write.csv(risk_before_mean, "Daten/risk_before_mean.csv", row.names = FALSE)

# Exportieren der Mittelwerte von risk_after in eine CSV-Datei
write.csv(risk_after_mean, "Daten/risk_after_mean.csv", row.names = FALSE)


# Visualisierung

library(ggplot2)

# Daten für das Balkendiagramm erstellen
risk_before_mean <- risk_before_mean %>% 
  mutate(Text = "Vor Framing")
risk_after_mean <- risk_after_mean %>% 
  mutate(Text = "Nach Framing")

# Auswahl der relevanten Spalten und Umbenennung
risk_before_mean <- risk_before_mean %>%
  select(prior_knowledge, Dest_mean, Charging_mean, Time_mean, Accident_mean, Price_mean, Support_mean, Text) %>%
  rename(Dest = Dest_mean, Charging = Charging_mean, Time = Time_mean, Accident = Accident_mean, Price = Price_mean, Support = Support_mean)

risk_after_mean <- risk_after_mean %>%
  select(prior_knowledge, Dest_2_mean, Charging_2_mean, Time_2_mean, Accident_2_mean, Price_2_mean, Support_2_mean, Text) %>%
  rename(Dest = Dest_2_mean, Charging = Charging_2_mean, Time = Time_2_mean, Accident = Accident_2_mean, Price = Price_2_mean, Support = Support_2_mean)

# Zusammenführen der Daten
data_combined_plot <- rbind(risk_before_mean, risk_after_mean)

# Laden der Bibliothek ggplot2, falls noch nicht installiert
#install.packages("ggplot2")
library(ggplot2)

# Balkendiagramm erstellen
ggplot(data_combined_plot, aes(x = prior_knowledge, y = Dest, fill = Text)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Balkendiagramm der Risikowahrnehmung vor und nach Framing",
       x = "Vorwissen",
       y = "Risikowahrnehmung (Destination)") +
  theme_minimal()


# Positives Framing (am Bsp. von Dest)----
# Vorher
model_pos_before <- lm(Dest ~ prior_knowledge, data = data_positiv)
summary(model_pos_before)

# Nachher
model_pos_after <- lm(Dest_2 ~ prior_knowledge, data = data_positiv)
summary(model_pos_after)


# Negatives Framing ----
# Vorher
model_neg_before <- lm(Dest ~ prior_knowledge, data = data_negativ)
summary(model_neg_before)

# Nachher
model_neg_after <- lm(Dest_2 ~ prior_knowledge, data = data_negativ)
summary(model_neg_after)

# Hypothese Einfluss von Geschlecht ----
# Laden der benötigten Bibliotheken
library(dplyr)
library(ggplot2)
library(stats)
library(car)

# Lineare Regression für dest_before
model_before_dest <- lm(Dest ~ gender, data = data_combined)
summary(model_before_dest)

#ANOVA für dest_before
anova_before_dest <- aov(Dest ~ gender, data = data_combined)
summary(anova_before_dest)

# Daten Risikowahrnehmung_before zusammensetzen und umbenennen
data_combined <- data_combined %>%
  mutate(risk_before = (Dest + Charging + Time + Accident + Price + Support) / 6)

# ANOVA für risk_before
anova_result_before <- aov(risk_before ~ gender, data = data_combined)

# ANOVA-Ergebnisse risk_before
summary(anova_result_before)

# Daten Risikowahrnehmung_after zusammensetzen und umbenennen
data_combined <- data_combined %>%
  mutate(risk_after = (Dest_2 + Charging_2 + Time_2 + Accident_2 + Price_2 + Support_2) / 6)

# ANOVA für risk_after
anova_result_after <- aov(risk_after ~ gender, data = data_combined)

# ANOVA-Ergebnisse risk_after
summary(anova_result_after)



# Hypothese für Tech