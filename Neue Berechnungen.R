# Neue Berechnungen der Hypothesen

# Pakete laden
library(tidyverse)

# Hypothese: Vorwissen hat einfluss auf Risikowahrnehmung ----

# Daten einlesen und überprüfen
data_combined <- readRDS("Daten/data_combined.rds")

# Mapping der Textwerte zu numerischen Werten
prior_knowledge_mapping <- c("Ich könnte ihre Funktionsweise im Detail erklären." = 1,
                             "Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren." = 2,
                             "Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos." = 3,
                             "Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen." = 4,
                             "Ich habe keine Kenntnisse über Elektrofahrzeuge." = 5)

# Wende die Umwandlung auf prior_knowledge an
data_combined$prior_knowledge_numeric <- prior_knowledge_mapping[data_combined$prior_knowledge]

# Überprüfe, ob die Umwandlung korrekt ist
sample_rows <- sample(nrow(data_combined), 5)
selected_data <- data_combined[sample_rows, c("prior_knowledge", "prior_knowledge_numeric")]
print(selected_data)

# Kurzer Test, ob es auch richtig konvertiert wurde
sample_rows <- sample(nrow(data_combined), 5)
selected_data <- data_combined[sample_rows, c("prior_knowledge", "prior_knowledge_numeric")]
print(selected_data)

# ANOVA Dest
anova_Dest_pre <- aov(Dest ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Dest_pre)

anova_Dest_post <- aov(Dest_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Dest_post)


# ANOVA Charging
anova_Charging_pre <- aov(Charging ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Charging_pre)

anova_Charging_post <- aov(Charging_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Charging_post)


# ANOVA Time
anova_Time_pre <- aov(Time ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Time_pre)

anova_Time_post <- aov(Time_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Time_post)


# ANOVA Accident
anova_Accident_pre <- aov(Accident ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Accident_pre)

anova_Accident_post <- aov(Accident_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Accident_post)


# ANOVA Price
anova_Price_pre <- aov(Price ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Price_pre)

anova_Price_post <- aov(Price_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Price_post)


# ANOVA Support
anova_Support_pre <- aov(Support ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Support_pre)

anova_Support_post <- aov(Support_2 ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Support_post)



# Berechnung der Risikowahrnehmung vor und nach dem Framing
data_combined$Risiko_pre <- rowMeans(data_combined[, c("Dest", "Charging", "Time", "Accident", "Price", "Support")])
data_combined$Risiko_post <- rowMeans(data_combined[, c("Dest_2", "Charging_2", "Time_2", "Accident_2", "Price_2", "Support_2")])

# ANOVA Risiko_pre
anova_Risiko_pre <- aov(Risiko_pre ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Risiko_pre)

# ANOVA Risiko_post
anova_Risiko_post <- aov(Risiko_post ~ prior_knowledge_numeric, data = data_combined)
summary(anova_Risiko_post)


#Visualisierung----
# Pakete laden
library(ggplot2)
library(tidyr)

# Boxplot für Risiko_pre
ggplot(data_combined, aes(x = prior_knowledge_numeric, y = Risiko_pre)) +
  geom_boxplot() +
  labs(x = "Vorwissen", y = "Risiko_pre") +
  ggtitle("Boxplot für Risiko_pre nach Vorwissen")

# Boxplot für Risiko_post
ggplot(data_combined, aes(x = prior_knowledge_numeric, y = Risiko_post)) +
  geom_boxplot() +
  labs(x = "Vorwissen", y = "Risiko_post") +
  ggtitle("Boxplot für Risiko_post nach Vorwissen")

# Daten in ein längeres Format umwandeln
data_long <- data_combined %>%
  pivot_longer(cols = c(Risiko_pre, Risiko_post), names_to = "Zeitpunkt", values_to = "Risiko") %>%
  mutate(Zeitpunkt = sub("Risiko_", "", Zeitpunkt))

# Boxplots nebeneinander platzieren
ggplot(data_long, aes(x = factor(prior_knowledge_numeric), y = Risiko, fill = Zeitpunkt)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(x = "Vorwissen", y = "Risiko") +
  ggtitle("Boxplot für Risiko_pre und Risiko_post nach Vorwissen") +
  scale_fill_manual(name = "Zeitpunkt", values = c("Risiko_pre" = "#999999", "Risiko_post" = "#CCCCCC")) +
  theme_minimal() +
  facet_grid(~ Zeitpunkt, scales = "free_x", space = "free_x")


# Versuch mit T-Test ----

# Neue Variable erstellen, in der Vorwissen aus nur 2 levels besteht und diese konvertieren
data_combined$prior_knowledge_2_level <- ifelse(data_combined$prior_knowledge_numeric %in% c(1, 2, 3), 1, 2)
data_combined$prior_knowledge_2_level <- as.numeric(data_combined$prior_knowledge_2_level)


# T-Test für Variable "Dest"
t_test_Dest_pre <- t.test(Dest ~ prior_knowledge_2_level, data = data_combined)
t_test_Dest_post <- t.test(Dest_2 ~ prior_knowledge_2_level, data = data_combined)

# T-Test für Variable "Charging"
t_test_Charging_pre <- t.test(Charging ~ prior_knowledge_2_level, data = data_combined)
t_test_Charging_post <- t.test(Charging_2 ~ prior_knowledge_2_level, data = data_combined)

# T-Test für Variable "Time"
t_test_Time_pre <- t.test(Time ~ prior_knowledge_2_level, data = data_combined)
t_test_Time_post <- t.test(Time_2 ~ prior_knowledge_2_level, data = data_combined)

# T-Test für Variable "Accident"
t_test_Accident_pre <- t.test(Accident ~ prior_knowledge_2_level, data = data_combined)
t_test_Accident_post <- t.test(Accident_2 ~ prior_knowledge_2_level, data = data_combined)

# T-Test für Variable "Price"
t_test_Price_pre <- t.test(Price ~ prior_knowledge_2_level, data = data_combined)
t_test_Price_post <- t.test(Price_2 ~ prior_knowledge_2_level, data = data_combined)

# T-Test für Variable "Support"
t_test_Support_pre <- t.test(Support ~ prior_knowledge_2_level, data = data_combined)
t_test_Support_post <- t.test(Support_2 ~ prior_knowledge_2_level, data = data_combined)



#Ergebnisse anzeigen lassen

print("T-Test Ergebnisse für Variable Dest (pre):")
print(t_test_Dest_pre)
print("T-Test Ergebnisse für Variable Dest (post):")
print(t_test_Dest_post)

print("T-Test Ergebnisse für Variable Charging (pre):")
print(t_test_Charging_pre)
print("T-Test Ergebnisse für Variable Charging (post):")
print(t_test_Charging_post)

print("T-Test Ergebnisse für Variable Time (pre):")
print(t_test_Time_pre)
print("T-Test Ergebnisse für Variable Time (post):")
print(t_test_Time_post)

print("T-Test Ergebnisse für Variable Accident (pre):")
print(t_test_Accident_pre)
print("T-Test Ergebnisse für Variable Accident (post):")
print(t_test_Accident_post)

print("T-Test Ergebnisse für Variable Price (pre):")
print(t_test_Price_pre)
print("T-Test Ergebnisse für Variable Price (post):")
print(t_test_Price_post)

print("T-Test Ergebnisse für Variable Support (pre):")
print(t_test_Support_pre)
print("T-Test Ergebnisse für Variable Support (post):")
print(t_test_Support_post)



# Liste der p-Werte für T-Tests pre-Variablen
p_values_ttest_pre <- c(t_test_Dest_pre$p.value, 
                        t_test_Charging_pre$p.value,
                        t_test_Time_pre$p.value,
                        t_test_Accident_pre$p.value,
                        t_test_Price_pre$p.value,
                        t_test_Support_pre$p.value)

# Liste der p-Werte für T-Tests post-Variablen
p_values_ttest_post <- c(t_test_Dest_post$p.value, 
                         t_test_Charging_post$p.value,
                         t_test_Time_post$p.value,
                         t_test_Accident_post$p.value,
                         t_test_Price_post$p.value,
                         t_test_Support_post$p.value)



# p-Werte in numerische Werte konvertieren
numeric_p_values_ttest_pre <- as.numeric(p_values_ttest_pre)
numeric_p_values_ttest_post <- as.numeric(p_values_ttest_post)

# Durchschnitt der p-Werte für T-Tests pre- und post-Variablen
average_p_ttest_pre <- mean(numeric_p_values_ttest_pre, na.rm = TRUE)
average_p_ttest_post <- mean(numeric_p_values_ttest_post, na.rm = TRUE)

# Durchschnitts p-Werte
cat("Durchschnittsp-Wert für Risiko_pre_gesamt (T-Tests):", average_p_ttest_pre, "\n")
cat("Durchschnittsp-Wert für Risiko_post_gesamt (T-Tests):", average_p_ttest_post, "\n")
