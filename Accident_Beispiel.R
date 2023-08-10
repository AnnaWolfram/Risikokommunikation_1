#Pakete aktivieren
library(hcictools)
library(tidyverse)
library(psych)
library(readxl)
library(careless)
source("qualtricshelpers.R")
library(jmv)
library(dplyr)
library(rstatix)
library(car)
library(ez)
library(nortest)
library(ggpubr)





# Umwandeln der Daten in das "long" Format für Accident und Accident_2
data_long_accident <- data_filtered %>%
  select(ID = ResponseId, framing, Accident, Accident_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Accident", "Accident_2")))

# 1. Auf Ausreißer prüfen
boxplot(data_filtered$Accident, data_filtered$Accident_2, main="Check for Outliers", names=c("Accident", "Accident_2"))

data_filtered %>%
  select(ID = ResponseId, framing, Accident, Accident_2) %>%
  pivot_longer(cols = c(Accident, Accident_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# 2. Normalverteilung prüfen
shapiro.test(data_filtered$Accident)
shapiro.test(data_filtered$Accident_2)

# Erstellen eines QQ-Plots
qq_plot <- ggqqplot(data_long, "value", ggtheme = theme_bw()) +
  facet_grid(time ~ framing)

print(qq_plot)

#Für Dest zusammengefasst ohne Unterteilung in Framing-Gruppen ----

# Erstellen eines QQ-Plots für Aaccident
qq_plot_accident <- ggqqplot(data_filtered, "Accident", ggtheme = theme_bw())
qq_plot_accident

# Erstellen eines QQ-Plots für Accident
qq_plot_accident_2 <- ggqqplot(data_filtered, "Accident_2", ggtheme = theme_bw())
qq_plot_accident_2

# 3. Überprüfung der Homogenität der Varianz (Levene-Test)
leveneTest(Accident ~ framing, data = data_filtered)
leveneTest(Accident_2 ~ framing, data = data_filtered)



# Gemischte ANOVA für Accident und Accident_2
mixed_anova_result_accident <- aov_ez(data_long_accident, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result_accident)


#Post-Hoc Tests

# Effekt von framing zu jedem Zeitpunkt
one.way_framing <- data_long_accident %>%
  group_by(time) %>%
  anova_test(dv = value, wid = ID, between = framing) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_framing

#Vergleich zwischen framing-ebenen 
pwc_framing <- data_long_accident %>%
  group_by(time) %>%
  pairwise_t_test(value ~ framing, p.adjust.method = "bonferroni")
pwc_framing

# Effekt des Zeitpunkts zu jedem Framing-Level
one.way_time <- data_long_accident %>%
  group_by(framing) %>%
  anova_test(dv = value, wid = ID, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_time

# Differenz zwischen den Zeiten für jeden Teilnehmer berechnen
data_long_accident <- data_long_accident %>%
  spread(key = time, value = value) %>%
  mutate(diff_value = Accident_2 - Accident)

# t-Test, um die Differenzen zwischen den Framing-Gruppen zu vergleichen
diff_t_test <- t.test(diff_value ~ framing, data = data_long_accident)
print(diff_t_test)

