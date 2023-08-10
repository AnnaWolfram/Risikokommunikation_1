#Beispielhypothese:
# Inwieweit beeinflusst die positiv- oder negativ-geframte Darstellung (framing) verschiedener 
# Risikoparameter (Hier:Reichweite - Dest und Dest_2) von E-Mobilität deren Beurteilung/Risikowahrnehmung?

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

# Alle Outlier prüfen ----
# Identify outliers for Dest and Dest_2
data_filtered %>%
  select(ID = ResponseId, framing, Dest, Dest_2) %>%
  pivot_longer(cols = c(Dest, Dest_2), names_to = "time", values_to = "value") %>%
  group_by(time, framing) %>%
  identify_outliers(value)

# Anderson-Darling-Test für die Variable "Dest" --> Normalverteilung ----
ad_test_dest <- ad.test(data_filtered$Dest)
print(ad_test_dest)

# Anderson-Darling-Test für die Variable "Dest_2" --> Normalverteilung
ad_test_dest_2 <- ad.test(data_filtered$Dest_2)
print(ad_test_dest_2)

# Erstellen eines QQ-Plots
qq_plot <- ggqqplot(data_long, "value", ggtheme = theme_bw()) +
  facet_grid(time ~ framing)

print(qq_plot)

#Für Dest zusammengefasst ohne Unterteilung in Framing-Gruppen ----

# Erstellen eines QQ-Plots für Dest
qq_plot_dest <- ggqqplot(data_filtered, "Dest", ggtheme = theme_bw())

# Erstellen eines QQ-Plots für Dest_2
qq_plot_dest_2 <- ggqqplot(data_filtered, "Dest_2", ggtheme = theme_bw())

# Die beiden QQ-Plots anzeigen
print(qq_plot_dest)
print(qq_plot_dest_2)

#Plot für Normalverteilung Dest ----
qqnorm(data_filtered$Dest)
qqline(data_filtered$Dest)

#Plot für Normalverteilung Dest_2
qqnorm(data_filtered$Dest_2)
qqline(data_filtered$Dest_2)

#Umwandeln der Daten in das "long" Format für Dest und Dest_2 ----
data_long <- data_filtered %>%
  select(ID = ResponseId, framing, Dest, Dest_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Dest", "Dest_2")))

#Überprüfung der Homogenität der Varianz (Levene-Test) ----
leveneTest(Dest ~ framing, data = data_filtered)
leveneTest(Dest_2 ~ framing, data = data_filtered)



# Gemischte ANOVA für Dest und Dest_2 ----
library(afex)
mixed_anova_result <- aov_ez(data_long, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result)


#Haupteffekte überprüfen mittels T-Test
data_long %>%
  pairwise_t_test(
    value ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

data_long %>%
  pairwise_t_test(
    value ~ framing, 
    p.adjust.method = "bonferroni"
  )
