# Hypothese Vorwissen mit Mann-Whitney-U-Test

library(dplyr)
library(stats)

# Mapping des Vorwissens
vorwissen_mapping <- c(
  "Ich könnte ihre Funktionsweise im Detail erklären." = 1,
  "Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren." = 1,
  "Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos." = 1,
  "Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen." = 2,
  "Ich habe keine Kenntnisse über Elektrofahrzeuge." = 2
)

# Anpassung der prior_knowledge_numeric Spalte
data_filtered <- data_filtered %>%
  mutate(prior_knowledge_numeric = vorwissen_mapping[prior_knowledge])

# Daten in zwei Gruppen aufteilen
group1 <- data_filtered %>%
  filter(prior_knowledge_numeric == 1) %>%
  select(Änderung_RW)

group2 <- data_filtered %>%
  filter(prior_knowledge_numeric == 2) %>%
  select(Änderung_RW)

# Durchführung des Mann-Whitney-U-Tests
mwu_result <- wilcox.test(group1$Änderung_RW, group2$Änderung_RW)

# Anzeigen der Ergebnisse
print(mwu_result)

# Robuste ANOVA mit oneway_test --------------------------------------
# Lade das coin-Paket
#install.packages("coin")
library(coin)

data_filtered$prior_knowledge_numeric <- as.factor(data_filtered$prior_knowledge_numeric)
is.factor(data_filtered$prior_knowledge_numeric)

# Robuste ANOVA mit oneway_test
robust_anova_one_way_result <- oneway_test(Änderung_RW ~ prior_knowledge_numeric, data = data_filtered)
print(robust_anova_one_way_result)

# Robuste ANOVA --------------------------------------
# Lade das Paket lmrob
#install.packages("robustbase")
library(robustbase)

# Robuste ANOVA
robust_anova_result <- lmrob(Änderung_RW ~ prior_knowledge_numeric, data = data_filtered)

# Zusammenfassung der Ergebnisse
print(robust_anova_result)
summary(robust_anova_result)
