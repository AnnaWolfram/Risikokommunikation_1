# Hypothese Vorwissen mit Mann-Whitney-U-Test

library(dplyr)
library(stats)

# Mapping des Vorwissens
vorwissen_mapping <- c(
  "Ich könnte ihre Funktionsweise im Detail erklären." = 1,
  "Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren." = 2,
  "Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos." = 3,
  "Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen." = 4,
  "Ich habe keine Kenntnisse über Elektrofahrzeuge." = 5
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


# Lade das WRS2-Paket
library(WRS2)

# Führe eine robuste ANOVA durch
robust_anova_result <- anovaBF(Änderung_RW ~ prior_knowledge_numeric, data = data_filtered, whichRandom = "ID")







