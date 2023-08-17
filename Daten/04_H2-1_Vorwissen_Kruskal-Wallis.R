# Hypothese Vorwissen mit Mann-Whitney-U-Test

# Laden der erforderlichen Pakete
#install.packages("coin")
#install.packages("WRS2")
library(coin)
library(WRS2)
library(dplyr)
library(stats)

#Kruskal-Wallis
jmv::anovaNP(
  formula = overall_diff ~ prior_knowledge,
  data = data_filtered,
  es = TRUE,
  pairs = FALSE)

#alt _________________________________________________________________________________
# Mapping des Vorwissens
vorwissen_mapping <- c(
  "Ich könnte ihre Funktionsweise im Detail erklären." = 1,
  "Ich habe eine relativ klare Vorstellung, wie E-Autos funktionieren." = 2,
  "Ich habe eine ungefähre Vorstellung über die Funktionsweise von E-Autos." = 3,
  "Ich habe schon einmal von E-Autos gehört, kann aber nichts darüber sagen." = 4,
  "Ich habe keine Kenntnisse über Elektrofahrzeuge." = 5
)

# Laden der erforderlichen Pakete
#install.packages("coin")
#install.packages("WRS2")
library(coin)
library(WRS2)

# Durchführung des Kruskal-Wallis-Tests
kruskal_result <- kruskal_test(overall_diff ~ prior_knowledge_numeric, data = data_filtered)

# Ausgabe der Ergebnisse
print("Kruskal-Wallis-Test:")
print(kruskal_result)


