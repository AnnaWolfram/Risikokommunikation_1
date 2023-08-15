#Pakete laden
library(ggplot2)
library(dplyr)
library(rstatix)

# Kruskal-Wallis-Test 
kw_result <- data_filtered %>%
  group_by(prior_knowledge) %>%
  kruskal_test(overall_diff ~ prior_knowledge)

# Zusammenfassung der Ergebnisse
summary(kw_result)

# Post-hoc-Tests, da signifikante Unterschiede festgestellt wurden
posthoc_results <- kw_result %>%
  tukey_hsd(overall_diff ~ Vorwissen)

# Post-hoc-Test Ergebnisse
print(posthoc_results)

# Boxplot-Darstellung der Daten
ggplot(daten, aes(x = prior_knowledge, y = overall_diff)) +
  geom_boxplot() +
  labs(x = "prior_knowledge", y = "Änderung_RW") +
  geom_signif(comparisons = posthoc_results)


data_filtered$prior_knowledge



# Kruskal-Wallis-Anova

jmv::anovaNP(
  formula = overall_diff ~ prior_knowledge,
  data = data_filtered,
  es = TRUE,
  pairs = TRUE)

# es = Effektstärke // pairs = paarweise Vergleiche nach DSCF