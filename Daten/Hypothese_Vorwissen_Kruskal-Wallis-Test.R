#Pakete laden
library(ggplot2)
library(dplyr)
library(rstatix)

# Kruskal-Wallis-Test 
kw_result <- data_filtered %>%
  group_by(prior_knowledge) %>%
  kruskal_test(Änderung_RW ~ prior_knowledge)

# Zusammenfassung der Ergebnisse
summary(kw_result)

# Post-hoc-Tests, da signifikante Unterschiede festgestellt wurden
posthoc_results <- kw_result %>%
  tukey_hsd(Änderung_RW ~ Vorwissen)

# Post-hoc-Test Ergebnisse
print(posthoc_results)

# Boxplot-Darstellung der Daten
ggplot(daten, aes(x = prior_knowledge, y = Änderung_RW)) +
  geom_boxplot() +
  labs(x = "prior_knowledge", y = "Änderung_RW") +
  geom_signif(comparisons = posthoc_results)


data_filtered$prior_knowledge
