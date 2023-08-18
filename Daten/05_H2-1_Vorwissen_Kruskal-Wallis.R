# Hypothese Vorwissen mit Mann-Whitney-U-Test

#Kruskal-Wallis
jmv::anovaNP(
  formula = overall_diff ~ prior_knowledge,
  data = data_filtered,
  es = TRUE,
  pairs = FALSE)



