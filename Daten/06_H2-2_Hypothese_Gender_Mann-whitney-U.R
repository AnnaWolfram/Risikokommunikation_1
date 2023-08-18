# Hypothese Frauen lassen sich eher beeinflussen als Männer.

# Daten filtern
data_filtered <- data_filtered[data_filtered$gender %in% c("männlich", "weiblich"), ] #hier wird eine Person aus dem Datensatz gefiltert

# t-Test durchführen
jmv::ttestIS(
  formula = overall_diff ~ gender,
  data = data_filtered,
  vars = "overall_diff",
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  desc = TRUE
)

# --> Levene's Test schlägt aus (Verletzung Annahme gleicher Varianzen) und Shapiro-Wilk-Test schlägt aus (Verletzung Normalverteilung)

# Da Voraussetzung für parametrisches Verfahren (t-Test) nicht erfüllt sind, wird Mann-Whitney-U-Test angewandt
jmv::ttestIS(
  formula = overall_diff ~ gender,
  data = data_filtered,
  vars = "overall_diff",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)
