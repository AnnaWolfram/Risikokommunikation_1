#### Hypothese Frauen lassen sich eher beeinflussen als Männer.

# Daten filtern, um nur "männlich" und "weiblich" zu behalten
data_filtered <- data_filtered[data_filtered$gender %in% c("männlich", "weiblich"), ] #hier wird eine Person aus dem Datensatz gefiltert!

# t-Test durchführen
jmv::ttestIS(
  formula = Änderung_RW ~ gender,
  data = data_filtered,
  vars = "Änderung_RW",
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  desc = TRUE
)

# --> Levene's Test schlägt aus (Verletzung Annahme gleicher Varianzen) und Shapiro-Wilk-Test schlägt aus (Verletzung Normalverteilung)

# Da Voraussetzung für parametrisches Verfahren (t-Test) nicht erfüllt sind, wird Mann-Whitney-U-Test angewandt
jmv::ttestIS(
  formula = Änderung_RW ~ gender,
  data = data_filtered,
  vars = "Änderung_RW",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  desc = TRUE)

# --> Ergebnis nicht Signifikant

# Wir könnten generell noch die Teststärke berechnen und prüfen, ob unsere Stichprobe groß genug war, wenn nicht wär das ein Punkt für die Diskussion

