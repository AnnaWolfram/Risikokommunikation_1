# Mann-Whitney-U-Test für ungepaarte Sichproben

# Dest
jmv::ttestIS(
  formula = diff_dest ~ framing,
  data = risk_diff,
  vars = "diff_dest",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Charging
jmv::ttestIS(
  formula = diff_charging ~ framing,
  data = risk_diff,
  vars = "diff_charging",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Time
jmv::ttestIS(
  formula = diff_time ~ framing,
  data = risk_diff,
  vars = "diff_time",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Price
jmv::ttestIS(
  formula = diff_price ~ framing,
  data = risk_diff,
  vars = "diff_price",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Support
jmv::ttestIS(
  formula = diff_support ~ framing,
  data = risk_diff,
  vars = "diff_support",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

# Accident
jmv::ttestIS(
  formula = diff_accident ~ framing,
  data = risk_diff,
  vars = "diff_accident",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

#Allgemeine Risikowahnehmung ----
# Summe der Differenzen für jede Person
risk_diff$overall_diff <- rowSums(risk_diff[,c("diff_dest", "diff_charging", "diff_time", "diff_accident", "diff_price", "diff_support")], na.rm = TRUE)
risk_diff$overall_diff
library(dplyr)

# Füge die overall_diff-Spalte aus risk_diff zu data_filtered hinzu, basierend auf der ResponseId-Variable
data_filtered <- data_filtered %>%
  left_join(select(risk_diff, ResponseId, overall_diff), by = "ResponseId")

# overall_diff
jmv::ttestIS(
  formula = overall_diff ~ framing,
  data = risk_diff,
  vars = "overall_diff",
  students = FALSE,
  mann = TRUE,
  norm = TRUE,
  qq = TRUE,
  eqv = TRUE,
  effectSize = TRUE,
  desc = TRUE)

