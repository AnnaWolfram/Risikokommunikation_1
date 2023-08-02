saveRDS(data_combined, "Daten/data_combined.rds")
##Datenauswertung

#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#installed.packages("tidyverse")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
install.packages("jmv")

library(hcictools)
library(tidyverse)
library(psych)
library(readxl)
library(careless)
source("qualtricshelpers.R")
library(jmv)
library(dplyr)




data_combined$framing <- ifelse(data_combined$framing == "positiv", "P", "N")

# 2. Umwandeln der Daten in das "long" Format
data_long <- data_combined %>%
  select(ID = ResponseId, framing, Dest, Dest_2) %>%
  gather(key = "time", value = "value", -ID, -framing) %>%
  mutate(time = factor(time, levels = c("Dest", "Dest_2")))

library(afex)
mixed_anova_result <- aov_ez(data_long, dv = "value", id = "ID", between = "framing", within = "time")
print(mixed_anova_result)










# Überprüfen Sie die eindeutigen Werte in den Spalten 'data_positiv' und 'data_negativ'
unique(data_combined$data_positiv)
unique(data_combined$data_negativ)

# Erstellen Sie eine temporäre Spalte, um zu sehen, wie die Bedingung funktioniert
data_combined <- data_combined %>%
  mutate(temp_framing = ifelse(data_positiv == 1, "P", "N"))

# Überprüfen Sie die eindeutigen Werte in der temporären Spalte
unique(data_combined$temp_framing)

# Überprüfen Sie die Länge der temporären Spalte
length(data_combined$temp_framing)




















# Laden der benötigten Bibliotheken
library(tidyverse)

# Lesen des kombinierten Datensatzes
data_combined <- readRDS("Daten/data_combined.rds")

# Umformung der Daten in das "long" Format
data_long <- data_combined %>%
  gather(key = "time", value = "Dest", Dest, Dest_2) %>%
  mutate(time = factor(time, levels = c("Dest", "Dest_2")))

# Überprüfung auf fehlende Werte
na_check <- any(is.na(data_long$Dest))
if (na_check) {
  warning("Es gibt fehlende Werte in der abhängigen Variable.")
}

# Überprüfung der Normalverteilung
shapiro_test <- shapiro.test(data_combined$Dest)
if (shapiro_test$p.value < 0.05) {
  warning("Die Daten sind nicht normalverteilt.")
}



# Überprüfung der Homogenität der Varianz
levene_test <- car::leveneTest(Dest ~ framing * time, data = data_long)
if (levene_test$p.value < 0.05) {
  warning("Die Varianz ist nicht homogen.")
}










#Neue Gruppe erstellen

data_framing <- bind_rows(data_positiv, data_negativ)


library(ez)

# Lesen Sie den Datensatz ein
data_combined <- readRDS("Daten/data_combined.rds")


# Ändern Sie die Spaltennamen
names(data_combined)[names(data_combined) == "concern_dest"] <- "concern_dest_before"
names(data_combined)[names(data_combined) == "2concern_dest"] <- "concern_dest_after"

# Daten umstrukturieren
long_data <- data_combined %>%
  pivot_longer(cols = c("concern_dest_before", "concern_dest_after"),
               names_to = "time",
               names_prefix = "concern_dest_",
               values_to = "concern_dest")

long_data

# Überprüfen Sie die eindeutigen Werte in der Spalte "time"
print(unique(long_data$time))

# Ersetzen Sie NA-Werte durch den Durchschnitt der vorhandenen Werte
long_data$concern_dest[is.na(long_data$concern_dest)] <- mean(long_data$concern_dest, na.rm = TRUE)

# Führen Sie die gemischte ANOVA durch
result <- ezANOVA(
  data = long_data,
  dv = concern_dest,
  wid = ResponseId,
  within = .(time),
  detailed = TRUE
)
print(result)


# Entfernen Sie Zeilen mit NA-Werten
long_data <- long_data[!is.na(long_data$concern_dest), ]

# Führen Sie die gemischte ANOVA durch
result <- ezANOVA(
  data = long_data,
  dv = concern_dest,
  wid = ResponseId,
  within = .(time),
  detailed = TRUE
)
print(result)


# Ändern Sie die Spaltennamen
names(data_positiv)[names(data_positiv) == "concern_dest"] <- "concern_dest_before"
names(data_positiv)[names(data_positiv) == "2concern_dest"] <- "concern_dest_after"

# Daten umstrukturieren
long_data_pos <- data_positiv %>%
  pivot_longer(cols = c("concern_dest_before", "concern_dest_after"),
               names_to = "time",
               names_prefix = "concern_dest_",
               values_to = "concern_dest")

# Überprüfen Sie die eindeutigen Werte in der Spalte "time"
print(unique(long_data$time))

# Ersetzen Sie NA-Werte durch den Durchschnitt der vorhandenen Werte
long_data_pos$concern_dest[is.na(long_data_pos$concern_dest)] <- mean(long_data_pos$concern_dest, na.rm = TRUE)

# Führen Sie die gemischte ANOVA durch
result <- ezANOVA(
  data = long_data,
  dv = concern_dest,
  wid = ResponseId,
  within = .(time),
  detailed = TRUE
)
print(result)



# To do for Anna

# suchen wie ich eine Anova mache --> Vorlage altes Forschungsseminar
# Hypothesen sind im Dokument
