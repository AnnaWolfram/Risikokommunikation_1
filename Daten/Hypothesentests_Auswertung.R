saveRDS(data_combined, "Daten/data_combined.rds")
##Datenauswertung

#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#installed.packages("tidyverse")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")

library(hcictools)
library(tidyverse)
library(psych)
library(readxl)
library(careless)
source("qualtricshelpers.R")

# Laden Sie das ez Paket
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
