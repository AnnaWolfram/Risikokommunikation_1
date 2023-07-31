# Ergebnisse

# Pakete laden
# install.packages("ez")

library(ez)

# Annahme: Ihre Daten sind in einem Datenrahmen mit dem Namen "datenrahmen" gespeichert.
# Verwenden Sie den richtigen Dateipfad und das Dateiformat für Ihre Daten.
datenrahmen <- read.csv("pfad_zur_datei.csv")

# Anzeige der ersten Zeilen des Datenrahmens
head(datenrahmen)

# Zusammenfassung der Daten
summary(datenrahmen)

# Häufigkeiten einer kategorialen Variable
table(datenrahmen$variable)

# Kreuztabelle zweier kategorialer Variablen
table(datenrahmen$variable1, datenrahmen$variable2)

# Mittelwert einer metrischen Variable
mean(datenrahmen$variable)

# Standardabweichung einer metrischen Variable
sd(datenrahmen$variable)

# Median einer metrischen Variable
median(datenrahmen$variable)

# Quartile einer metrischen Variable
quantile(datenrahmen$variable)

# Streudiagramm zweier metrischer Variablen
plot(datenrahmen$variable1, datenrahmen$variable2)

# Boxplot einer metrischen Variable
boxplot(datenrahmen$variable)

# Histogramm einer metrischen Variable
hist(datenrahmen$variable)
