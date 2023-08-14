#Vorbereitung:

    # Hinzufügen einer Framing-Gruppe
      data_combined$framing <- ifelse(!is.na(data_combined$n_control_reading), "N",
                                ifelse(!is.na(data_combined$control_question), "P", "Neutral"))

    # Filtern Sie die Daten, um nur die gewünschten Gruppen zu behalten
      data_filtered <- data_combined[data_combined$framing != "Neutral", ]

#Versuch Auswertung Hypothese: Je höher die Technikbereitschaft (TB), desto weniger lassen sich Personen durch das Framing beeinflussen.

# Es soll eine Korrelation gerechnet werden. Je höher die TB höher, desto niedriger die Änderung der allgemeinen Risikowahrnehmung zwischen vor- und nach-Framing. 
 
# Daten für "risk_before_mit_Neutral" auswählen und Zeitpunkt hinzufügen
risk_before_mit_Neutral <- data_combined %>%
  select(ResponseId, Dest, Charging, Time, Accident, Price, Support, framing) %>%
  mutate(Zeit = "Vor")

# Daten für "risk_after_mit_Neutral" auswählen und Zeitpunkt hinzufügen
risk_after_mit_Neutral <- data_combined %>%
  select(ResponseId, Dest_2, Charging_2, Time_2, Accident_2, Price_2, Support_2, framing) %>%
  mutate(Zeit = "Nach")

# Aus risk_before_mit_Neutral und risk_after_mit_Neutral alle Neutralen filtern

risk_before_neutral_filtered <- risk_before_mit_Neutral[risk_before_mit_Neutral$framing != "Neutral", ]
risk_after_neutral_filtered <- risk_after_mit_Neutral[risk_after_mit_Neutral$framing != "Neutral", ]


# Zuerst den Mean von allen Aspekten bei Time_before und Time_after berechnen (weil allgemeine Risikowahrnehmung)

risk_after_durchschnitt_RW <- rowMeans(risk_after_neutral_filtered [, c(2:7)]) # Mean über Zeilen, sollte korrekt sein!
risk_after_durchschnitt_RW

risk_before_durchschnitt_RW <- rowMeans(risk_before_neutral_filtered [, c(2:7)])
risk_before_durchschnitt_RW

#die ColMeans werden als Value gespeichert --> für die Korrelation benötige ich sie aber in einem Datensatz (passend zur jeweiligen ResponseID)

# Jetzt kann die Änderung (Differenz) von den Mittelwerten vor-Framing und nach-Framing berechnet werden.
# WICHTIG: Wie subtrahiert man bestimmte Variablen von einzelnen Datensätzen? Damit ich nicht ResponseID und Time (vor/nach) löschen muss.
    
Differenz_before_after_durchschnitt_RW <- risk_before_durchschnitt_RW - risk_after_durchschnitt_RW

#folgender Schritt laut Prof. Arning falsch: Sie möchte mit negativen Zahlen!!!
## Wir benötigen keine negativen Zahlen, sondern nur die Änderung --> Daher den Betrag berechnen.

#Betrag_Differenz_before_after_durchschnitt_RW <- abs(Differenz_before_after_durchschnitt_RW)
#print(Betrag_Differenz_before_after_durchschnitt_RW)


# Differenz_before_after_durchschnitt_RW als Spalte in Datensatz data_filtered hinzufügen

data_filtered <- mutate(data_filtered, Änderung_RW = Differenz_before_after_durchschnitt_RW )
print(data_filtered)

# Jetzt kann die Korrelation berechnet werden.

jmv::corrMatrix(
  data = data_filtered,
  vars = vars(Tech_Interaction, Änderung_RW))

#--> p-Value nicht mehr signifikant (wenn man mit Betrag rechnet, ist er signifikant)


#-----------------------------------------------------------------------------

#Mit Betrag gerechnet:
 

#folgender Schritt laut Prof. Arning falsch: Sie möchte mit negativen Zahlen!!!
## Wir benötigen keine negativen Zahlen, sondern nur die Änderung --> Daher den Betrag berechnen.

Betrag_Differenz_before_after_durchschnitt_RW <- abs(Differenz_before_after_durchschnitt_RW)
print(Betrag_Differenz_before_after_durchschnitt_RW)


# Differenz_before_after_durchschnitt_RW als Spalte in Datensatz data_filtered hinzufügen

data_filtered <- mutate(data_filtered, Betrag_Änderung_RW = Betrag_Differenz_before_after_durchschnitt_RW )
print(data_filtered)

# Jetzt kann die Korrelation berechnet werden.

jmv::corrMatrix(
  data = data_filtered,
  vars = vars(Tech_Interaction, Betrag_Änderung_RW))
