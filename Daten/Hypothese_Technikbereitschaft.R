#Versuch Auswertung Hypothese: Je höher die Technikbereitschaft (TB), desto weniger lassen sich Personen durch das Framing beeinflussen.

# Es soll eine Korrelation gerechnet werden. Je höher die TB höher, desto niedriger die Änderung der allgemeinen Risikowahrnehmung zwischen vor- und nach-Framing. 

# Zuerst den Mean von allen Aspekten bei Time_before und Time_after berechnen (weil allgemeine Risikowahrnehmung)
risk_before_durchschnitt_Risikowahrnehmung <- colMeans(risk_before [, 2:7]) #falsch!

risk_after_durchschnitt_Risikowahrnehmung <- colMeans(risk_after[, 2:7]) #falsch! 

risk_after_durchschnitt_risiko_test <- rowMeans(risk_after [, c(2:7)]) # Mean über Zeilen, sollte korrekt sein!
risk_after_durchschnitt_risiko_test

#ACHTUNG: in risk_before ist die falsche Time variable (vor und nicht mean) dies muss behoben werden bevor ich weiterrechne.
#die ColMeans werden als Value gespeichert --> für die Korrelation benötige ich sie aber in einem Datensatz (passend zur jeweiligen ResponseID)

# Jetzt kann die Änderung (Differenz) von den Mittelwerten vor-Framing und nach-Framing berechnet werden.
# WICHTIG: Wie subtrahiert man bestimmte Variablen von einzelnen Datensätzen? Damit ich nicht ResponseID und Time (vor/nach) löschen muss.
    
Differenz_before_after_durchschnitt_RW <- risk_before_durchschnitt_Risikowahrnehmung - risk_after_durchschnitt_Risikowahrnehmung


# Wir benötigen keine negativen Zahlen, sondern nur die Änderung --> Daher den Betrag berechnen.

abs(Differenz_before_after_durchschnitt_RW)


# Jetzt kann die Korrelation berechnet werden.

jmv::corrMatrix(
  data = data_filtered,
  vars = vars(Tech_Interaction, Differenz_before_after_durchschnitt_RW))
