#Hypothese: Je höher die TB höher, desto niedriger die Änderung der allgemeinen Risikowahrnehmung zwischen vor- und nach-Framing. 

# Mit overall_diff (der Änderung von Risiko-vor-Framing und Risiko-nach-Framing) kann die Korrelation berechnet werden.

jmv::corrMatrix(
  data = data_filtered,
  vars = vars(Tech_Interaction, overall_diff),
  spearman = TRUE,
  kendall = TRUE)

#--> p-Value nicht signifikant (wenn man mit Betrag rechnet, ist er signifikant)
#--> wenn Voraussetzungen für parametrische Verfahren nicht erfüllt sind, bieten sich Spearman Korrelation oder Kendall's Tau an

# --> Alles nicht signifikant.


# Zur Interpretation: Ein negativer Korrelationskoeffizient würde darauf hindeuten, dass höhere Technikbereitschaft mit geringerer Beeinflussbarkeit durch das Framing korreliert.

