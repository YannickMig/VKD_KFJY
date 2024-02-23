# Grafik zum Abgangsgrund je Rasse
table_umgeordnet <- table(kuehe$rasse, kuehe$abggrund)[
  , c("Euter / Leistung", "Unfruchtbarkeit", "Klauen / Gliedmaßen",
      "Alter", "Verkauf Zucht", "Stoffwechsel", "Sonstiges")]
table_rel <- prop.table(table_umgeordnet, margin = 1) * 100

barplot_data <- barplot(table_rel,
                        col = c("black", "brown"),
                        beside = TRUE,
                        xlab = "Abgangsgrund",
                        ylab = "Relative Häufigkeit (%)",
                        ylim = c(0, 40))
text(x = barplot_data,
     y = table_rel + 1,
     label = paste0(round(table_rel, 1), "%"))
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("black", "brown"),
       title = "Kuhrassen")