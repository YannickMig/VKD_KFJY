# Datensatz laden
load("kuehe.RData")


# Trennung des Datensatzes nach Rasse
holstein <- subset(kuehe, rasse == "Holstein-Schwarzbunt")
fleckvieh <- subset(kuehe, rasse == "Fleckvieh")


# Daten zu doppelt vorkommenden Kühen rausfiltern
# um tatsächliche Anzahl zu bestimmen
einmalige_lom_kuehe <- unique(kuehe$lom)
einmalige_zeilen_kuehe <- !duplicated(kuehe$lom)
einmalige_kuehe <- subset(kuehe, einmalige_zeilen_kuehe)

# Barplot zu Kuhrassen je Farm
barplot_data = barplot(table(einmalige_kuehe$rasse, einmalige_kuehe$farm),
                       col = c("darkgrey", "brown2"), beside = TRUE,
                       xlab = "Farm", ylab = "absolute Häufigkeit",
                       ylim = c(0,300))
text(x = barplot_data, y = table(einmalige_kuehe$rasse, einmalige_kuehe$farm) + 10,
     label = table(einmalige_kuehe$rasse, einmalige_kuehe$farm))
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("darkgrey", "brown2"), title = "Kuhrassen")


# Barplot zum Abgangsgrund je Rasse
table_umgeordnet <- table(kuehe$rasse, kuehe$abggrund)[
  , c("Euter / Leistung", "Unfruchtbarkeit", "Klauen / Gliedmaßen",
      "Alter", "Verkauf Zucht", "Stoffwechsel", "Sonstiges")]
table_rel <- prop.table(table_umgeordnet, margin = 1) * 100

barplot_data <- barplot(table_rel,
                        col = c("darkgrey", "brown2"),
                        beside = TRUE,
                        xlab = "Abgangsgrund",
                        ylab = "Relative Häufigkeit (%)",
                        ylim = c(0, 40))
text(x = barplot_data, y = table_rel + 1, 
     label = paste0(round(table_rel, 1), "%"))
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("darkgrey", "brown2"),
       title = "Kuhrassen")


# Boxplot zum Gewicht der Kühe je Farm
gew_kuehe <- subset(kuehe, !(farm %in% c(3, 9)))
gew_kuehe$farm <- factor(gew_kuehe$farm, levels = unique(gew_kuehe$farm))

opar <- par(mar = c(4, 4, 3, 1), lwd = 2, las = 1)
boxplot(gew ~ farm , data = gew_kuehe , xlab = "",
        ylab = "", horizontal = TRUE ,
        pars = list(outcex = 1, medlwd = 2),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2",
                "darkgrey", "brown2", "darkgrey", "darkgrey", "darkgrey"))
title(xlab = "Gewicht [kg]", line = 2.5)
title(ylab = "Farm", line = 2.5)
title(main = "Gewicht der Kühe je Farm")
legend("topright", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.7,
       title = "Kuhrassen")


# Milchkühe (Kühe ohne mkg=NA oder =0)
milch_kuehe <- subset(kuehe, !is.na(mkg) & mkg != 0)

# Milchkühe die doppelt vorkommen rausfiltern um Anzahl je Farm zu zählen
einmalige_lom_kuehe <- unique(milch_kuehe$lom)
einmalige_zeilen_kuehe <- !duplicated(milch_kuehe$lom)
einmalige_milch_kuehe <- subset(milch_kuehe, einmalige_zeilen_kuehe)

# Milch pro Kuh je Farm
anzahl_kuehe_messungen <- aggregate(lom ~ farm, data = milch_kuehe, FUN = length)
summe_mkg_milch_kuehe <- aggregate(mkg ~ farm, data = milch_kuehe, FUN = sum)
summe_mkg_milch_kuehe$anzahl <- anzahl_kuehe_messungen$lom
summe_mkg_milch_kuehe$MKG_pro_Kuh <- summe_mkg_milch_kuehe$mkg / anzahl_kuehe_messungen$lom

# Barplot zur mittleren Milchmenge pro Kuh je Farm
milch_proh_kuh_je_farm_plot <- barplot(summe_mkg_milch_kuehe$MKG_pro_Kuh,
                                       names.arg = summe_mkg_milch_kuehe$farm,
                                       ylim = c(0,45),
                                       col = c("darkgrey", "darkgrey",
                                               "darkgrey", "brown2", "brown2",
                                               "darkgrey", "brown2", "darkgrey",
                                               "darkgrey", "darkgrey"))
title(xlab = "Farm Nummer", line = 2)
title(ylab = "mittle Milchmenge je Kuh in [kG]", line = 2)
text(x = milch_proh_kuh_je_farm_plot,
     y = summe_mkg_milch_kuehe$MKG_pro_Kuh + 2,
     label = paste0(round(summe_mkg_milch_kuehe$MKG_pro_Kuh, 1)))
title(main = "durchschnittliche Milchmenge je Farm")
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("darkgrey", "brown2"),
       title = "Kuhrassen")

# Boxplot mittlere Milchmenge pro Kuh je Farm
opar <- par(mar = c(4, 4, 3, 1), lwd = 2, las = 1)
boxplot(mkg ~ farm , data = milch_kuehe , xlab = "",
        ylab = "", #horizontal = TRUE ,
        pars = list(outcex = 1, medlwd = 2),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2",
                "darkgrey", "brown2", "darkgrey", "darkgrey"))
title(ylab = "Milchmenge in [kg]", line = 2.5)
title(xlab = "Farm", line = 2.5)
title(main = "mittlere Milchmenge der Kühe je Farm")
legend("topright", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.7,
       title = "Kuhrassen")


# Gewichte der Kühe je Rasse
gew_holstein <- kuehe$gew[kuehe$rasse == "Holstein-Schwarzbunt" &!is.na(kuehe$gew)]
gew_fleckvieh <- kuehe$gew[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$gew)]

# Dichte der Gewichte der Kühe je Rasse
dens_gew_holstein <- density(gew_holstein)
dens_gew_fleckvieh <- density(gew_fleckvieh)

# mittlere Milchmenge der Kühe je Rasse
mkg_holstein <- kuehe$mkg[kuehe$rasse == "Holstein-Schwarzbunt" & !is.na(kuehe$mkg)]
mkg_fleckvieh <- kuehe$mkg[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$mkg)]

# DIchte der mittleren Milchmenge der Kühe je Rasse
dens_mkg_holstein <- density(mkg_holstein)
dens_mkg_fleckvieh <- density(mkg_fleckvieh)

# Layout festlegen
mat <- matrix(c(1,4,2,3), ncol = 2, byrow = TRUE)
layout(mat, widths = c(9, 1.5), heights = c(2, 5))
layout.show(4)

# Plot Kerndichteschätzer Gewicht nach Rasse
par(mar = c(0, 5, 2, 0))
plot(dens_gew_holstein, main = "", xlab = "", ylab = "Dichte",
     col = "darkgrey", lwd = 2, ylim = c(0, 0.006), xaxt = "n")
lines(dens_gew_fleckvieh, col = "brown2", lwd = 2)
title(main = "Gewicht und Durchschnittliche Milchmenge", line = 0.4, adj = 0.55)


# Plot Zusammenhang zwischen Milch und Gewicht
par(mar = c(5, 5, 0, 0))
plot(holstein$gew, holstein$mkg, 
     xlab = "Gewicht der Kuh in [kg]", 
     ylab = "mittle Milchmenge der Kuh in [kG]",
     col = "darkgrey",
     pch = 20)
points(fleckvieh$gew, fleckvieh$mkg, 
       col = "brown2",
       pch = 20)

# Plot Kerndichteschätzer Milch nach Rasse
par(mar = c(5, 0, 0, 1))
plot(dens_mkg_holstein$y, dens_mkg_holstein$x, main = "", xlab = "Dichte",
     ylab = "", type = "l", col = "darkgrey", lwd = 2, xlim = c(0, 0.07),
     yaxt = "n")
lines(dens_mkg_fleckvieh$y, dens_mkg_fleckvieh$x, col = "brown2", lwd = 2)

# Legende
par(mar = c(0,0,1,0.6))
plot.new()
legend("right", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.79, box.lwd = NA,
       title = "Kuhrassen")
layout(1)



