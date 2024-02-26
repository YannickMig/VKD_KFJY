# Milchkühe (Kühe ohne mkg=NA oder =0)
milch_kuehe <- subset(kuehe, !is.na(mkg) & mkg != 0)

# Kühe die doppelt vorkommen rausfiltern um Anzahl je Farm zu zählen
einmalige_lom_kuehe <- unique(milch_kuehe$lom)
einmalige_zeilen_kuehe <- !duplicated(milch_kuehe$lom)
einmalige_milch_kuehe <- subset(milch_kuehe, einmalige_zeilen_kuehe)


# Milch pro Kuh je Farm
anzahl_kuehe_messungen <- aggregate(lom ~ farm, data = milch_kuehe, FUN = length)
summe_mkg_milch_kuehe <- aggregate(mkg ~ farm, data = milch_kuehe, FUN = sum)

summe_mkg_milch_kuehe$anzahl <- anzahl_kuehe_messungen$lom
summe_mkg_milch_kuehe$MKG_pro_Kuh <- summe_mkg_milch_kuehe$mkg / anzahl_kuehe_messungen$lom

opar <- par(mar = c(4, 4, 3, 1), lwd = 2, las = 1)
boxplot(mkg ~ farm , data = milch_kuehe , xlab = "",
        ylab = "", #horizontal = TRUE ,
        pars = list(outcex = 1, medlwd = 2),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2", "darkgrey", "brown2", "darkgrey", "darkgrey", "darkgrey"))
title(ylab = "Milchmenge in [kg]", line = 2.5)
title(xlab = "Farm", line = 2.5)
title(main = "mittlere Milchmenge der Kühe je Farm")
legend("topright", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.7,
       title = "Kuhrassen")
