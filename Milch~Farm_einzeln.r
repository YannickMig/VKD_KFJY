holstein <- subset(kuehe, rasse == "Holstein-Schwarzbunt")
fleckvieh <- subset(kuehe, rasse == "Fleckvieh")

# Milchkühe (Kühe ohne mkg=NA oder =0)
milch_kuehe <- subset(kuehe, !is.na(mkg) & mkg != 0)
milch_holstein <- subset(holstein, !is.na(mkg) & mkg != 0)
milch_fleckvieh <- subset(fleckvieh, !is.na(mkg) & mkg != 0)


# Kühe die doppelt vorkommen rausfiltern um Anzahl je Farm zu zählen
einmalige_lom_kuehe <- unique(milch_kuehe$lom)
einmalige_zeilen_kuehe <- !duplicated(milch_kuehe$lom)
einmalige_milch_kuehe <- subset(milch_kuehe, einmalige_zeilen_kuehe)


# Holstein die doppelt vorkommen rausfiltern um Anzahl je Farm zu zählen
einmalige_lom_holstein <- unique(milch_holstein$lom)
einmalige_zeilen_holstein <- !duplicated(milch_holstein$lom)
einmalige_milch_holstein <- subset(milch_holstein, einmalige_zeilen_holstein)


# Fleckvieh die doppelt vorkommen rausfiltern um Anzahl je Farm zu zählen
einmalige_lom_fleckvieh <- unique(milch_fleckvieh$lom)
einmalige_zeilen_fleckvieh <- !duplicated(milch_fleckvieh$lom)
einmalige_milch_fleckvieh <- subset(milch_fleckvieh, einmalige_zeilen_fleckvieh)



# Kühe je Farm
milch_kuehe_je_farm <- table(einmalige_milch_kuehe$farm)
barplot(milch_kuehe_je_farm, 
        main = "Anzahl Milchekühe je Farm", 
        xlab = "Farm Nummer", 
        ylab = "Anzahl Kühe",
        ylim = c(0, 200),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2", "darkgrey", "brown2", "darkgrey", "darkgrey", "darkgrey"))
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("darkgrey", "brown2"),
       title = "Kuhrassen")


# Holstein je Farm
milch_holstein_je_farm <- table(einmalige_milch_holstein$farm)
barplot(milch_holstein_je_farm, 
        main = "Anzahl Milchekühe (Holstein) je Farm", 
        xlab = "Farm Nummer", 
        ylab = "Anzahl Kühe",
        ylim = c(0, 200),
        col = "darkgrey")


# Fleckvieh je Farm
milch_fleckvieh_je_farm <- table(einmalige_milch_fleckvieh$farm)
barplot(milch_fleckvieh_je_farm, 
        main = "Anzahl Milchekühe (Fleckvieh) je Farm", 
        xlab = "Farm Nummer", 
        ylab = "Anzahl Kühe",
        ylim = c(0, 200),
        col = "brown2")


# Milch pro Kuh je Farm
anzahl_kuehe_messungen <- aggregate(lom ~ farm, data = milch_kuehe, FUN = length)
summe_mkg_milch_kuehe <- aggregate(mkg ~ farm, data = milch_kuehe, FUN = sum)

summe_mkg_milch_kuehe$anzahl <- anzahl_kuehe_messungen$lom
summe_mkg_milch_kuehe$MKG_pro_Kuh <- summe_mkg_milch_kuehe$mkg / anzahl_kuehe_messungen$lom

milch_proh_kuh_je_farm_plot <- barplot(summe_mkg_milch_kuehe$MKG_pro_Kuh, names.arg = summe_mkg_milch_kuehe$farm,
        ylim = c(0,45),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2", "darkgrey", "brown2", "darkgrey", "darkgrey", "darkgrey"))
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
