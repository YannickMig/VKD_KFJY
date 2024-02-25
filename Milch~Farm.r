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
        col = c("black", "black", "black", "brown", "brown", "black", "brown", "black", "black", "black"))
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("black", "brown"),
       title = "Kuhrassen")


# Holstein je Farm
milch_holstein_je_farm <- table(einmalige_milch_holstein$farm)
barplot(milch_holstein_je_farm, 
        main = "Anzahl Milchekühe (Holstein) je Farm", 
        xlab = "Farm Nummer", 
        ylab = "Anzahl Kühe",
        ylim = c(0, 200),
        col = "black")


# Fleckvieh je Farm
milch_fleckvieh_je_farm <- table(einmalige_milch_fleckvieh$farm)
barplot(milch_fleckvieh_je_farm, 
        main = "Anzahl Milchekühe (Fleckvieh) je Farm", 
        xlab = "Farm Nummer", 
        ylab = "Anzahl Kühe",
        ylim = c(0, 200),
        col = "brown")


# Milch pro Kuh je Farm
anzahl_kuehe_messungen <- aggregate(lom ~ farm, data = milch_kuehe, FUN = length)
summe_mkg_milch_kuehe <- aggregate(mkg ~ farm, data = milch_kuehe, FUN = sum)

summe_mkg_milch_kuehe$anzahl <- anzahl_kuehe_messungen$lom
summe_mkg_milch_kuehe$MKG_pro_Kuh <- summe_mkg_milch_kuehe$mkg / anzahl_kuehe_messungen$lom

milch_proh_kuh_je_farm_plot <- barplot(summe_mkg_milch_kuehe$MKG_pro_Kuh, names.arg = summe_mkg_milch_kuehe$farm,
        ylim = c(0,40),
        col = c("black", "black", "black", "brown", "brown", "black", "brown", "black", "black", "black"))
title(xlab = "Farm Nummer", line = 2)
title(ylab = "mittle Milchmenge je Kuh in [kG]", line = 2)
text(x = milch_proh_kuh_je_farm_plot,
     y = summe_mkg_milch_kuehe$MKG_pro_Kuh + 1,
     label = paste0(round(summe_mkg_milch_kuehe$MKG_pro_Kuh, 1)))
title(main = "Milchmenge je Farm")
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("black", "brown"),
       title = "Kuhrassen")


# Milch pro Kuh je Farm zu Anzahl Kühe
anzahl_kuehe_messungen <- aggregate(lom ~ farm, data = milch_kuehe, FUN = length)
summe_mkg_milch_kuehe <- aggregate(mkg ~ farm, data = milch_kuehe, FUN = sum)

summe_mkg_milch_kuehe$anzahl <- anzahl_kuehe_messungen$lom
summe_mkg_milch_kuehe$MKG_pro_Kuh <- summe_mkg_milch_kuehe$mkg / anzahl_kuehe_messungen$lom

plot(summe_mkg_milch_kuehe$anzahl , summe_mkg_milch_kuehe$MKG_pro_Kuh , xlab = "", ylab = "", pch = 20, col = c("black", "black", "black", "brown", "brown", "black", "brown", "black", "black", "black"))
title(xlab = "Anzahl Kühe", line = 2.5)
title(ylab = "Milch je Kuh [kg]", line = 3)
title(main = "Milchmenge je Farm")
legend("topright",
       legend = c("Holstein-Schwarzbunt", "Fleckvieh"),
       fill = c("black", "brown"),
       title = "Kuhrassen")
