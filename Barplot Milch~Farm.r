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
