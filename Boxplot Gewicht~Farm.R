gew_kuehe <- subset(kuehe, !(farm %in% c(3, 9)))
gew_kuehe$farm <- factor(gew_kuehe$farm, levels = unique(gew_kuehe$farm))

opar <- par(mar = c(4, 4, 3, 1), lwd = 2, las = 1)
boxplot(gew ~ farm , data = gew_kuehe , xlab = "",
        ylab = "", horizontal = TRUE ,
        pars = list(outcex = 1, medlwd = 2),
        col = c("darkgrey", "darkgrey", "darkgrey", "brown2", "brown2", "darkgrey", "brown2", "darkgrey", "darkgrey", "darkgrey"))
title(xlab = "Gewicht [kg]", line = 2.5)
title(ylab = "Farm", line = 2.5)
title(main = "Gewicht der Kühe je Farm")
legend("topright", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.7,
       title = "Kuhrassen")

