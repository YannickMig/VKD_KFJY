# Zusammengefasste Grafik für das Gewicht und die Milchmenge der Kühe
holstein <- subset(kuehe, rasse == "Holstein-Schwarzbunt")
fleckvieh <- subset(kuehe, rasse == "Fleckvieh")

gew_holstein <- kuehe$gew[kuehe$rasse == "Holstein-Schwarzbunt" & !is.na(kuehe$gew)]
gew_fleckvieh <- kuehe$gew[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$gew)]

dens_gew_holstein <- density(gew_holstein)
dens_gew_fleckvieh <- density(gew_fleckvieh)

mkg_holstein <- kuehe$mkg[kuehe$rasse == "Holstein-Schwarzbunt" & !is.na(kuehe$mkg)]
mkg_fleckvieh <- kuehe$mkg[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$mkg)]

dens_mkg_holstein <- density(mkg_holstein)
dens_mkg_fleckvieh <- density(mkg_fleckvieh)


mat <- matrix(c(1,4,2,3), ncol = 2, byrow = TRUE)
layout(mat, widths = c(9, 1.5), heights = c(2, 5))
layout.show(4)

# Plot Kerndichteschätzer Gewicht nach Rasse
par(mar = c(0, 5, 2, 0))
plot(dens_gew_holstein, main = "", xlab = "", ylab = "Dichte", col = "darkgrey", lwd = 2, ylim = c(0, 0.006), xaxt = "n")
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
plot(dens_mkg_holstein$y, dens_mkg_holstein$x, main = "", xlab = "Dichte", ylab = "", type = "l", col = "darkgrey", lwd = 2, xlim = c(0, 0.07), yaxt = "n")
lines(dens_mkg_fleckvieh$y, dens_mkg_fleckvieh$x, col = "brown2", lwd = 2)

# Legende
par(mar = c(0,0,1,0.6))
plot.new()
legend("right", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("darkgrey", "brown2"), pch = 15, cex = 0.79, box.lwd = NA, title = "Kuhrassen")
layout(1)