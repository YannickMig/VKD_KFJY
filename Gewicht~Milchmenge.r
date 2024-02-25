
# Zusammengefasste Grafik für das Gewicht und die Milchmenge der Kühe

gew_holstein <- kuehe$gew[kuehe$rasse == "Holstein-Schwarzbunt" & !is.na(kuehe$gew)]
gew_fleckvieh <- kuehe$gew[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$gew)]

dens_gew_holstein <- density(gew_holstein)
dens_gew_fleckvieh <- density(gew_fleckvieh)

mkg_holstein <- kuehe$mkg[kuehe$rasse == "Holstein-Schwarzbunt" & !is.na(kuehe$mkg)]
mkg_fleckvieh <- kuehe$mkg[kuehe$rasse == "Fleckvieh" & !is.na(kuehe$mkg)]

dens_mkg_holstein <- density(mkg_holstein)
dens_mkg_fleckvieh <- density(mkg_fleckvieh)


# Layout für die Grafiken
mat <- matrix(c(1,4,2,3), ncol = 2, byrow = TRUE)
layout(mat, widths = c(5, 2), heights = c(2, 5))
layout.show(4)

# Plot Kerndichteschätzer Gewicht nach Rasse
par(mar = c(0, 5, 1, 0)) # Adjusting margins to eliminate gaps
plot(dens_gew_holstein, main = "", xlab = "", ylab = "Dichte", col = "black", lwd = 2, ylim = c(0, 0.006), xaxt = "n")
lines(dens_gew_fleckvieh, col = "brown", lwd = 2)


# Plot Zusammenhang zwischen Milch und Gewicht
par(mar = c(5, 5, 0, 0)) # Adjusting margins to eliminate gaps
plot(holstein$gew, holstein$mkg, 
     xlab = "Gewicht der Kuh in [kg]", 
     ylab = "mittle Milchmenge der Kuh in [kG]",
     col = "black",
     pch = 20)
points(fleckvieh$gew, fleckvieh$mkg, 
       col = "brown", # Farbe für Fleckvieh
       pch = 20) # Form der Punkte

# Plot Kerndichteschätzer Milch nach Rasse
par(mar = c(5, 0, 0, 1)) # Adjusting margins to eliminate gaps
plot(dens_mkg_holstein$y, dens_mkg_holstein$x, main = "", xlab = "Dichte", ylab = "", type = "l", col = "black", lwd = 2, xlim = c(0, 0.07), yaxt = "n")
lines(dens_mkg_fleckvieh$y, dens_mkg_fleckvieh$x, col = "brown", lwd = 2)

# Legende
par(mar = c(0,0,0,1))
plot.new()
legend("center", legend = c("Holstein-Schwarzbunt", "Fleckvieh"), 
       col = c("black", "brown"), pch = 15, cex = 1.1, box.lwd = NA)

