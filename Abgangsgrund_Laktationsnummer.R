### Abgangsgründe der beiden Kuhrassen ### 

windows()
prozente <- proportions(table(kuehe[ , c("rasse", "abggrund")]), margin = 1) * 100
b <- barplot(prozente, beside = TRUE, col = c("brown3", "darkgrey"), 
             xlab = "Abgangsgrund", ylab = "Anteil [%]", main = "Abgangsgründe der
             beiden Kuhrassen")
legend(14.5, 33.35201, legend = c("Fleckvieh", "Holzstein-Schwarzbunt"),
       col= c("brown3", "darkgrey"), pch=16)

### Milchmenge in Abhängigkeit der Laktationsnummer ###

#install.packages("lattice")
library("lattice")
trellis.device(color = FALSE)
bwplot (lnr ~ mkg , data = kuehe , pch = "|",
         xlab = "Mittlere Milchmenge in kg aus vorheriger Laktation",
         ylab = "Laktationsnummer",
         scales = list ( y = list ( labels = 1:10) ), main = "Milchmenge in
        Abhängigkeit der Laktationsnummer")
citation("lattice")
