load("C:\\Users\\kiki2\\OneDrive\\Documents\\TU Dortmund\\Visualisierung komplexer Daten\\kuehe.RData")


unique(kuehe$abggrund)

# Datensatz mit allen abgegangenen Kühen

abgang <- kuehe[!is.na(kuehe$abggrund), ]

euter <- c(sum(abgang$mastitis[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$metritis[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$hypocal[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$ketose[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$nachgebver[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$zyklus[abgang$abggrund == "Euter / Leistung"]),
           sum(abgang$labmagen[abgang$abggrund == "Euter / Leistung"]))

glieder <- c(sum(abgang$mastitis[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$metritis[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$hypocal[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$ketose[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$nachgebver[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$zyklus[abgang$abggrund == "Klauen / Gliedmaßen"]),
             sum(abgang$labmagen[abgang$abggrund == "Klauen / Gliedmaßen"]))

fruchtbar <- c(sum(abgang$mastitis[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$metritis[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$hypocal[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$ketose[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$nachgebver[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$zyklus[abgang$abggrund == "Unfruchtbarkeit"]),
               sum(abgang$labmagen[abgang$abggrund == "Unfruchtbarkeit"]))

stoffwechsel <- c(sum(abgang$mastitis[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$metritis[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$hypocal[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$ketose[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$nachgebver[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$zyklus[abgang$abggrund == "Stoffwechsel"]),
                  sum(abgang$labmagen[abgang$abggrund == "Stoffwechsel"]))

alter <- c(sum(abgang$mastitis[abgang$abggrund == "Alter"]),
           sum(abgang$metritis[abgang$abggrund == "Alter"]),
           sum(abgang$hypocal[abgang$abggrund == "Alter"]),
           sum(abgang$ketose[abgang$abggrund == "Alter"]),
           sum(abgang$nachgebver[abgang$abggrund == "Alter"]),
           sum(abgang$zyklus[abgang$abggrund == "Alter"]),
           sum(abgang$labmagen[abgang$abggrund == "Alter"]))

verkauf <- c(sum(abgang$mastitis[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$metritis[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$hypocal[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$ketose[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$nachgebver[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$zyklus[abgang$abggrund == "Verkauf Zucht"]),
             sum(abgang$labmagen[abgang$abggrund == "Verkauf Zucht"]))

sonstiges <- c(sum(abgang$mastitis[abgang$abggrund == "Sonstiges"]),
               sum(abgang$metritis[abgang$abggrund == "Sonstiges"]),
               sum(abgang$hypocal[abgang$abggrund == "Sonstiges"]),
               sum(abgang$ketose[abgang$abggrund == "Sonstiges"]),
               sum(abgang$nachgebver[abgang$abggrund == "Sonstiges"]),
               sum(abgang$zyklus[abgang$abggrund == "Sonstiges"]),
               sum(abgang$labmagen[abgang$abggrund == "Sonstiges"])) 

abgang_frame <- data.frame(euter/sum(euter), glieder/sum(glieder),
                           stoffwechsel / sum(stoffwechsel), fruchtbar / sum(fruchtbar),
                           alter / sum(alter), verkauf / sum(verkauf),
                           sonstiges / sum(sonstiges))

t(abgang_frame) * 10
# Sterndiagramm

namen <- c("mastitis", "metritis", "hypocal", "ketose", "nachgebver",
           "zyklus", "Labmagen")
label <- c("Euter / Leistung", "Klauen / Gliedmaßen", "Stoffwechsel",
           "Unfruchtbarkeit", "Alter", "Verkauf Zucht", "Sonstiges")

grau <- c("grey0","grey17", "grey33", "grey40", "grey66", "grey83","grey100")

stars(t(abgang_frame) * 3 , scale = F , xlim = c(1, 10) , ylim = c(0, 15) ,
      lty = 1, lwd = 2, cex = 1, col.stars = rep("lightgrey", ncol(abgang_frame)),
      locations = cbind(c(-5, 5, 15, -10, 0, 10, 20) , c(14, 14, 14, 8, 8, 8, 8)),
      key.loc = c(5, 2), len = 2, key.labels = krankheiten, axes = F,
      draw.segments = TRUE, labels = label, col.segments = grau, 
      main = "Anteil der Krankheiten pro Abgangsgrund ")

stars( swiss[1:6, ]/100, scale = FALSE ,xlim = c(1, 5) ,
       ylim = c(0, 6.2) ,lty = 1, lwd = 2, cex = 1.4,
       col.stars = rep(" darkgrey ", ncol ( swiss ) ) ,
       locations = cbind(c(1, 3, 5, 1, 3, 5) ,c(6, 6, 6, 4, 4, 4) ) ,
       key.labels = namen , key.loc = c(3, 1) , draw.segments = F)




temp1 <- aggregate(kuehe$mkg[!is.na(kuehe$mkg)],
                   list(kuehe$farm[!is.na(kuehe$mkg)]), FUN = mean)

temp2 <- aggregate(kuehe$lnr[!is.na(kuehe$mkg)],
                   list(kuehe$farm[!is.na(kuehe$mkg)]), FUN = mean)

merge(temp1, temp2, by = "Group.1")



pcoord <- function (x , scale = " stand ",
                    var.labels = colnames( x ) , col = " black ",
                    lty = " solid ", ...) {
  if( scale == " stand ") x <- scale( x )
  if( scale == " norm ") x <- t((t( x ) - sapply(x ,
                                                  min) ) / sapply(x , function ( i )
                                                    diff ( range( i ) ) ) )
  matplot (1:ncol( x ) , t( x ) , axes = FALSE ,
           type = "l", col = col , lty = lty , ...)
  abline ( v = 1: ncol( x ) , col = " lightgrey ")
  box()
  axis(1, at = 1: ncol( x ) , labels = var.labels )
  axis(2, las = 1)
}





kuehe$mastitis_tage[kuehe$mastitis == 0] <- 0 
kuehe$metritis_tage[kuehe$metritis == 0] <- 0 
kuehe$hypocal_tage[kuehe$hypocal == 0] <- 0 
kuehe$ketose_tage[kuehe$ketose == 0] <- 0 
kuehe$nachgebver_tage[kuehe$nachgebver == 0] <- 0 
kuehe$zyklus_tage[kuehe$zyklus == 0] <- 0 
kuehe$labmagen_tage[kuehe$labmagen == 0] <- 0 

krank <- data.frame(kuehe$mastitis_tage, kuehe$metritis_tage,
                    kuehe$hypocal_tage, kuehe$ketose_tage,
                    kuehe$nachgebver_tage, kuehe$zyklus_tage, kuehe$labmagen_tage)

windows( width = 9, height = 5.25)
opar <- par( mar = c( 9.7, 3.2, 0.2, 0.2) , lwd = 2,
              cex = 1.4, las = 2)
pcoord( krank , xlab = "", ylab = "",
        var.labels = namen , lty = 2)
title( ylab = "Tage in der vorliegenden Laktation bis Auftreten der Krankheit",
       line = 2, cex.lab = 0.9)
par( opar )

krankheiten <- c("Mastitis", " Metritis", "Hypocalcämie", "Ketose",
                 "Nachgeburtsverhaltung", "Zyklusstörung", " Labmagenverlagerung")

matplot(t(krank), type = "l", lty = 1, col = "grey", 
        ylab = "Tage", cex.lab = 0.9,
        axes = FALSE)
axis(2)
axis(side=1,at=1:ncol(krank), labels = krankheiten)
title(main = "Tage bis zum Eintreten der Krankheiten in der vorliegenden Laktation",
      cex.main = 0.9)
