setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("kuehe.RData")
gesk <- numeric(1228)
for(i in 1:1228){
  gesk[i] <- kuehe[i,21]+kuehe[i,23]+kuehe[i,25]+kuehe[i,27]+kuehe[i,29]+kuehe[i,31]+kuehe[i,33]
}
kuehe$gesk <- gesk


krank <- numeric(1228)
krank[which(gesk > 0)] <- T
kuehe$krank <- krank


### Grafik 1 ###

par(mar = c(5, 4, 4, 6), cex.lab = 1.2, cex.main=1.6)
cd_plot <- cdplot(as.factor(kuehe$krank) ~ (kuehe$glukose), 
                  xlab = expression(paste("Gemittelter Glukosewert in \  ", frac("mmol", "L"))),
                  ylab="Gesundheitsstatus", main="Gesundheitsstatus in Abhängigkeit der Glukosewerte", 
                  yaxlabels = c("gesund","krank"))
mtext("Relative Häufigkeit", side = 4, line=2.5, cex = 1.2)
par(cex.axis = 1, cex.lab = 1, cex.main = 1)


### Grafik 2 ###

krank[which(krank == 1)] <- "krank"
krank[which(krank == 0)] <- "gesund"
kuehe$krank <- krank

par(cex.axis = 1.3, cex.lab = 1.5, cex.main=1.7)
mosaicplot(kuehe$farm ~ kuehe$krank,
           xlab="Farm", col=c("limegreen","darkgrey"),
           main="Gesundheitsstatus in Abhängigkeit der Farm", cex.axis=1.2,
           ylab="Gesundheitsstatus") 

par(cex.axis = 1, cex.lab = 1, cex.main = 1)

### Grafik 3 ###

library("DescTools")

#farben <- c("darkgrey","turquoise","chocolate4", "burlywood","pink","burlywood")

hs <- kuehe[which(kuehe$rasse == "Fleckvieh"),]
fv <- kuehe[which(kuehe$rasse == "Holstein-Schwarzbunt"),]

index <- c(9,12,14,15,16,17,18,22,24,26,28,30,32,34)
v <- numeric(34)
for(i in index){
  v[i] <- mean(hs[,i], na.rm = T)
}

Holstein_Schwarzbunt <- v[which(v != 0)]

index <- c(9,12,14,15,16,17,18,22,24,26,28,30,32,34)
f <- numeric(34)
for(i in index){
  f[i] <- mean(fv[,i], na.rm = T)
}
Fleckvieh <- f[which(f != 0)]

plotrasse <- rbind(Fleckvieh,Holstein_Schwarzbunt)
PlotFaces(plotrasse[1:2,],nr = 1 , nc = 2, scale = T)
citation("DescTools")

table(krank)
1-476/(476+752)
?PlotFaces
min(kuehe$glukose, na.rm = T)
