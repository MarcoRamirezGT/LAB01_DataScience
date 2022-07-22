### Name: paf
### Title: Principal Axis Factoring
### Aliases: paf
### Keywords: manip misc

### ** Examples


library(rela)

Belts <- Seatbelts[,1:7]
summary(Belts)

paf.belt <- paf(Belts)
paf.belt
summary(paf.belt)
plot(paf.belt)

Belts2 <- Belts[,-5]
Belts2 <- Belts2[,-5] 

paf.belt2 <- paf(Belts2)
paf.belt2
summary(paf.belt2)
plot(paf.belt2)



