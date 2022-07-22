### Name: itemanal
### Title: Item analysis function
### Aliases: itemanal
### Keywords: manip misc

### ** Examples


library(rela)

Belts <- Seatbelts[,1:7]
Belts.item <- itemanal(Belts)
Belts.item
summary(Belts.item)
plot(Belts.item)

Belts2 <- Belts[,-5]
Belts2 <- Belts2[,-5] 
Belts.item2 <- itemanal(Belts2)
Belts.item2
summary(Belts.item2)
plot(Belts.item2)



