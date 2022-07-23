

#Importar el dataset
db<-read.csv('train.csv')
#Resumen del dataset
summary(db)


#Gráficos de variables continuas
hist(x = db$LotFrontage)
hist(x = db$LotArea)
hist(x = db$MasVnrArea)
hist(x = db$BsmtFinSF1)
hist(x = db$BsmtFinSF2)
hist(x = db$BsmtUnfSF)
hist(x = db$TotalBsmtSF)
hist(x = db$X1stFlrSF)
hist(x = db$X2ndFlrSF)
hist(x = db$LowQualFinSF)
hist(x = db$GrLivArea)
hist(x = db$GarageArea)
hist(x = db$WoodDeckSF)
hist(x = db$OpenPorchSF)
hist(x = db$EnclosedPorch)
hist(x = db$ScreenPorch)
hist(x = db$PoolArea)
hist(x = db$MiscVal)


#Gráficos de variables discretas
