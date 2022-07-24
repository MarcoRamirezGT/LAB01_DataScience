

install.packages("FactoMineR")
install.packages("corrplot")
install.packages("arules")


library(arules)
library(psych)
library(FactoMineR)
library(fpc)
library(factoextra)
library(corrplot)



db<-read.csv('train.csv')

#Quitar nulos
db[is.na(db)] <- 0
#Calculo de percentiles
percentil <- quantile(db$SalePrice)
#Percentiles
estado<-c('Estado')
db$Estado<-estado
#Economica=0
#Intermedia=1
#Cara=2
db <- within(db, Estado[SalePrice<=129975] <- 'Economica')
db$Estado[(db$SalePrice>129975 & db$SalePrice<=163000)] <- 'Intermedia'
db$Estado[db$SalePrice>163000] <- 'Cara'


#Cambio de tipo de columnas a numerico, ya que necesitamos las variables numericas 

db$SalePrice<-as.numeric(db$SalePrice)
db$GrLivArea<-as.numeric(db$GrLivArea)

db$LotFrontage<-as.numeric(db$LotFrontage)
db$LotArea<-as.numeric(db$LotArea)
db$YearBuilt<-as.numeric(db$YearBuilt)
db$YearRemodAdd<-as.numeric(db$YearRemodAdd)
db$MasVnrArea<-as.numeric(db$MasVnrArea)
db$BsmtFinSF1<-as.numeric(db$BsmtFinSF1)
db$BsmtFinSF2<-as.numeric(db$BsmtFinSF2)
db$BsmtUnfSF<-as.numeric(db$BsmtUnfSF)
db$TotalBsmtSF<-as.numeric(db$TotalBsmtSF)
db$X1stFlrSF<-as.numeric(db$X1stFlrSF)
db$X2ndFlrSF<-as.numeric(db$X2ndFlrSF)
db$LowQualFinSF<-as.numeric(db$LowQualFinSF)

db$GarageArea<-as.numeric(db$GarageArea)
db$WoodDeckSF<-as.numeric(db$WoodDeckSF)
db$OpenPorchSF<-as.numeric(db$OpenPorchSF)
db$ScreenPorch<-as.numeric(db$ScreenPorch)
db$EnclosedPorch<-as.numeric(db$EnclosedPorch)
db$PoolArea<-as.numeric(db$PoolArea)
db$MiscVal<-as.numeric(db$MiscVal)



db$Estado<-as.factor(db$Estado)


set.seed(123)
datos<-data.frame(db$Estado,db$SalePrice,db$GrLivArea,db$LotFrontage,db$LotArea,db$YearBuilt,db$YearRemodAdd,db$MasVnrArea,db$BsmtFinSF1,db$BsmtFinSF2,db$BsmtUnfSF,db$TotalBsmtSF,db$X1stFlrSF,db$X2ndFlrSF,db$LowQualFinSF,db$GarageArea,db$WoodDeckSF,db$OpenPorchSF,db$ScreenPorch,db$EnclosedPorch,db$PoolArea,db$MiscVal)

rcor<-cor(datos[,2:22],use = "pairwise.complete.obs")
det(rcor)#Si el determinante de la matriz de correlación es cercano a 0 significa que hay multicolinealidad

cortest.bartlett(datos[,-1])

#se muestra la matriz de correlación
cor(datos[,-1],use = "pairwise.complete.obs")



#Esta función normaliza los datos de una vez
compPrinc<-prcomp(datos[,2:22], scale = T)



summary(compPrinc)

# Importance of components:
#                         PC1   PC2   PC3   PC4    PC5    PC6     PC7
# Standard deviation     1.65 1.230 1.181 0.944 0.5889 0.3167 0.15973
# Proportion of Variance 0.39 0.216 0.199 0.127 0.0495 0.0143 0.00364
# Cumulative Proportion  0.39 0.606 0.805 0.932 0.9820 0.9964 1.00000
compPrincPCA<-PCA(datos[,-1],ncp=ncol(datos[,-1]), scale.unit = T)

summary(compPrincPCA)


#Se obtiene el scree plot de las componentes principales.

fviz_eig(compPrinc, addlabels = TRUE, choice = c("eigenvalue"), ylim = c(0, 3))

fviz_pca_biplot(compPrinc,repel = F)

# En la siguiente gráfica se ilustra la calidad de la representación de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Contribución de las variables a las 3 primeras dimensiones
fviz_contrib(compPrinc, choice = "var", axes = 1, top = 10) #Dimensión 1
fviz_contrib(compPrinc, choice = "var", axes = 2, top = 10) #Dimensión 2
fviz_contrib(compPrinc, choice = "var", axes = 3, top = 10) #Dimensión 3
fviz_contrib(compPrinc, choice = "var", axes = 4, top = 10) #Dimensión 4
fviz_contrib(compPrinc, choice = "var", axes = 5, top = 10) #Dimensión 5
fviz_contrib(compPrinc, choice = "var", axes = 6, top = 10) #Dimensión 6
fviz_contrib(compPrinc, choice = "var", axes = 7, top = 10) #Dimensión 7

var<-get_pca_var(compPrinc)
corrplot(var$cos2, is.corr = F)




# REGLAS DE ASOCIACI?N




datos$db.LotFrontage[datos$db.LotFrontage==0] <- 63
datos$db.MasVnrArea[datos$db.MasVnrArea==0] <- 103.1
datos$db.BsmtFinSF1    [datos$db.BsmtFinSF1   ==0] <- 383.5
datos$db.BsmtFinSF2[datos$db.BsmtFinSF2==0] <- 46.55
datos$db.BsmtUnfSF[datos$db.BsmtUnfSF==0] <- 567.2



datos$db.SalePrice<-as.numeric(datos$db.SalePrice)
datos$db.GrLivArea<-as.factor(datos$db.GrLivArea)
datos$db.LotFrontage<-as.factor(datos$db.LotFrontage)
datos$db.LotArea<-as.factor(datos$db.LotArea)
datos$db.YearBuilt<-as.factor(datos$db.YearBuilt)
datos$db.YearRemodAdd<-as.factor(datos$db.YearRemodAdd)
datos$db.MasVnrArea<-as.factor(datos$db.MasVnrArea)
datos$db.BsmtFinSF1<-as.factor(datos$db.BsmtFinSF1)
datos$db.BsmtFinSF2<-as.factor(datos$db.BsmtFinSF2)
datos$db.BsmtUnfSF<-as.factor(datos$db.BsmtUnfSF)

datos$db.TotalBsmtSF<-as.factor(datos$db.TotalBsmtSF)
datos$db.X1stFlrSF<-as.factor(datos$db.X1stFlrSF)
datos$db.X2ndFlrSF<-as.factor(datos$db.X2ndFlrSF)
datos$db.LowQualFinSF<-as.factor(datos$db.LowQualFinSF)

datos$db.GarageArea<-as.factor(datos$db.GarageArea)
datos$db.WoodDeckSF<-as.factor(datos$db.WoodDeckSF)
datos$db.OpenPorchSF<-as.factor(datos$db.OpenPorchSF)
datos$db.ScreenPorch<-as.factor(datos$db.ScreenPorch)
datos$db.EnclosedPorch<-as.factor(datos$db.EnclosedPorch)
datos$db.PoolArea<-as.factor(datos$db.PoolArea)
datos$db.MiscVal<-as.factor(datos$db.MiscVal)

# 


# row_sub = apply(datos, 1, function(row) all(row !=0 ))
# 
# View(datos)
# ##Subset as usual
# datos[row_sub,]


str(datos)
reglas<-apriori(datos[ ,2:16 ], parameter = list(support = 0.2,
                                                       confidence = 0.70,
                                                       target = "rules"))
inspect(reglas)


