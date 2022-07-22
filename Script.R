

install.packages("FactoMineR")
install.packages("corrplot")
install.packages("arules")


library(arules)

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


#Cambio de tipo de columnas

db$SalePrice<-as.numeric(db$SalePrice)
db$GrLivArea<-as.numeric(db$GrLivArea)
db$GarageCars<-as.numeric(db$GarageCars)
db$YearBuilt<-as.numeric(db$YearBuilt)
db$GarageArea<-as.numeric(db$GarageArea)
db$X1stFlrSF<-as.numeric(db$X1stFlrSF)

db$Estado<-as.factor(db$Estado)


set.seed(123)
datos<-data.frame(db$Estado,db$SalePrice,db$GrLivArea,db$GarageCars,db$YearBuilt,db$GarageArea,db$X1stFlrSF)

rcor<-cor(datos[,2:6],use = "pairwise.complete.obs")
det(rcor)#Si el determinante de la matriz de correlación es cercano a 0 significa que hay multicolinealidad



#se muestra la matriz de correlación
cor(datos[,-1],use = "pairwise.complete.obs")



#Esta función normaliza los datos de una vez
compPrinc<-prcomp(datos[,2:6], scale = T)
compPrinc


summary(compPrinc)

# Importance of components:
#                         PC1   PC2   PC3   PC4    PC5    PC6     PC7
# Standard deviation     1.65 1.230 1.181 0.944 0.5889 0.3167 0.15973
# Proportion of Variance 0.39 0.216 0.199 0.127 0.0495 0.0143 0.00364
# Cumulative Proportion  0.39 0.606 0.805 0.932 0.9820 0.9964 1.00000
compPrincPCA<-PCA(datos[,-1],ncp=ncol(datos[,-1]), scale.unit = T)

summary(compPrincPCA)


#Se obtiene el scree plot de las componentes principales.
# Como se ve hacen falta 4 de las 7 componentes para explicar m?s del 80% de la variabilidad
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))
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

var<-get_pca_var(compPrinc)
corrplot(var$cos2, is.corr = F)


# TRABAJANDO CON IRIS
pafIris <- paf(as.matrix(iris[,1:4]))
pafIris$KMO #La adecuaci?n muestral no es buena

cortest.bartlett(iris[,1:4])

irisPCA <- PCA(iris[,1:4])
summary(irisPCA)
# Con las primeras 2 dimensiones se explica el 95% de la variancia del conjunto de datos
# En la primera dimensi?n est?n muy bien representadas las variables Petal.Length y Petal.Width
# En la segunda dimensi?n se pueden incluir las variables Sepal.Width y Sepal.Length
# Estas dos primeras componentes se puede interpretar de la siguiente forma:
# PC1: Medidas del P?talo
# PC2: Medidas del sépalo

#Scree Plot
fviz_eig(irisPCA, addlabels = TRUE, ylim = c(0, 80))
fviz_eig(irisPCA, addlabels = TRUE,choice = c("eigenvalue"), ylim = c(0, 3))


#Representaci?n de las variables en cada componente

fviz_contrib(irisPCA, choice = "var", axes = 1, top = 10)
fviz_contrib(irisPCA, choice = "var", axes = 2, top = 10)


fviz_pca_var(irisPCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Representaci?n de cada variable en cada componente
var<-get_pca_var(irisPCA)
corrplot(var$cos2, is.corr = F)
#Seg?n la representaci?n de las variables en las componentes se podr?a incluir en la dimensi?n 1 pero la interpretabilidad del componente principal ser?a m?s complicada.



# REGLAS DE ASOCIACI?N


# El m?nimo nivel de soporte y confianza aceptados
str(datos)
datos$db.SalePrice <- as.factor(datos$db.SalePrice)
datos$db.GrLivArea <- as.factor(datos$db.GrLivArea)
datos$db.GarageCars <- as.factor(datos$db.GarageCars)
datos$db.GarageArea <- as.factor(datos$db.GarageArea)


reglas<-apriori(datos[, c(2,3,5)], parameter = list(support = 0.2,
                                                       confidence = 0.70,
                                                       target = "rules"))
inspect(reglas)


