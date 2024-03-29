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
plot(db$MSSubClass)
plot(db$OverallQual)
plot(db$OverallCond)
plot(db$YearBuilt)
plot(db$YearRemodAdd)
plot(db$BsmtFullBath)
plot(db$BsmtHalfBath)
plot(db$FullBath)
plot(db$HalfBath)
plot(db$BedroomAbvGr)
plot(db$KitchenAbvGr)
plot(db$TotRmsAbvGrd)
plot(db$Fireplaces)
plot(db$GarageYrBlt)
plot(db$GarageCars)
plot(db$MoSold)
plot(db$YrSold)


##Gráficos de variables cualitativas
t1<-table(db$MSZoning)
barplot(t1,  main = "MSZoning")
t2<-table(db$Street)
barplot(t2,  main = "Street")
t3<-table(db$Alley)
barplot(t3,  main = "Alley")
t4<-table(db$LotShape)
barplot(t4,  main = "LotShape")
t5<-table(db$LandContour)
barplot(t5,  main = "LandContour")
t6<-table(db$Utilities)
barplot(t6,  main = "Utilities")
t7<-table(db$LotConfig)
barplot(t7,  main = "LotConfig")
t8<-table(db$LandSlope)
barplot(t8,  main = "LandSlope")
t9<-table(db$Neighborhood)
barplot(t9,  main = "Neighborhood")
t10<-table(db$Condition1)
barplot(t10,  main = "Condition1")
t11<-table(db$Condition2)
barplot(t11,  main = "Condition2")
t12<-table(db$BldgType)
barplot(t12,  main = "BldgType")
t13<-table(db$HouseStyle)
barplot(t13,  main = "HouseStyle")
t14<-table(db$RoofStyle)
barplot(t14,  main = "RoofStyle")
t15<-table(db$RoofMatl)
barplot(t15,  main = "RoofMatl")
t16<-table(db$Exterior1st)
barplot(t16,  main = "Exterior1st")
t17<-table(db$Exterior2nd)
barplot(t17,  main = "Exterior2nd")
t18<-table(db$MasVnrType)
barplot(t18,  main = "MasVnrType")
t19<-table(db$ExterQual)
barplot(t19,  main = "ExterQual")
t20<-table(db$ExterCond)
barplot(t20,  main = "ExterCond")
t21<-table(db$Foundation)
barplot(t21,  main = "Foundation")
t22<-table(db$BsmtQual)
barplot(t22,  main = "BsmtQual")
t23<-table(db$BsmtCond)
barplot(t23,  main = "BsmtCond")
t24<-table(db$BsmtExposure)
barplot(t24,  main = "BsmtExposure")
t25<-table(db$BsmtFinType1)
barplot(t25,  main = "BsmtFinType1")
t26<-table(db$BsmtFinType2)
barplot(t26,  main = "BsmtFinType2")
t27<-table(db$Heating)
barplot(t27,  main = "Heating")
t28<-table(db$HeatingQC)
barplot(t28,  main = "HeatingQC")
t29<-table(db$CentralAir)
barplot(t29,  main = "CentralAir")
t30<-table(db$Electrical)
barplot(t30,  main = "Electrical")
t31<-table(db$KitchenQual)
barplot(t31,  main = "KitchenQual")
t32<-table(db$Functional)
barplot(t32,  main = "Functional")
t33<-table(db$FireplaceQu)
barplot(t33,  main = "FireplaceQu")
t34<-table(db$GarageType)
barplot(t34,  main = "GarageType")
t35<-table(db$GarageFinish)
barplot(t35,  main = "GarageFinish")
t36<-table(db$GarageQual)
barplot(t36,  main = "GarageQual")
t37<-table(db$GarageCond)
barplot(t37,  main = "GarageCond")
t38<-table(db$PavedDrive)
barplot(t38,  main = "PavedDrive")
t39<-table(db$PoolQC)
barplot(t39,  main = "PoolQC")
t40<-table(db$Fence)
barplot(t40,  main = "Fence")
t41<-table(db$MiscFeature)
barplot(t41,  main = "MiscFeature")
t42<-table(db$SaleType)
barplot(t42,  main = "SaleType")
t43<-table(db$SaleCondition)
barplot(t43,  main = "SaleCondition")


#Correlación de las variables numéricas
df1<-data.frame(db$LotFrontage, db$LotArea, db$MasVnrArea, db$BsmtFinSF1, db$BsmtFinSF2, db$BsmtUnfSF)
df2<-data.frame(db$TotalBsmtSF, db$X1stFlrSF, db$X2ndFlrSF, db$LowQualFinSF, db$GrLivArea, db$GarageArea)
df3<-data.frame(db$WoodDeckSF, db$OpenPorchSF, db$EnclosedPorch, db$ScreenPorch, db$PoolArea, db$MiscVal)
df4<-data.frame(db$MSSubClass, db$OverallQual, db$OverallCond, db$YearBuilt, db$YearRemodAdd, db$BsmtFullBath)
df5<-data.frame(db$BsmtHalfBath, db$FullBath, db$HalfBath, db$BedroomAbvGr, db$KitchenAbvGr, db$TotRmsAbvGrd)
df6<-data.frame(db$Fireplaces, db$GarageYrBlt, db$GarageCars, db$MoSold, db$YrSold)

corrplot::corrplot(cor(df1))
corrplot::corrplot(cor(df2))
corrplot::corrplot(cor(df3))
corrplot::corrplot(cor(df4))
corrplot::corrplot(cor(df5))
corrplot::corrplot(cor(df6))


#Tablas de frecuencia de variables categóricas
t1
t2
t3
t4
t5
t6
t7
t8
t9
t10
t11
t12
t13
t14
t15
t16
t17
t18
t19
t20
t21
t22
t23
t24
t25
t26
t27
t28
t29
t30
t31
t32
t33
t34
t35
t36
t37
t38
t39
t40
t41
t42
t43
