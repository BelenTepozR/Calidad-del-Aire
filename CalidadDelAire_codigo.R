library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(dendextend)
library(reshape)
library(readxl)
library(caTools)
library(lattice)
library(caret)
library(kernlab)
library(e1071)
library(NbClust)
library(ellipse)
library(stats)
setwd("C:/Users/btepo/Documents/R/final")
PM10_2019 <- read_excel("2019PM10.xls", na = "-99")
str(PM10_2019)
colnames(PM10_2019) <- c("FECHA","PM10_MER","PM10_PED","PM10_TLA","PM10_XAL","PM10_LOM","PM10_LPR","PM10_NEZ","PM10_SHA","PM10_UIZ")
# View(PM10_2019)

PM25_2019 <- read_excel("2019PM25.xls", na = "-99")
PM25_2019 <- PM25_2019[1:61,]
str(PM25_2019)
colnames(PM25_2019) <- c("FECHA","PM25_MER","PM25_TLA","PM25_COY","PM25_UIZ","PM25_SAG","PM25_PED","PM25_XAL")
# View(PM25_2019)


PST_2019 <- read_excel("2019PST.xls", na = "-99")
str(PST_2019)
colnames(PST_2019) <- c("FECHA","PST_MER","PST_PED","PST_TLA","PST_XAL","PST_UIZ")
# View(PST_2019)



datos <-  cbind(PM10_2019, PM25_2019[,2:8])
datos <- cbind(datos,PST_2019[,2:6])
#datos <- datos[,c(-9,-13,-18,-19)]
datos[is.na(datos)] <- 0
#datos <- na.omit(datos)
datos <- datos[order(datos$FECHA),]
datos <- as.data.frame(datos)
str(datos)
View(datos)
 head(datos, n=10)
 tail(datos, n=10)
length(which(duplicated(datos$FECHA)))

datos[,11:17] <- as.numeric(unlist(datos[,11:17]))
datosLimpios <- datos[,-1]
datosLimpios<-as.data.frame(t(datosLimpios))
str(datosLimpios)
colnames(datosLimpios) <- datos[,1]
datosLimpios$DATOS_ENERO = rowMeans(datosLimpios[ , 1:5])
datosLimpios$DATOS_FEBRERO = rowMeans(datosLimpios[ , 6:10])
datosLimpios$DATOS_MARZO = rowMeans(datosLimpios[ , 11:15])
datosLimpios$DATOS_ABRIL = rowMeans(datosLimpios[ , 16:21])
datosLimpios$DATOS_MAYO = rowMeans(datosLimpios[ , 22:25])
datosLimpios$DATOS_JUNIO = rowMeans(datosLimpios[ , 26:30])
datosLimpios$DATOS_JULIO = rowMeans(datosLimpios[ , 31:35])
datosLimpios$DATOS_AGOSTO = rowMeans(datosLimpios[ , 36 :41])
datosLimpios$DATOS_SEPTIEMBRE = rowMeans(datosLimpios[ , 42:46])
datosLimpios$DATOS_OCTUBRE = rowMeans(datosLimpios[ , 47:51])
datosLimpios$DATOS_NOVIEMBRE = rowMeans(datosLimpios[ , 52:56])
datosLimpios$DATOS_DICIEMBRE = rowMeans(datosLimpios[ , 57:61])
datosLimpiosM <- datosLimpios[,62:73]
View(datosLimpiosM)

set.seed(123)
data <-  sapply(datosLimpiosM, scale)
data <- as.data.frame(data)
row.names(data) <- row.names(datosLimpios)
View(data)

# Silhouette method
fviz_nbclust(data, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")



set.seed(1234)
kmeans <- kmeans(data, 3, nstart = 10)
kmeans$cluster
kmeans$centers
kmeans$totss
kmeans$betweenss
kmeans$withinss
kmeans$tot.withinss
datosLimpiosM$cluster <- kmeans$cluster
dataset <- as.data.frame(datosLimpiosM) 

vy <- aggregate( dataset[-13], list(cluster= kmeans$cluster ), mean)
plot(vy)
fviz_cluster(kmeans, data=data,palette =c("#FF00FF","#0000FF","#FF00FF"),
             ellipse.type ="convex",star.plot=TRUE, repel=TRUE, 
             ggtheme = theme_minimal())

print(kmeans)

ds <- NULL
ds <- f[1,-1]
ds <- as.data.frame(t(ds))
colnames(ds) <- c("cluster1")
dat1 <- c("CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1","CLUSTER1")
r <- rownames(ds)
as.data.frame(r)
ds <- cbind(r,ds)
ds1 <- cbind(ds,dat1)
rownames(ds1) <- NULL
colnames(ds1) <- c("DATOS POR MES","DATOS_PROMEDIO_X_MES", "CLUSTER")
ds <- NULL
ds <- f[2,-1]
ds <- as.data.frame(t(ds))
dat1 <- c("CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2","CLUSTER2")
r <- rownames(ds)
as.data.frame(r)
ds <- cbind(r,ds)
ds2 <- cbind(ds,dat1)
rownames(ds2) <- NULL
colnames(ds2) <- c("DATOS POR MES","DATOS_PROMEDIO_X_MES", "CLUSTER")
ds <- NULL
ds <- f[3,-1]
ds <- as.data.frame(t(ds))
dat1 <- c("CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3","CLUSTER3")
r <- rownames(ds)
as.data.frame(r)
ds <- cbind(r,ds)
ds3 <- cbind(ds,dat1)
rownames(ds3) <- NULL
colnames(ds3) <- c("DATOS POR MES","DATOS_PROMEDIO_X_MES", "CLUSTER")
ds <-  rbind(ds1,ds2)
ds <-  rbind(ds,ds3)
ds

colnames(ds) <- c("MES","DATOS_PROMEDIO_X_MES", "CLUSTER")
ds
Grafico = ggplot(data=ds, aes(x=CLUSTER, y=DATOS_PROMEDIO_X_MES, fill=MES)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("GreenYellow", "purple","DarkMagenta", "pink", "orange","blue","Gray","green","Gold","HotPink","DeepSkyBlue","DarkViolet"))
Grafico
c1
c1$Nombre
c2$Nombre
c3$Nombre
View(ds1)
View(ds2)
View(ds3)

write_csv(datosLimpiosM, file = "C:/Users/btepo/Desktop/data.csv")
write.xlsx(vy, "C:/Users/btepo/Desktop/vy.xlsx", sheetName = "vy", row.names = TRUE)