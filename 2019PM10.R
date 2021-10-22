library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(dendextend)
library(reshape)
library(readxl)
setwd("C:/Users/btepo/Documents/R/final")
X2019PM10 <- read_excel("2019PM10.xls", na = "-99")
View(X2019PM10)
datos <- as.data.frame(X2019PM10)
View(datos)
datos <- datos[,-9]
datos <- na.omit(datos)
datos <- datos[order(datos$FECHA),]

length(which(duplicated(datos$FECHA)))

data <-  sapply(datos[,2:9], scale)
row.names(data) <- datos[,1]
View(data)
d.norm <- dist(data, method = "euclidean")
as.matrix(d.norm)[1:9, 1:9]

hc1 <- hclust(d.norm, method = "complete")
re.cop <- cophenetic(hc1)
complete <- cor(d.norm, re.cop)
hc1 <- hclust(d.norm, method = "single")
re.cop <- cophenetic(hc1)
single <- cor(d.norm, re.cop)
hc1 <- hclust(d.norm, method = "average")
re.cop <- cophenetic(hc1)
average <- cor(d.norm, re.cop)
hc1 <- hclust(d.norm, method = "centroid")
re.cop <- cophenetic(hc1)
centroid <- cor(d.norm, re.cop)

t <- c("complete", "single", "average", "centroid")
c <- c(complete, single, average, centroid)
names(c) <- t
(c)

hc1 <- hclust(d.norm, method = "complete")

# Elbow method
fviz_nbclust(data, hcut, method = "wss") +
  geom_vline(xintercept =3 , linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(data, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(data, hcut ,nstart= 25 ,method= "gap_stat", nboot= 50)+
  labs (subtitle ="Gap statistic method")

plot(hc1,cex = 0.6, hang = -1, main = "Dendograma Pharmaceuticals")
grp <-  cutree(hc1, k = 2)
table(grp)

rownames(data)[grp == 2]

fviz_dend(hc1, k = 2,cex = 0.5, colors = c("#FF00FF" ,"#000080"),
          color_labels_by_k = TRUE ,horiz = TRUE,rect = TRUE, 
          rect_border = c("#FF00FF","#2E9FDF"),
          rect_fill = TRUE)+
  geom_hline(yintercept = 3, linetype = "twodash", color = "#7FFF00", size = 1.5) +
  labs(title = "Herarchical clustering", subtitle = "LifeCycleSavings.csv, Enlace Average, K = 2"
  )  

fviz_cluster(list(data = datos[,-c(1,9)], cluster = grp),
             palette = c("#2E9FDF", "#FF00FF","#00AFBB"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())