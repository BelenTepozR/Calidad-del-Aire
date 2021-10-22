setwd("C:/Users/btepo/Documents/R/final")
library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(dendextend)
library(reshape)
library(readxl)
X2019PM25 <- read_excel("2019PM25.xls", na = "-99")
View(X2019PM25)
datos <- as.data.frame(X2019PM25)
View(datos)
datos <- datos[,-4]
datos <- na.omit(datos)
datos <- datos[order(datos$FECHA),]

length(which(duplicated(datos$FECHA)))
str(datos)
datos[,2:7] <- as.numeric(unlist(datos[,2:7]))
data <-  sapply(datos[,2:7], scale)
row.names(data) <- datos[,1]
View(data)
d.norm <- dist(data, method = "euclidean")
as.matrix(d.norm)[1:6, 1:6]

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

hc1 <- hclust(d.norm, method = "average")

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
grp <-  cutree(hc1, k = 3)
table(grp)

rownames(data)[grp == 2]

fviz_dend(hc1, k = 2,cex = 0.5, colors = c("#FF00FF" ,"#000080","#FF1493","#FFD700","#4B0082","#00BFFF"),
          color_labels_by_k = TRUE ,horiz = TRUE,rect = TRUE, 
          rect_border = c("#FF00FF" ,"#000080","#FF1493","#FFD700","#4B0082","#00BFFF"),
          rect_fill = TRUE)+
       # geom_hline(yintercept = 3, linetype = "twodash", color = "#7FFF00", size = 1.5) +
          labs(title = "Herarchical clustering", subtitle = "LifeCycleSavings.csv, Enlace Average, K = 2")  

fviz_cluster(list(data = datos[,-1], cluster = grp),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())






install.packages("gganimate")
library(ggplot2)
library(gganimate)
library(tidyverse)
df_evolution_data <- data.frame(Name=rep(c("Madrid","Barcelona",   # Creamos el dataset
                                           "Valencia","Alicante",
                                           "Sevilla"),5),
                                Year = factor(sort(rep(2001:2005, 5))),
                                Value = runif(25,100,1000))
df_evolution_data_filtered <- df_evolution_data %>%
  group_by(Year) %>%                                               
  mutate(Rank = rank(Value)) %>%                                   # Añadimos la columna rank y
  filter(Rank >= 2)       
# descartamos el de menor valor
ggplot(df_evolution_data_filtered) +
  geom_col(aes(x=Rank,                          # Creamos el gráfico de barras
               y=Value,
               group=Name,                      # Afrumando y filleando por ciudad
               fill=Name),
           width=0.4) +
  geom_text(aes(x=Rank,                         # Etiquetamos las barras con los nombres
                y=0,
                label=Name,
                group=Name),
            hjust=1.25) +
  theme_minimal() +                             # Elegimos un theme que no sea gris
  ylab('Value') + 
  theme(axis.title.y = element_blank(),         # Eliminamos los labels y titles 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(5,5,5,5), 'lines')) +  # Escogemos el zoom
  scale_fill_brewer(palette="Dark2") +          # Paleta de colores de las barras
  coord_flip(clip='off') +                      # Hacemos las barras horizontales
  ggtitle('{closest_state}') +                  # Tilulo == al valor de la columna que
  transition_states(Year,                       # Animamos la columna Year
                    transition_length = 1,      # Duración de la animación de transición
                    state_length = 1) +         # Duración de cada Year
  exit_fly(x_loc = 0, y_loc = 0) +              # Salida de la ciudad no top4
  enter_fly(x_loc = 0, y_loc = 0)               # Entrada de la ciudad al top4
