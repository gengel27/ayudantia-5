library(tidyverse)
library(ggplot2)
library(dplyr)

sanguchez <- read.csv2("/Users/gabrielengel/Downloads/sanguchez.csv")
s <- sanguchez[c(4, 6)]
s$Precio = as.numeric(gsub("\\$", "", s$Precio))
s<- na.omit(s)

s$Precio<- as.numeric(s$Precio)
s$nota <- as.numeric(s$nota)

data_s<- s[, colnames(s) %in%  c("Precio", "nota")]
escal_s<- scale(data_s) %>% as_tibble()



escal_s %>% summary()


modelo_kmeans <- kmeans(escal_s, centers = 10)
modelo_kmeans2 <- kmeans(data_s, centers = 10)

escal_s$clus <- modelo_kmeans$cluster %>% as.factor()
data_s$clus <- modelo_kmeans2$cluster %>% as.factor()

ggplot(escal_s, aes(Precio, nota, color=clus)) + 
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()

ggplot(data_s, aes(Precio, nota, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()

info_clus <- modelo_kmeans$centers
info_clus2 <- modelo_kmeans2$centers


SSinterior <- numeric(30)

for (k in 1:30) {
  modelo<- kmeans(escal_s, centers = k)
  SSinterior[k] <- modelo$tot.withinss
}
plot(SSinterior)

#metodo del codo 2

k.max <- 30
wss1 <- sapply(1:k.max, function(k){kmeans(escal_s, k, nstart=50, iter.max = 8)$tot.withinss})

wss2<- sapply(1:k.max, function(k){kmeans(data_s, k, nstart=50, iter.max=8)$tot.withinss})

plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Numeros de clusters K",
     ylab="Total within-clusters sum of squares")

plot(1:k.max, wss2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Numeros de clusters K",
     ylab="Total within-clusters sum of squares")



#evaluacion

escal_s$clus <- as.numeric(escal_s$clus)
data_s$clus <- as.numeric(data_s$clus)

# uso distancia euclidiana
tempDist <- dist(escal_s) %>% as.matrix()

#reordeno filas y columnas en base al cluster obtenido
index <- sort(modelo_kmeans$cluster, index.return=TRUE)
tempDist <- tempDist[index$ix,index$ix]
rownames(tempDist) <- c(1:nrow(escal_s))
colnames(tempDist) <- c(1:nrow(escal_s))

image(tempDist)
