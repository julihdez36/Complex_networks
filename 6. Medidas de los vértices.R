

# Distribución de grado ---------------------------------------------------
# Analizar la distribución del grado es el primer paso para analizar una red

library(igraph)
library(sand)

data(yeast) # datos
vcount(yeast) # Orden 2617
ecount(yeast) # Tamaño
is_directed(yeast)
is_weighted(yeast)

d <- degree(yeast) #Grádo de cada vértice
length(d) # Orden de 2617 

unique(d)
length(dd)

max(table(degree(yeast))) / 2617
max(degree_distribution(yeast))

# Distribución de grado

dd <- degree_distribution(yeast)

# Gráfico
windows()
plot((0:max(d))[dd != 0], dd[dd != 0], log = "xy", pch = 16, col = adjustcolor("royalblue", 0.5),
     xlab = "Log-grado", ylab = "Log-densidad", main = "Distribución de grado (log-log)")

mnd <- knn(graph = yeast, vids = V(yeast))$knn
mean(d[as.numeric(neighbors(graph = yeast, v = 1))])

# visualización: GPV vs. grado
plot(x = d, y = mnd, log = "xy", pch = 16, col = adjustcolor("yellow3", 0.5),
     xlab = "Log-grado", ylab = "Log-grado promedio de los vecinos")


# Medidas de centralidad --------------------------------------------------

# Las medidas de centralidad permiten cuantificar la noción de importancia 
# de los nodos de una red

# Centralidad de cercania (closeness centrality)

## Un vértice se considera importante si está cerca de muchos otros vértices
## Se calcula como el inverso de la suma de las distancias geodésicas
## La distancia geodésica es la caminata mas corta a un vértice

data("karate")
sprintf('La base karate es de orden %d y tiene un tamaño %d',
         vcount(karate), ecount(karate)) # orden 34, tamaño 78

# Calculamos la matriz de distancia que nos da la distancia mas corta entre los
# nodos de un gráfico

distances(graph = karate)[1:5,1:5] #Previsualización de la salida
D <- distances(graph = karate)

# closeness centrality no normalizada

head(closeness(graph = karate, normalized = F)) #centralidad de cercanias

# Visualicemos las cercanías junto con su calculo de la sumatoria inversa (row, columns)
# Validamos con dan lo mismo

head(
  cbind(
    closeness(graph = karate, normalized = F), 
    1/apply(X = D, MARGIN = 1, FUN = sum), 
    1/apply(X = D, MARGIN = 2, FUN = sum)), n = 5)

# closenees centrality normalizada

n <- vcount(karate) #Orden de la red (número de vértices)

head(
  cbind(
    closeness(graph = karate, normalized = T), 
    (n - 1)/apply(X = D, MARGIN = 1, FUN = sum), 
    (n - 1)/apply(X = D, MARGIN = 2, FUN = sum)), n = 5)

# Top 5: nodos mas importantes por cercanias 

cc <- closeness(graph = karate, normalized = T)
head(sort(cc, decreasing = T), n = 5) 

# Centralidad de intermediaación (betweenness centrality)
# Un nodo se considera importante si se encuentra entre dos pares de vértices
# En general, un vértice se encuentra en muchos caminos, es decir, son cruciales
# para el proceso de comunicación

# betweenness centraliy no normalizada
head(x = betweenness(graph = karate, normalized = F), n = 5)

# betweenness centrality normalizada
n <- vcount(karate)
head(
  cbind(
    betweenness(graph = karate, normalized = F)/((n-1)*(n-2)/2), 
    betweenness(graph = karate, normalized = T)), n = 5)

# top 5: nodos mas importantes por intermediación

bc <- betweenness(graph = karate, normalized = T)
head(sort(bc, decreasing = T), n = 5)

# Centralidad propia (eigenvector centrality)
# Un vértice se considera importante si sus vecinos son a su vez centrales

# matriz de adyacencia
Y <- as_adjacency_matrix(karate, sparse = F)
g <- graph_from_adjacency_matrix(Y)
evd <- eigen(Y)
# eigen centraliy no normalizada
head(
  cbind(
    eigen_centrality(graph = karate, scale = F)$vector,
    eigen_centrality(graph = g, scale = F)$vector,
    Y%*%c(1/evd$values[1]*(-1)*evd$vectors[,1])), n = 5)  # vector propio x (-1)

# eigen centraliy normalizada
head(
  cbind(
    eigen_centrality(graph = karate, scale = T)$vector,
    eigen_centrality(graph = g, scale = T)$vector,
    Y%*%c(1/evd$values[1]*(-1)*evd$vectors[,1])/max(Y%*%c(1/evd$values[1]*(-1)*evd$vectors[,1]))), n = 5)

# top 5: nodos mas importantes por centralida dpropia
ec <- eigen_centrality(graph = karate, scale = T)$vector
head(sort(ec, decreasing = T), n = 5)


# Interacciones sociales --------------------------------------------------


# Dibujaremos los nodos que nos permita identificar la importancia o 
# centralidad de los nodos 

# medidas de centralidad
dc <- degree          (graph = karate, normalized = T)
cc <- closeness       (graph = karate, normalized = T)
bc <- betweenness     (graph = karate, normalized = T)
ec <- eigen_centrality(graph = karate, scale = T)$vector

# visualizacion
windows()
par(mfrow = c(2,2), mar = c(4, 3, 3, 1))
set.seed(123)
l <- layout_with_dh(karate)
plot(karate, layout = l, vertex.size = 15*sqrt(dc), vertex.frame.color = "black", vertex.label = NA, main = "Grado")
plot(karate, layout = l, vertex.size = 15*sqrt(cc), vertex.frame.color = "black", vertex.label = NA, main = "Cercania")
plot(karate, layout = l, vertex.size = 15*sqrt(bc), vertex.frame.color = "black", vertex.label = NA, main = "Intermediación")
plot(karate, layout = l, vertex.size = 15*sqrt(ec), vertex.frame.color = "black", vertex.label = NA, main = "Propia")


# Centralidad con digrafos ------------------------------------------------

# data
data(aidsblog)
aidsblog <- upgrade_graph(aidsblog)

vcount(aidsblog) # orden 146
ecount(aidsblog) # tamaño 187

is_directed(aidsblog) #es dirigida, true
is_weighted(aidsblog) #es ponderada, false

# Los centros son importante por la cantidad de vérrices centrales a los que señalan
# Autoridades son importanes por la cantidad de vértices centrales que los señalan

# centros y autoridades
hs <- igraph::hub_score(graph = aidsblog, scale = T)$vector
as <- igraph::authority_score(graph = aidsblog, scale = T)$vector


# visualización

windows()

set.seed(123)
l <- layout_with_kk(aidsblog)
par(mfrow = c(1,2), mar = c(4, 3, 3, 1))
plot(aidsblog, layout = l, vertex.label = NA, vertex.size=15*sqrt(hs), main = "Centros")
plot(aidsblog, layout = l, vertex.label = NA, vertex.size=15*sqrt(as), main = "Autoridades")
