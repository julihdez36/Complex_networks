

# Distribución de grado ---------------------------------------------------
# Analizar la distribución del grado es el primer paso para analizar una red

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




# Centralidad de intermediaación (betweenness centrality)

# Centralidad propia (eigenvector centrality)


