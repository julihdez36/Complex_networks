# Taller 1

library(igraph)


# Punto 1 -----------------------------------------------------------------

# Red binaria no dirigida G(n,m) n nodos y orden m
set.seed(123)
g <- sample_gnm(7,10)
V(g)
vcount(g) # Orden del grafo

E(g)
ecount(g) # Tamaño del grafo

as_edgelist(g) # Lista de relaciones
as_adjacency_matrix(g, sparse = F) # Matriz de adyacencia

set.seed(123)
plot(g)

# Red ponderada no dirigida

## Empecemos por asignar pesos a las conexiones de nuestra red

wg <- g

set.seed(123)
E(wg)$weight <- round(runif(n = ecount(wg)), 3)

E(wg)$weight # Visualizo los pesos creados
is_weighted(wg) # Pregunto si es ponderada


as_data_frame(wg) #Puedo ver los pesos por cada conexión
as_adjacency_matrix(wg,sparse = F, attr = 'weight')

set.seed(123)
plot(wg,edge.width = E(g)$weight)

# Red binaria dirigida

dg <- sample_gnm(3, 4, directed = TRUE)
set.seed(123)
plot(dg)

vcount(dg) # Orden del gráfo
ecount(dg) # Tamaño del grafo

#Conexión
is.connected(dg, mode = 'weak') # Conexión debil
is.connected(dg, mode = 'strong') # No conexión fuerte


# Punto 2 -----------------------------------------------------------------

# Consideramos un grafo G(V,E) con los siguientes edges

enla <- c(1,2,1,3,2,3,2,4,2,5,3,5,4,5)

# Lo convertimos en un grafo
g <- graph(edges = enla,directed = F)

# Lo visualizamos 
set.seed(123)
plot(g)

# Orden, tamaño y diámetro del gráfo

sprintf('El orden del gráfo es de %d', vcount(g))
sprintf('El tamaño del gráfo es de %d', ecount(g))
sprintf('El diametro del gráfo es %d', diameter(g))

# Grado de cada vértice

degree(graph = g, v = V(g)) #Grado de todos los vértices

# Graficar subgrafo generado por los nodos 1,2,3 y 4

nodos_subgrafo <- c(1, 2, 3, 4)

subgrafo <- induced_subgraph(g, nodos_subgrafo) # Obtener el subgrafo
set.seed(123)

plot(subgrafo)


# Punto 3 -----------------------------------------------------------------

# Consideramos un digrafo G(V,E) con los siguientes edges

enla3 <- c(1,3,2,3,2,4,2,5,3,1,3,5,4,5,5,4)

# Lo convertimos en un grafo
g3 <- graph(edges = enla3,directed = T)

# Lo visualizamos 
set.seed(123)
plot(g3)

# Orden, tamaño y diámetro del gráfo

sprintf('El orden del gráfo es de %d', vcount(g3)) # 5
sprintf('El tamaño del gráfo es de %d', ecount(g3)) # 8
sprintf('El diametro del gráfo es %d', diameter(g3)) # 3

# Grado de cada vértice

degree(graph = g3, v = V(g3), mode = 'in') #Grados de entrada
degree(graph = g3, v = V(g3), mode = 'out')  #Grados de salida

# Graficar subgrafo generado por los nodos 1,2,3 y 4

nodos_subgrafo <- c(1, 2, 3, 4)

subgrafo3 <- induced_subgraph(g3, nodos_subgrafo) # Obtener el subgrafo
set.seed(123)
plot(subgrafo3)


# Punto 4 -----------------------------------------------------------------

# Empecemos por graficar la triada; para ello, definamosla

# Triada o tripleta
# Una tríada es un conjunto de tres vertices

# Estados triádicos dirigidos

# Cuántaos estádos triadicos no dirigidos puede haber? 2^3 = 8
# Ninguna, una, dos o tres aristas

# Definamos vertices

vertices <- c("v1", "v2", "v3")

# Generemos una lista con el número de aristas posibles por cada grafo

edgelists <- list(
  c(),                   # Ninguna arista
  c("v1", "v2"),         # Una arista (v1 - v2)
  c("v1", "v3"),         # Una arista (v1 - v3)
  c("v2", "v3"),         # Una arista (v2 - v3)
  c("v1", "v2", "v1", "v3"),   # Dos aristas (v1 - v2, v1 - v3)
  c("v1", "v2", "v2", "v3"),   # Dos aristas (v1 - v2, v2 - v3)
  c("v1", "v3", "v2", "v3"),   # Dos aristas (v1 - v3, v2 - v3)
  c("v1", "v2", "v1", "v3", "v2", "v3")  # Tres aristas (triángulo completo)
)


# Los convertimos en grafos para tener los estados triadicos no dirigidos

for (i in 1:length(edgelists)) {
  edgelists[[i]] <- graph(edgelists[[i]],directed = F)
}

edgelists[[1]] <- make_empty_graph(n = 3, directed = FALSE)
V(edgelists[[1]])$name <- vertices

# Los graficamos

par(mfrow=c(3,3))
for (i in edgelists) {
  plot(i, main = sprintf('Grafo con %d aristas', ecount(i)))
}

# Valoremos ahora cuales de estos grafos son isomorfos

# Mostremos cuales de estos grafos son isomorfos

prueba_isomorfismo <- list()
for (i in 1:(length(edgelists) - 1)) {
  for (j in (i + 1):length(edgelists)) {
    iso <- isomorphic(edgelists[[i]], edgelists[[j]])
    if(iso == T){
    cat(sprintf("Grafo %d y Grafo %d son isomorfos: %s\n", i, j, iso), "\n")}
  }
} 

# Punto 6 -----------------------------------------------------------------



