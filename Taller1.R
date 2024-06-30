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

plot(subgrafo)


# Punto 3 -----------------------------------------------------------------

# Triada o tripleta
# Una tríada es un conjunto de tres nodos conectados por tres aristas
# El censo de una tríada se define como el número de subgrafos cerrados
# de tamaño tres que contienen la tríada.



# Punto 4 -----------------------------------------------------------------

# Graficar todos los grafos conectados con 4 vértices.

components(g)
