
# Descripción de redes

# 1. Técnicas de visualización (se exploraron diferentes algoritmos)
# 2. Medidas de centralización o importancia relativa: grado, cercanía, 
# intermediación, valores propios; para digrafos hubs y autoridades
# 3. Conectividad: censos de cliques, densidad, transitividad;
# para digrafos, reciprocidad


# Asortatividad -----------------------------------------------------------

# ¿Cómo las caracteristicas de los nodos influyen en la relación entre estos? 
# Esto lo evaluamos a travéde de la asortatividad que puede pensarse como la

# La asortatividad es la preferencia de los nodos de una red por unirse a 
# otros que le son similares en alguna característica

# La homofilia se refiere a que nodos similares tienen a relacionarse entre sí.
# Fenomeno que suele evidenciarse en diversos sistemas

# Los coeficientes de asortatividad miden el nivel de homofilia (homophily)
# del grafo, basándose en las etiquetas o valores asignados a los vértices.

# El coeficiente de asortatividad va entre -1 y 1. 1 indica un fuerte caracter
# asortativo. -1 indica un caracter disortativo. 0 implica que no hay
# asortatvidad.

# Veamos el ejemplo de proteinas

library(igraph)
library(igraphdata)

data("yeast")

ecount(yeast) # Tamaño 11855
vcount(yeast) # Orden 2617 nodos

# Observemos las clases de nodos que hay en la red
table(V(yeast)$Class)

# Quiero validar si todos los nodos están clasificados

sum(table(V(yeast)$Class)) # 2577, distinto al orden

# validemos si hay NAs

table(is.na(V(yeast)$Class)) # 40 missing

# Qué podemos hacer con esos vacios? 
# Se puede imputar aleatoriamente siguiendo la distribución de probabilidad 
# de las clases. En este caso, se procedio a asignarlo a una sola clase

# clase P: desempeñan un papel en la síntesis de proteínas
v.types <- (V(yeast)$Class == "P") + 1
v.types[is.na(v.types)] <- 1
table(v.types)

# Veamos si la clase motiva o no la homofilia con la
# asortatividad nominal

assortativity_nominal(graph = yeast, types = v.types, directed = F)

# 0.5232879, lo que indica que la clase si motiva la homofilia

# asortatividad grado
assortativity_degree(yeast) # 0.4610798


# Interacciones sociales --------------------------------------------------

# datos
data(karate)
karate <- upgrade_graph(karate)
# la representación de datos internos a veces cambia entre versiones
Y <- as.matrix(get.adjacency(graph = karate, names = F))

# orden
vcount(karate)

# tamaño
ecount(karate)

# visualización
windows()
par(mfrow = c(1,1), mar = c(4, 3, 3, 1))
set.seed(123)
plot(karate, layout = layout_with_dh, vertex.size = 10, vertex.frame.color = "black", vertex.label.color = "black", main = "")

# Creamos dos clases según los dos grupos que se separaron
v.types <- V(karate)$Faction 
# asortatividad nominal
assortativity_nominal(graph = karate, types = v.types, directed = F)
# 0.7434211 bastante alta

# asortatividad grado
assortativity_degree(karate) # -0.4756131

