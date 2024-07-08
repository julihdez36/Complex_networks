
# Caracterización de la conectividad --------------------------------------

# Nos preguntamos qué tan conectados están los vértices
# La cohesión se refiere a la medida en que subconjuntos de vértices
# específicos son cohesivos (adherentes) respecto a la relación que
# define las aristas.

# Podemos evaluar si existe una conectividad local (vecindades) o global (red)


# Subgrafos ---------------------------------------------------------------


# un enfoque para definir la cohesión de una red es medianta la especificación
# de ciertos subgrafos de interés

# Entre mas subgrafos conectados, puedo definir conectividad

# Los cliques son subgrafos completos (todos los vértices son accesibles)

# Un clique maximal es aquel que no es un subconjunto de un clique mas grande

# Los cliques grandes requieren que el grado sea denso.

library(igraph)
library(sand)

# Veamos un ejemplo sencillo

g <- graph(edges = c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5,6,7,6,8,7,8,9,10,1,6,2,9,7,9), directed = F)
Y <- as.matrix(get.adjacency(graph = g, names = F)) # Matriz de adyacencia

# Visualización

windows()
par(mfrow = c(1,2), mar = c(4, 3, 3, 1))

# Representación del grafo
set.seed(42)
plot(g, vertex.size = 20, vertex.color = 0, vertex.label.color = "black", edge.color = "blue4")

# Representación de la matriz de adyacencia
corrplot::corrplot(corr = Y, col.lim = c(0,1), method = "color", tl.col = "black", addgrid.col = "gray", cl.pos = "n")

vcount(g) # orden 10
ecount(g) # tamaño 17

# clan?
# Crea subgrafo inducido que pasa por 6,7 y 8
c1 <- induced_subgraph(graph = g, vids = c(6,7,8)) 

# Comparamos las atistas con la función ecount que devuelve el número de
# aristas del subgrafo c1
ecount(c1) == choose(n = vcount(c1), k = 2) 

# Veamos los cliques

cliques(g) # La salida es una lista con el número de cliques
length(cliques(g) ) # Tenemos 44 cliques o sugrafos en nuestra red

# Dado que queremos la frecuencias del orden de cada clique, usaremos
# la cunficón table

# Encontrar el número de cliques de cada tamaño (orden, en verdad)

table(sapply(cliques(g), length)) # El clique maximal es 5 y sólo hay uno


# cliques maximales (no contenido en ningun otro)
maximal.cliques(graph = g)

# cliques máximos (mas grandes)
largest.cliques(graph = g)

# número clan
clique.number(graph = g)

# En la práctica, clanes “grandes” son escasos, ya que requieren que el grafo
# sea denso, pero las redes reales comúnmente son dispersas (sparse).

# Aplicación Karate -------------------------------------------------------


data(karate)
Y <- as.matrix(get.adjacency(graph = karate, names = F))

vcount(karate) # Orden 34
ecount(karate) # Tamaño 78

# Visualicemosla

windows()
set.seed(123)
plot(karate, layout = layout_with_dh, vertex.size = 10, vertex.frame.color = "black", vertex.label.color = "black", main = "")

# Frecuencia de cliques
table(sapply(cliques(karate), length))

# Clique de tamaño 5

cliques(karate)[sapply(cliques(karate), length) == 5] #ya sabemos que son 2

# Cliques maximales

table(sapply(max_cliques(karate), length)) # No contenidos en otro clique

# clanes máximos
table(sapply(largest.cliques(graph = karate), length))
largest.cliques(graph = karate)

# número clique (tamaño u orden de clique mas grande)
clique.number(graph = karate)

# Interacciones proteína-proteína -----------------------------------------

# datos
data(yeast)
yeast <- upgrade_graph(yeast)
# la representación de datos internos a veces cambia entre versiones

vcount(yeast) # orden 2617
ecount(yeast) # tamaño 11855
clique.number(graph = yeast) # Numero clique 23

# Nótese que el número clique tiende a ser relativamente pequeño, 
# lo que suele ser una regularidad


# Aplicación digrafo blogs ------------------------------------------------

data(aidsblog)

vcount(aidsblog) # Orden 146
ecount(aidsblog) # tamaño 187

is_directed(aidsblog) # dirigida

# visualización

set.seed(123)
plot(aidsblog, layout = layout_with_kk, vertex.label = NA, vertex.size = 5, vertex.frame.color = 1, edge.arrow.size = 0.5, main = "")

# Dado que se trata de un multigrafo, vamos a simplificarlo
# Con esto eliminamos lo bucles (reflexividades) o aristas múltiples

is_simple(aidsblog) # FALSE

# censo de estados diádicos, es decir, ontar y clasificar las relaciones 
# entre pares de vértices en términos de la presencia o ausencia de 
# aristas entre ellos.

aidsblog <- simplify(aidsblog)

# Veamos el censo de los estados diadicos o triadicos

dyad_census(aidsblog)

#   mut   The number of pairs with mutual connections.
#   asym  The number of pairs with non-mutual connections.
#   null  The number of pairs with no connection between them.
dyad_census(aidsblog)

# Podemos ver que la conectividad, en su mayoría, va en una sola dirección

# También podemos ver el censo de estados triadicos
triad_census(aidsblog)

# La mayoría de los estados son nules y de los que no lo son, 
# casi todos son aimétricos


# Medidas estructurales ---------------------------------------------------

# Veamos medidas que nos permita valorar la conectividad del grafo


# Densidad ----------------------------------------------------------------


# la frecuencia relativa de las aristas observadas respecto al potencial de aristas.
# Cantidad de aristas que se observaron sobre total de aristas (prporción)

# Se interpreta como propoción de conexiones
# N. de aristas / No. maximo de aristas posibles

# La densidad asume valores entre 0 y 1 y se puede interpretar como 
# una medida de qué tan cerca se encuentra H de ser un clan.
# *Por lo general, en aplicaciones reales, las densidades son menores al .2

# Esta es una mediad global (de toda la red)

# densidad: usamos la f'romula combinatoria para grafos no dirigidos
# N*(N-1)/2, N = no. vertices
ecount(karate)/(vcount(karate)*(vcount(karate)-1)/2)

# También la podemos calcular así

edge_density(graph = karate) #0.1390374

# También podemos usar la matriz de adyacencia para es mismo calculo
mean(Y[lower.tri(Y, diag = F)])
mean(Y[upper.tri(Y, diag = F)])

# ego networks
g_1  <- induced_subgraph(graph = karate, vids = neighborhood(graph = karate, order = 1, nodes = 1) [[1]])
g_34 <- induced_subgraph(graph = karate, vids = neighborhood(graph = karate, order = 1, nodes = 34)[[1]])

# densidades
edge_density(graph = g_1)

edge_density(graph = g_34)


# Transitividad global ----------------------------------------------------

# La transitividad (transitivity) de un grafo se cuantifica por medio
# del coeficiente de agrupamiento

# Se calcula como no. triplas cerradas / no. triplas total

# Veamos un ejemplo

h <- graph(edges = c(1,2,1,3,2,3,1,4), directed = F)

# visualización
windows()
set.seed(123)
plot(h, vertex.size = 20, vertex.color = 0, vertex.label.color = "black", edge.color = "blue4")

# número de triángulos por vértice
count_triangles(graph = h) # Todos menos uno que está aislado

# vértices que son parte de un triángulo
triangles(graph = h) # El 1,2 y 3

# conteos de estados triádicos
(mot <- motifs(graph = h, size = 3))

# transitividad

# Manera manual
3*mot[4]/(mot[3] + 3*mot[4])

# Por funciones
transitivity(graph = h, type = "global") # 0.6


# Reciprocidad ------------------------------------------------------------

# Un concepto exclusivo de los dígrafos es la reciprocidad, i.e., 
# la propensión con la que hay reciprocidad de aristas en la red.

# no. aristas reciprocas / no. aristas

# reciprocidad (aristas)
reciprocity(aidsblog, mode = "default")

# reciprocidad (díadas)
reciprocity(aidsblog, mode = "ratio")


# Componente gigantes -----------------------------------------------------

# Comúnmente una de las componentes conectadas de un grafo
# domina a las demás en magnitud. Tal componente se denomina 
# componente gigante (giant component).

# En la práctica, la atención se restringe a la componente gigante 
# para llevar a cabo tanto el análisis como el modelamiento

# La propiedad del pequeño mundo que se refiere a la situación donde la distanca
# geodésica entre pares de vértices es generalmente bastante pequeña, y la 
# transitividad es relativamente alta


# red conectada?
is_connected(yeast) # No es posible acceder a todos lo vertices por una caminata

# componentes
componentes <- decompose(yeast)
length(componentes) # Tenemos 92 componentes 

table(sapply(X = componentes, FUN = vcount)) # Tamaño de componentes
# El último es de 2375 vertice, el cual es el componente mas grande (gigante)

# tamaño de la componte gigante
max(sapply(X = componentes, FUN = vcount)) # Validamos el tamaño del componente gigante

max(sapply(X = componentes, FUN = vcount))/vcount(yeast) #este conecta el 90% de los vértices

# componente gigante
yeast_gc <- decompose(yeast)[[1]] # Exraemos la componente mas grande

# Probemos si se cumple la propiedad del mundo pequeño
# distiancia geodésica pequeña y transitividad grande

mean_distance(yeast_gc) # 5.09597 pasos

diameter(yeast_gc) # El diametro es 15

transitivity(yeast_gc) # 0.4686663, lo que es muy grande
# subgrafo muy conectado

# Parece que se cumple la propiedad del mundo pequeño

# Preguntemonos cuantos vertices o aristas tenemos que eliminar para
# modificar la conectividad de la red (puntos de articulación)
# Cuantos nos dividen la componente gigante en dos

# vértice-conectividad
vertex_connectivity(yeast_gc)

# arista-conectividad
edge_connectivity(yeast_gc)

# puntos de articulación
yeast_cv <- articulation_points(yeast_gc)
length(yeast_cv)

length(yeast_cv)/vcount(yeast_gc)

# Se requiere la eliminación de un solo vértice o una sola arista para 
# dividir el componente gigante en componentes adicionales.
# 
# Aproximadamente el 15% de los vértices son puntos de articulación.


# Asortatividad -----------------------------------------------------------

# No se desarrolla del todo, pero en Rpubs se desarrolla después de agrupamiento

