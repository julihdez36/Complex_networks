# Visualización de datos relacionales

# Librerías ---------------------------------------------------------------

library(igraph)
library(igraphdata)
library(sand)

# Proyecto Lazega (source: sand) ------------------------------------------


# Red de relaciones de trabajo colaborativo entre miembros de una firma
# de abogados

# El data set lazega contiene tres elementos

lazega # Grafo no dirigdo
elist.lazega # Matriz de enlaces o relaciones
v.attr.lazega # DF de los atributos de los vértices

# Visualicemos los vértices

V(lazega) 

# Visualicemos las conexiones

E(lazega)

# Preguntemonos por el orden y el tamaño del grafo

sprintf('El orden del gráfo es de %d',vcount(lazega)) # 36
sprintf('El tamaño del gráfo es de %d aristas',ecount(lazega)) # 115

# También preguntémonos si se trata de un grafo conectado y sobre sus componentes

sprintf('El grafo está conectado: %s',is_connected(lazega)) # No lo está
sprintf('Número de componentes: %s', clusters(lazega)$no)


# Proyecto Blogs sida (source: sand) --------------------------------------

# Red de blogs asociados con el SIDA, los pacientes y las redes de apoyo
# Se trata de una red dirigida

# Preguntemonos por el orden y el tamaño del grafo

sprintf('El orden del gráfo es de %d',vcount(aidsblog)) # 146
sprintf('El tamaño del gráfo es de %d aristas',ecount(aidsblog)) # 187

# También preguntémonos si se trata de un grafo conectado y sobre sus componentes

sprintf('El grafo está conectado: %s',is_connected(aidsblog)) # Si está conectado
sprintf('Número de componentes: %s', clusters(aidsblog)$no)


# Blogs de política (source: sand) ----------------------------------------

# Red de blogs políticos franceses clasificados por el proyecto “Observatoire Presidentielle”
# Es una red dirigida 

data(fblog)

sprintf('El orden del gráfo es de %d',vcount(fblog)) # 192
sprintf('El tamaño del gráfo es de %d aristas',ecount(fblog)) # 1431

# También preguntémonos si se trata de un grafo conectado y sobre sus componentes

sprintf('El grafo está conectado: %s',is_connected(fblog)) # True
sprintf('Número de componentes: %s', clusters(fblog)$no)


# Caltech (repositorio) ---------------------------------------------------

url <- 'https://raw.githubusercontent.com/julihdez36/Complex_networks/main/Data/caltech.txt'

df <- read.table(url)
head(df)

caltech <- graph_from_data_frame(d = df,directed = F)
rm(df)

sprintf('El orden del gráfo es de %d',vcount(caltech)) # 769
sprintf('El tamaño del gráfo es de %d aristas',ecount(caltech)) # 16656

# También preguntémonos si se trata de un grafo conectado y sobre sus componentes

sprintf('El grafo está conectado: %s',is_connected(caltech)) # False
sprintf('Número de componentes: %s', clusters(caltech)$no) # 4 clusters


# Graph layout (diseño del grafo) -----------------------------------------

# Circle Layout
# No es muy común si el orden del gráfo es muy alto
# Esta forma no está optimizada

igraph.options(vertex.label = NA, edge.arrow.size = 0.5,vertex.color=1,
               vertex.frame.color='black')
par(mfrow = c(1,2))
plot(lazega, layout = layout_in_circle, vertex.size = 6)
title(main = 'Lazega')
plot(aidsblog, layout=layout_in_circle, vertex.size = 3)
title(main = 'Blogs de Sida')

# Diseño del algoritmo de Fruchterman y Reingold

# En este si se optimiza la red

set.seed(123)

igraph.options(vertex.label = NA, edge.arrow.size = 0.4,vertex.color=1,
               vertex.frame.color='black')
par(mfrow = c(1,2))
plot(lazega, layout = layout_with_fr, vertex.size = 6)
title(main = 'Lazega')
plot(aidsblog, layout=layout_with_fr, vertex.size = 4)
title(main = 'Blogs de Sida')

#En este layout es mucho mas sencillo ver los nodos mas pupulares
#o ver los componentes o clusters del grafo


# Algoritmo de Kamanda-kawai 

# Este algoritmo es mas popular, contando entre sus ventajas el enfoque
# para hacer evidente la transitividad de la red

set.seed(123)

igraph.options(vertex.label = NA, edge.arrow.size = 0.4,vertex.color=1,
               vertex.frame.color='black')
par(mfrow = c(1,2))
plot(lazega, layout = layout_with_kk, vertex.size = 6)
title(main = 'Lazega')
plot(aidsblog, layout=layout_with_kk, vertex.size = 4)
title(main = 'Blogs de Sida')


# Algoritmo de Davidson-Harel

# Su esquema se asemeja mas a las formas de etrella

set.seed(123)

igraph.options(vertex.label = NA, edge.arrow.size = 0.4,vertex.color=1,
               vertex.frame.color='black')
par(mfrow = c(1,2))
plot(lazega, layout = layout_with_dh, vertex.size = 6)
title(main = 'Lazega')
plot(aidsblog, layout=layout_with_dh, vertex.size = 4)
title(main = 'Blogs de Sida')

# Algoritmo DrL

# Es un algoritmo diseñado para redes masivas (large)
# Distribuye bien los vectores para que no parezca una mancha amarilla

set.seed(42)

igraph.options(vertex.label = NA, edge.arrow.size = 0.4,vertex.color=1,
               vertex.frame.color='black')
plot(caltech, layout = layout_with_drl, vertex.size = 6)
title(main = 'Caltech')


# Decoración de los gráfos ------------------------------------------------

# Podemos explotar mas la información nodal incorporando información adicional
# vatiando características como el tamaño, la forma y el color de los vértices
# y los borde

# Ejemplo Lazega

# Data
data(lazega)
# Color: ubicación de la oficina
v.colors <- c('brown1','purple','blue')[V(lazega)$Office]

# Forma del verticeL tipo de practica
v.shapes <- c('circle','square')[V(lazega)$Practice]

# Tamaño del vertice: proporcional a años de servicio
v.size <- 3.5*sqrt(V(lazega)$Years)

# Etiquetas
v.label <- V(lazega)$Seniority

# Diseño
set.seed(42)
l <- layout_with_fr(lazega)

# Grafico
plot(lazega,layout = l, vertex.color = v.colors, vertex.shape = v.shapes,
     vertex.size = v.size, vertex.label = v.label, edge.color = 'gray50',
     vertex.frame.color = 'black',vertex.label.color = 'black')
title(main = 'Lazega')

# Referencias on-line

cols <- RColorBrewer::brewer.pal(n = 9, name = "Set1")
partido_num <- as.numeric(as.factor(V(fblog)$PolParty))
V(fblog)$color <- cols[partido_num]
E(fblog)$color <- adjustcolor("black", 0.1)
# visualización
set.seed(123)
plot(fblog, layout = layout_with_fr, vertex.size = 4, vertex.label = NA, vertex.frame.color = cols[partido_num], main = "Referencias on-line")

