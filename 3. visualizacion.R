# Visualización de datos relacionales



# Proyectos de visualización ----------------------------------------------

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





