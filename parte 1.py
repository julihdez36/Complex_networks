# Sesiones de redes complejas

# networkx
# Si bien igraph es una buena librería, se enfoca en redes sociales
# Hay funcionalidades de redes complejas que no encontramos en igraph

import networkx as nx

#-------------------------------------------------------
# Vamos a crear una red inicial

# Creemos los nodos

red = nx.Graph() # Graph define una red no dirigida
# Para una red dirigida usamos nx.DiGraph()


# Podemos agregar nodos particulares

red.add_node(1)

# Iteremoslo para crear varios nodos

for i in range(1,4):
    red.add_node(i)

# Podemos ver que se trata de nodos iconexos 

nx.draw(red,with_labels= True)

# Creemos entonces los enlaces para tener una triada conectada

red.add_edge(1,2)
red.add_edge(2,3)
red.add_edge(3,1)

# Para conocer los nodos puedo decir

red.nodes()

#-------------------------------------------------------
# ejercicio: Creemos un gráfo aleatorio

import numpy as np

# Creemos los nodos

g_rand = nx.Graph()

for i in range(1,13):
    g_rand.add_node(i)

g_rand.nodes()

# Recordemos que el número máximo de enlaces de una red se define como
# la combinatorio de n en 2 o: n(n-1)/2 

10*9 / 2 # 45

# consideremos 22 aristas (n-1)/2

for i in range(1,23):
    g_rand.add_edge(np.random.randint(1,20),np.random.randint(1,20))

g_rand.edges()


nx.draw(g_rand, with_labels= True)


# Grafiquemos la frecuencia de grado con lo que sabemos

import matplotlib.pyplot as plt

g_rand.degree() # Frecuencia de grado

grado_alea = dict(g_rand.degree())


plt.bar(grado_alea.keys(), grado_alea.values())

plt.hist(grado_alea.values(), bins= 'sturges') # Fecuencia de grado


#-------------------------------------------------------

# Si no nos interesa adicionar nodos individualmente podríamos

nodos = ['uno','dos','tres']
red2 = nx.Graph()
red2.add_nodes_from(nodos)

red2.add_edges_from([('uno','dos'),('dos','tres'),('uno','tres')])

nx.draw(red2, with_labels= True)


