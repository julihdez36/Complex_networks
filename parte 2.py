# Parte 2

import networkx as nx

# Trabajemos con el famoso set de la red del grupo de karate

net = nx.karate_club_graph()


print('Número de nodos:', net.number_of_nodes()) # 34
print('Número de aristas:', net.number_of_edges()) # 78

# ¿Qué tan densa es nuestra red?

nx.density(net)  #  d = \frac{2m}{n(n-1)} \in [0,1]

grado = dict(net.degree())
grado

# Podríamos crear una tabla de frecuencia

valores_g = list(grado.values())
min(valores_g), max(valores_g)


for i in range(1,17):
    print(i,valores_g.count(i))

# Y graficarlo en un diagrama de barras
import matplotlib.pyplot as plt

plt.bar(grado.keys(), grado.values())

plt.hist(valores_g, bins=10) # Histograma de grados 

# Hecho esto, podríamos ver el grafo

nx.draw(net, with_labels = True)

# Esto es una red muy vulnerable, al tener nodos centrales muy conectados que son centrales
# en la topología de la red
