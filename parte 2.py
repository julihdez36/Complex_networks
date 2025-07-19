# Parte 2

import networkx as nx

# Trabajemos con el famoso set de la red del grupo de karate

net = nx.karate_club_graph()


print('Número de nodos:', net.number_of_nodes()) # 34
print('Número de aristas:', net.number_of_edges()) # 78



print('Número de nodos: ',))
nx.draw(net, with_labels= True)


import matplotlib.pyplot as plt

plt.bar(frec_dict.keys(), frec_dict.values())


net.degree()

plt.hist(x= net.degree(), bins= 'sturges')