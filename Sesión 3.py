import networkx as nx
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def plot_degree_dist(G):
    degrees = [G.degree(i) for i in G.nodes()] # Calcula grado por nodos en una lista
    plt.hist(degrees)
    plt.show()
    

def plot_cum_degree_dist(G):
    degrees = [G.degree(i) for i in G.nodes()]
    data = np.array(degrees)
    distribucion_grado = np.bincount(data)
    s = sum(distribucion_grado)
    cdf = distribucion_grado.cumsum(0)/s
    # ccdf = np.flipud(cdf)
    ccdf = 1 - cdf
    plt.plot(range(len(ccdf)),ccdf, 'bo')
    plt.xscale('log')
    plt.yscale('log')
    plt.ylim([0,1])
    plt.ylabel('CDF')
    plt.xlabel('Degree')
    #plt.hist(degrees)
    plt.show()
    
net = nx.karate_club_graph()
print('Número de nodos:', net.number_of_nodes())
print('Número de atistas:', net.number_of_edges())

nx.density(net) # Densidad
grado = dict(net.degree())

# Vamos a evaluar la función acumulada inversa, por la riqueza interpretativa
plot_degree_dist(net)

plot_cum_degree_dist(net)

# Exponente de decaimiento: a medida que aumenta el grado puedo ver que nuevas conexiones hay 

# Acá es importante ver lo de las leyes de potencia y su importancia en esta interpretación