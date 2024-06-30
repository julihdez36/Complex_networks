# Visualización de redes estáticas y dinámicas con R

# Ognyanova, K. (2023) Visualización de redes con R. Obtenido de www. kateto.net/network-visualization.

# install.packages("igraph") 
# install.packages("network") 
# install.packages("sna")
# install.packages("ggraph")
# install.packages("visNetwork")
# install.packages("threejs")
# install.packages("networkD3")
# install.packages("ndtv")

library("network")
library("sna")
library("ggraph")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")


# Color in R plots --------------------------------------------------------

# In R functions you can use named colort, hex, or RGB values
# pch is shape, cex is size

plot(x = 1:10, y = rep(x = 5,times = 10), pch = 19, cex = 3, col = 'dark red')
points(x = 1:10, y = rep(x = 6,times = 10), pch = 19, cex = 3, col = '557799')
points(x = 1:10, y = rep(x = 4,times = 10), pch = 19, cex = 3, col = rgb(.25,.5,.3))

# We can set the opacity/transparency using the parameter alpha (range 0-1)

plot(x = 1:5, y = rep(5,5), pch = 19, cex = 12, 
     col = rgb(.25, .5, .3, alpha = .5),xlim = c(0,6))


# R comes with some predefined palette function

pal1 <- heat.colors(n = 5, alpha = 1)
pal2 <- rainbow(n = 5,alpha = .5)
plot(x = 1:10, y = 1:10, pch = 19, cex = 5, col = pal1)
points(x = 1:10, y = 10:1, pch = 19, cex = 5, col = pal2)

# We can also generate our own gradients 


palf <- colorRampPalette(c("gray80", "dark red")) 
plot(x=1:10, y=10:1, pch=19, cex=5, col=palf(10)) 


# Data format, size and preparation ---------------------------------------

# We'll work with two small example data sets. 
# Both contain data about media organizations.

# Data set 1: edge list

nodes <- read.csv('https://raw.githubusercontent.com/julihdez36/Complex_networks/main/Data/Dataset1-Media-Example-NODES.csv',header = T, as.is = T) 
links <- read.csv('https://raw.githubusercontent.com/julihdez36/Complex_networks/main/Data/Dataset1-Media-Example-EDGES.csv',header = T, as.is = T)

head(nodes) #Newspaper and audience size
head(links) # hyperlinks

# Let's convert the raw data to an igraph

library("igraph")

# d: edges of the network (take the id columns)
# vertices: take the ID column

net <- graph_from_data_frame(d = links,vertices = nodes, directed = T)
net

# At the top we can find some letters
# D or U, for a directed or undirected graph
# N when nodes have a name attribute
# W for a weighted graph
# B for a bipartitie graph

# The numbers thah follow (17 4) refer to the number of nodes and edges

# We can acces to nodes, edges and their attributes

E(net) # The edges of the net
V(net) # The vertices of the net
E(net)$type; E(net)$weight # Edge's attribute
V(net)$media; V(net)$audience.size # Vertex's attribute

# We can use that way to explore the graph

V(net)[media == 'BBC']
E(net)[type == 'mention']


# arrangements associated with graphs -------------------------------------

# Get an edge list

as_edgelist(net)

# Get a adjacency matrix

as_adjacency_matrix(net)
as_adjacency_matrix(net, attr = 'weight') # with the weights

# Or data frames describing nodes and edges (Return to the original state)
as_data_frame(net, what = 'edges')
as_data_frame(net, what = 'vertices')

# Now, let's make a firs attempt to plot it

set.seed(22)
plot(net)

# We can remove the loops in the graph

net <- simplify(net, remove.multiple = F, remove.loops = T)

#Let's reduce the arrow size and remove the labels

set.seed(22)
plot(net, edge.arrow.size = .4, vertex.label = NA)


# Plotting networks with igraph -------------------------------------------

# we can set the node and edge option in two ways

# 1. Specifying them in the plot() function

set.seed(42)
plot(net, edge.arrow.size = .6, edge.curved = .2, edge.color = '#CD96CD',
     vertex.color = 'lightblue', vertex.label = V(net)$media,
     vertex.label.color = '#838B83', vertex.frame.color = '#ffffff',
     main = "Network of newspapers' hyperlinks")

# 2. Add them to the igraph object

# Let's create a new graph following the Barabasi-Albert model

net.bg <- sample_pa(100) 

# let's adjust the parameters

V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)


### Network layouts ---------------------------------------------------------

# Network layouts are simply algotihms that return coordinates for each node in a network

plot(net.bg, layout = layout_randomly) # Random
plot(net.bg, layout = layout_in_circle) # In circle
plot(net.bg, layout = layout_on_sphere) #3D sphere layout

# Fruchterman-Reingold algorithms 
# Force-directed layouts try to get a nice-looking graph where edges 
# are similar in length and cross each other as little as possible.

plot(net.bg, layout = layout_with_fr)

# Kamada Kawai algorithms

plot(net.bg, layout= layout_with_kk)

# The MDS (multidimensional scaling) algorithm

plot(net.bg, layout=layout_with_mds)

# Let’s take a look at all available layouts in igraph:


layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

# https://kateto.net/network-visualization
