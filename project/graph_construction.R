# Load r ----------------------------------------------------------------

# write.csv(r,file = 'Correlation_matrix.csv')
# write.csv(ts_df_wide,file = 'ts_df_wide.csv', row.names = FALSE)

list.files()
r <- read.csv('Correlation_matrix.csv',row.names = 1)
ts_df_wide <- read.csv('ts_df_wide.csv', row.names = 1)

dim(r) #(177,177)
dim(ts_df_wide) # (143,178)

# graph with threshold ----------------------------------------------------

# Create graph an adjacency matrix an set threshold

# ts_df_wide_corr <- read.csv('Correlation_matrix.csv', row.names = 1)

threshold1 <- 0.36  # set Threshold
threshold2 <- 0.5 
adj_matrix01 <- ifelse(abs(r) >= threshold1, 1, 0)
adj_matrix02 <- ifelse(abs(r) >= threshold2, 1, 0)

g01 <- graph_from_adjacency_matrix(adj_matrix01, mode = "undirected", diag = FALSE)
g02 <- graph_from_adjacency_matrix(adj_matrix02, mode = "undirected", diag = FALSE)

windows()

group_df <- ts_df %>%
  group_by(seriesID) %>%
  summarize(Name_group = unique(Name_group), .groups = 'drop')
dim(group_df)

groups_l <- group_df$Name_group 

V(g01)$group <- groups_l
V(g02)$group <- groups_l

library(viridisLite)
library(viridis)
unique_groups <- unique(V(g01)$group)
colors <- viridis(length(unique_groups))  # Usa rainbow o define tus colores manualmente

# Crear un vector de colores para cada vértice
vertex_colors <- colors[as.numeric(factor(V(g01)$group))]

# Algoritmo de disposición de gráficos Fruchterman-Reingold (default)
# Fruchterman-Reingold
windows()
set.seed(123)
# Configuración para reducir la distancia entre los gráficos
par(mfrow = c(1, 2), oma = c(0, 0, 0, 7), mar = c(5, 4, 4, 1))  # Ajusta los márgenes internos

# Gráfico 1
plot(g01, vertex.label = NA, vertex.size = 4, vertex.color = vertex_colors,
     main = 'Threshold = 0.36')

# Gráfico 2
par(mar = c(5, 1, 4, 2))  # Reduce el margen izquierdo del segundo gráfico
plot(g02, vertex.label = NA, vertex.size = 4, vertex.color = vertex_colors,
     main = 'Threshold = 0.5')

# Dibuja la leyenda en el margen derecho
par(fig = c(0, 1, 0, 1), new = TRUE, oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))

legend("topright",                       # Posición en el margen derecho
       legend = unique_groups,        # Etiquetas de la leyenda
       fill = colors,                 # Colores de la leyenda
       title = "Groups",               # Título de la leyenda
       xpd = TRUE,                    # Permite que la leyenda se dibuje fuera del área de gráficos
       inset = c(-.05, 0),           # Ajusta la posición de la leyenda hacia adentro del margen
       bty = "n",                     # Elimina el borde de la leyenda para evitar recortes
       cex = 1)                     # Ajusta el tamaño de la leyenda


# Results

ecount(g01) # 388
vcount(g01) # 177

heatmap(adj_matrix)




# Fisher’s transformation and Benjamini-Hochberg adjustment ---------------

# Estadistico de prueba: transformación z de Fisher 
# Estabiliza la varianza de todos los puntajes de la correlación

z <- 0.5 * log((1 + r) / (1 - r)) #estadistico de prueba
dim(z) # (177,177)
z.vec <- z[upper.tri(z)] # Diagonal superior
length(z.vec ) # Debo hacer 15576 pruebas de hipótesis


# H0: si (xi,xj)~N y n es lo suficientemente grande (condiciones)
n <- dim(ts_df_wide)[1] # tamño de la muestra (observaciones)
n

# Valores p
corr.pvals <- 2 * pnorm(abs(z.vec), 0,
                        sqrt(1 / (n-3)), lower.tail=FALSE)

length(corr.pvals) #Number of possible couples (combinatory): 15576

# Al ser tanta pruebas múltiples se incrementa el 
# False Descovery Rate FDR

# Benjamini-Hochberg adjustment

corr.pvals.adj <- p.adjust(corr.pvals, "BH")

# Comparing to a standard 0.05 significance level 

table(corr.pvals <0.05) # 3271 valores significativos (no. enlaces)
table(corr.pvals.adj <0.05) # 1433 valores significativos (no. enlaces)

# Son datos muy grandes, lo que es sospechoso.

# Normality test

car::qqPlot(z.vec)
shapiro.test(x = sample(x = z.vec, size = 5000, replace = F)) 

# se rechaza normalidad, eso puede explicar la gran cantidad de enlaces

# Para ello podemos usar una metodología no paramétrica
# Se estudia la distirbución nula cuando no estamos seguros de los z
# Corrección no paramétrica

library(fdrtool)
?fdrtool()
# Estimate (Local) False Discovery Rates For Diverse Test Statistics

r.vec  <- r[upper.tri(r)]

windows()
fdr <- fdrtool(r.vec , statistic="correlation")

length(fdr$qval) #15576

head(fdr$qval)

table(fdr$qval <0.05) # 396 significativos (edges), lo que tiene mucho sentido

# Crear una matriz de adyacencia inicial con 0s
adj_matrix_fdr <- matrix(0, nrow = ncol(ts_df_wide), ncol = ncol(ts_df_wide))

# Llenar la matriz de adyacencia con las correlaciones significativas basadas en el ajuste FDR
for (i in 1:length(fdr$qval)) {
  if (fdr$qval[i] < 0.05) {
    row_col <- which(upper.tri(r), arr.ind = TRUE)[i, ]
    adj_matrix_fdr[row_col[1], row_col[2]] <- r[row_col[1], row_col[2]]
    adj_matrix_fdr[row_col[2], row_col[1]] <- r[row_col[1], row_col[2]]  # Asegurar simetría
  }
}

dim(r) # (177,177)
dim(adj_matrix_fdr) # (177,177)

colnames(adj_matrix_fdr) <- colnames(r)
rownames(adj_matrix_fdr) <- rownames(r)

# Crear el grafo ponderado usando igraph, en valores absolutos
g_fdr <- graph_from_adjacency_matrix(abs(adj_matrix_fdr), mode = "undirected", weighted = TRUE, diag = FALSE)

colnames(ts_df)
dim(ts_df)

group_df <- ts_df %>%
  group_by(seriesID) %>%
  summarize(Name_group = unique(Name_group), .groups = 'drop')
dim(group_df)

groups_l <- group_df$Name_group 

V(g_fdr)$group <- groups_l

# Graficarlo
windows()

library(viridis)
unique_groups <- unique(V(g_fdr)$group)
colors <- viridis(length(unique_groups))  # Usa rainbow o define tus colores manualmente

# Crear un vector de colores para cada vértice
vertex_colors <- colors[as.numeric(factor(V(g_fdr)$group))]

# Algoritmo de disposición de gráficos Fruchterman-Reingold (default)
# Fruchterman-Reingold
set.seed(123)

plot(g_fdr, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     main = "Non-parametrically fitted correlation network")  # Título del gráfico

legend("topright",                   # Posición de la leyenda
       legend = unique_groups,       # Etiquetas de la leyenda
       fill = colors,                # Colores de la leyenda
       title = "Group")              # Título de la leyenda

ecount(g_fdr) # 396
vcount(g_fdr) # 177


# Comparación de grafos ---------------------------------------------------

# Convertir las matrices ponderadas en matrices binarias
binary_adj_matrix_g01 <- ifelse(adj_matrix01 > 0, 1, 0)
binary_adj_matrix_g_fdr <- ifelse(adj_matrix_fdr > 0, 1, 0)

# Extraer los pares de aristas para cada grafo
edges_g01 <- binary_adj_matrix_g01[lower.tri(binary_adj_matrix_g01)]
edges_g_fdr <- binary_adj_matrix_g_fdr[lower.tri(binary_adj_matrix_g_fdr)]

# Construir la matriz de comparación
comparison_table <- table(edges_g01, edges_g_fdr)

# Mostrar la matriz de comparación
comparison_table

ecount(g01)
ecount(g_fdr)


# Modelo gaussiano --------------------------------------------------------

ma_df_wide <- as.matrix(ts_df_wide)

library(huge)

set.seed(123)
# Inferencia de redes con el método de regularización Lasso
# Lasso (Least Absolute Shrinkage and Selection Operator)
huge.out_gaus <- huge(ma_df_wide, method = "glasso")

windows()
plot(huge.out_gaus)

# Stability Approach to Regularization Selection
huge.opt_gaus <-  huge.select(huge.out_gaus, criterion= "stars",stars.thresh = .05)
huge.opt_gaus$opt.lambda # 0.335174

g_gauss <-  graph_from_adjacency_matrix(huge.opt_gaus$refit, "undirected")

unique_groups <- unique(V(g_fdr)$group)
colors <- viridis(length(unique_groups))  # Usa rainbow o define tus colores manualmente

# Crear un vector de colores para cada vértice
vertex_colors <- colors[as.numeric(factor(V(g_fdr)$group))]

# Algoritmo de disposición de gráficos Fruchterman-Reingold (default)
# Fruchterman-Reingold
set.seed(123)

windows()
plot(g_gauss, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     main = "Gaussian Graphical Model Networks")  # Título del gráfico

legend("topright",                   # Posición de la leyenda
       legend = unique_groups,       # Etiquetas de la leyenda
       fill = colors,                # Colores de la leyenda
       title = "Group")              # Título de la leyenda

ecount(g_gauss) # 371
vcount(g_fdr)


# Copulas gaussianas ------------------------------------------------------

# Trasnformación a una cópula gaussiana

# Supongamos que tus datos están en ts_df_wide (143, 177)
# Aplicar la transformación de rangos a cada columna
ts_df_rank <- apply(ts_df_wide, 2, rank) / (nrow(ts_df_wide) + 1)

# Transformar los datos al espacio gaussiano utilizando la función inversa de la normal estándar
ts_df_gaussian <- qnorm(ts_df_rank)


# Inferencia del Grafo usando un Modelo Gráfico Gaussiano:

# Convertir los datos transformados a una matriz
ma_gaussian <- as.matrix(ts_df_gaussian)

# Inferir la red utilizando Lasso (glasso)
set.seed(123)
huge.out <- huge(ma_gaussian, method = "glasso")

# Seleccionar el modelo óptimo usando el criterio de estabilidad
huge.opt <- huge.select(huge.out, criterion= "stars", stars.thresh = .05)

# Crear el grafo a partir de la matriz de adyacencia resultante
g_copula_gaus <- graph_from_adjacency_matrix(huge.opt$refit, "undirected")


ecount(g_copula_gaus) # 384

# Visualización

set.seed(123)

windows()

plot(g_copula_gaus, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     #edge.color = '#474747',layout = layout_fr,
     main = "Semiparametric Gaussian copula graphical models")  # Título del gráfico

legend("topright",                   # Posición de la leyenda
       legend = unique_groups,       # Etiquetas de la leyenda
       fill = colors,                # Colores de la leyenda
       title = "Group")              # Título de la leyenda

intersection(g_fdr,g_copula_gaus)


# Comparación de resultados -----------------------------------------------

adj_fdr <- as_adjacency_matrix(g_fdr, sparse = FALSE)
adj_copula_gaus <- as_adjacency_matrix(g_copula_gaus, sparse = FALSE)

# Convertir las matrices ponderadas en matrices binarias
binary_fdr <- ifelse(adj_fdr > 0, 1, 0)
binary_copula <- ifelse(adj_copula_gaus > 0, 1, 0)

# Extraer los pares de aristas para cada grafo
edges_fdr <- binary_fdr[lower.tri(binary_fdr)]
edges_copula <- binary_copula[lower.tri(binary_copula)]

# Construir la matriz de comparación
comparison_table <- table(edges_fdr, edges_copula)

# Mostrar la matriz de comparación
sum(comparison_table)

rownames(comparison_table) <- paste("edges_fdr:", rownames(comparison_table))
colnames(comparison_table) <- paste("edges_copula:", colnames(comparison_table))

comparison_table

ecount(g_gauss) # 371
ecount(g_fdr) # 396
ecount(g_copula_gaus) # 384

library(xtable)

# Suponiendo que ya tienes tu tabla comparison_table
latex_table <- xtable(comparison_table)

# Imprimir la tabla en formato LaTeX
print(latex_table, type = "latex")



# Graficos finales --------------------------------------------------------

plot(g_fdr, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     main = "Non-parametrically fitted correlation network")  # Título del gráfico

plot(g_copula_gaus, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     #edge.color = '#474747',layout = layout_fr,
     main = "Semiparametric Gaussian copula graphical models")  # Título del gráfico

legend("topright",                   # Posición de la leyenda
       legend = unique_groups,       # Etiquetas de la leyenda
       fill = colors,                # Colores de la leyenda
       title = "Group")              # Título de la leyenda


windows()
set.seed(123)
# Configuración para reducir la distancia entre los gráficos
par(mfrow = c(1, 2), oma = c(0, 0, 0, 7), mar = c(5, 4, 4, 1))  # Ajusta los márgenes internos

# Gráfico 1
plot(g_fdr, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     main = "Non-parametrically fitted correlation network")  # Título del gráfico


# Gráfico 2
par(mar = c(5, 1, 4, 2))  # Reduce el margen izquierdo del segundo gráfico
plot(g_copula_gaus, 
     vertex.color = vertex_colors,  # Asignar colores a los nodos
     vertex.size = 5,               # Tamaño de los nodos
     vertex.label = NA,             # Sin etiquetas en los nodos
     edge.width = E(g_fdr)$weight * 4,
     #edge.color = '#474747',layout = layout_fr,
     main = "Semiparametric Gaussian copula graphical models")  # Título del gráfico

# Dibuja la leyenda en el margen derecho
par(fig = c(0, 1, 0, 1), new = TRUE, oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))

legend("topright",                       # Posición en el margen derecho
       legend = unique_groups,        # Etiquetas de la leyenda
       fill = colors,                 # Colores de la leyenda
       title = "Groups",               # Título de la leyenda
       xpd = TRUE,                    # Permite que la leyenda se dibuje fuera del área de gráficos
       inset = c(-.05, 0),           # Ajusta la posición de la leyenda hacia adentro del margen
       bty = "n",                     # Elimina el borde de la leyenda para evitar recortes
       cex = 1)                     # Ajusta el tamaño de la leyenda

