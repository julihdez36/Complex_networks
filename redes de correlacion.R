
# Redes de correlación ----------------------------------------------------

library(igraph)
library(sand)

g.RegDB <- graph_from_adjacency_matrix(adjmatrix=regDB.adj, mode = "undirected")
windows()
plot(g.RegDB, vertex.label=NA, vertex.size =7.5, vertex.frame.color = "black")
title(main = "Red E.Coli")

vcount(g.RegDB)
ecount(g.RegDB)

data(Ecoli.data)

r <- cor(Ecoli.expr)
z <- 0.5 * log((1 + r) / (1 - r))
z.vec <- z[upper.tri(z)]
n <- dim(Ecoli.expr)[1]
corr.pvals <-2 * pnorm(abs(z.vec), 0, sqrt(1 / (n - 3)), lower.tail = FALSE)

length(corr.pvals)


# Para las pruebas múltiples, si las decisiones sobres las hipótesis individuales 
# se basan en valores p marginales no ajustados, suele haber una gran probabilidad
# de que se rechacen algunas de las hipótesis nulas verdaderas, es decir, hay un 
# aumento sustancial del False Discovery Rate (FDR)

# La función p.adjust proporciona herramientas para ajustar los valores p
# para las pruebas múltiples. En este caso, se aplica el ajuste de
# Benjamini-Hochberg para controlar el FDR

corr.pvals.adj <- p.adjust(corr.pvals, "BH")
table(corr.pvals.adj <0.05) #luego del ajuste

# Normalidad del conjunto de datos

car::qqPlot(z.vec)

shapiro.test(x = sample(x = z.vec, size = 5000, replace = F))

# p-value = 2.575e-06, se rechaza H0 de normalidad, p < 0.05

# Tanto el QQ-Plot, como la prueba de Shapiro- Wilk, rechazan la normalidad de
# los scores y podría ser el porqué de los resultados con los ajustes.


# Metodología no paramétrica

library(fdrtool) # Control de tasas de falsos descubrimientos

#calculo de los valores p, de nuevo
r.vec <- r[upper.tri(r)]
fdr <-fdrtool(x = r.vec,statistic = "correlation",plot = F,verbose = F)
length(fdr$qval)

?fdrtool(x = r.vec,statistic = "correlation",plot = F,verbose = F)

head(fdr$qval) # arrojan no significativos

table(fdr$qval <0.05) # No hay ninguna correlación


# Redes de correlación parcial --------------------------------------------

#Tamaño de la muestra
n <- nrow(Ecoli.expr)
n

#n de vertices
N <- ncol(Ecoli.expr)
N


#correlaciones agregadas y obteniendo el max
m <- 1
pcorr.pvals <- matrix(0, N, N)
for (i in 1:N) {
  for (j in 1:N) {
    rowi <- r[i,-c(i, j)]
    rowj <- r[j,-c(i, j)]
    tmp <- (r[i, j] - rowi * rowj) / sqrt((1 - rowi^2) * (1 - rowj^2)) #correlaciones parciales
    tmp.zvals <-(0.5) * log((1 + tmp) / (1 - tmp)) #estadístico de prueba
    tmp.pvals <-2 * pnorm(abs(tmp.zvals),mean = 0,sd = 1 / sqrt(n - m - 3),lower.tail = FALSE)
    pcorr.pvals[i, j] <-max(tmp.pvals)
  }
}

# valores p ajustados (pruebas multiples)

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# relaciones significativas

pcorr.edges <- (pcorr.pvals.adj < 0.05)
table(pcorr.edges)


#correlaciones agregadas y obteniendo el max
m <- 1
pcorr.pvals <- matrix(0, N, N)
for (i in 1:N) {
  for (j in 1:N) {
    rowi <- r[i,-c(i, j)]
    rowj <- r[j,-c(i, j)]
    tmp <- (r[i, j] - rowi * rowj) / sqrt((1 - rowi^2) * (1 - rowj^2)) #correlaciones parciales
    tmp.zvals <-(0.5) * log((1 + tmp) / (1 - tmp)) #estadístico de prueba
    tmp.pvals <-2 * pnorm(abs(tmp.zvals),mean = 0,sd = 1 / sqrt(n - m - 3),lower.tail = FALSE)
    pcorr.pvals[i, j] <-max(tmp.pvals)
  }
}

# valores p ajustados (pruebas multiples)

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# relaciones significativas

pcorr.edges <- (pcorr.pvals.adj < 0.05)
table(pcorr.edges)


