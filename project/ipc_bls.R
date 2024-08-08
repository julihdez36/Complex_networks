
# Working libraries -------------------------------------------------------

library(fpp3) # time series
library(rjson)
library(blsAPI) # Official API
library(readxl)
library(dplyr)


# Working directory -------------------------------------------------------

setwd('C:/Users/Julian/Desktop/Cursos/Fisica/Econofísica/WorkBench')
getwd()
list.files()


# Get data ----------------------------------------------------------------

# Key Api

api_key <- readline(prompt = "APi Key: ")

#'3e54893b7aa243398b4248404e48f378'
# get list of ID series

item_list <- read_excel("cpi-basic-item-aggregation.xlsx", 
                        sheet = "Sheet2")
nrow(item_list) # 211 basic items

# The API allows us to make a maximum of 50 queries at a time

# First part
payload1 <- list(
  'seriesid'= item_list$item_code[1:49],
  'startyear'=2012,
  'endyear'=2023,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df1 <- blsAPI(payload = payload1, api_version =  2,return_data_frame =  TRUE)
length(table(df1$seriesID)) # 49 items

# second part
payload2 <- list(
  'seriesid'= item_list$item_code[50:99],
  'startyear'=2012,
  'endyear'=2023,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df2 <- blsAPI(payload = payload2, api_version =  2,return_data_frame =  TRUE)
length(table(df2$seriesID)) # 45 items 

# third part
payload3 <- list(
  'seriesid'= item_list$item_code[100:149],
  'startyear'=2012,
  'endyear'=2023,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df3 <- blsAPI(payload = payload3, api_version =  2,return_data_frame =  TRUE)
length(table(df3$seriesID)) # 41 items 

# fourth part
payload4 <- list(
  'seriesid'= item_list$item_code[150:199],
  'startyear'=2012,
  'endyear'=2023,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df4 <- blsAPI(payload = payload4, api_version =  2,return_data_frame =  TRUE)
length(table(df4$seriesID)) # 38 items

# fifth part
payload5 <- list(
  'seriesid'= item_list$item_code[200:211],
  'startyear'=2012,
  'endyear'=2023,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df5 <- blsAPI(payload = payload5, api_version =  2,return_data_frame =  TRUE)
length(table(df5$seriesID)) # 9 items

df <- rbind(df1,df2,df3,df4,df5)

# Removing objects 
rm(df1,df2,df3,df4,df5)
rm(payload1,payload2,payload3,payload4,payload5)

dim(df) # (26089,5)

# number of items
length(levels(as.factor(df$seriesID))) #182 items

# Data cleansing ----------------------------------------------------------

# Checking data consistency

sum(!levels(as.factor(df$seriesID)) %in% item_list$item_code) # 0

table(df$year) # different records by years

# Missing values

sum(is.na(df)) # 0 NA

freq_table <- table(df$seriesID)
filtered_values <- freq_table[freq_table < 144]
sum(-filtered_values +144) #119 missings

length(filtered_values) # 9 items have lees records

items_toremove <- names(freq_table[freq_table < 138]) #removing 5 items
df <- df[!(df$seriesID %in% items_toremove),] #26089 to 25484

# Change type

df$date <-parse_date_time(paste(df$year, df$periodName, "01"), 
                           orders = "ymd")

df$date <- as.Date(df$date)
df$seriesID <- as.factor(df$seriesID)
df$value <- as.numeric(df$value)

length(levels(df$seriesID)) # 177 items

# Creating a tsibble

ts_df <- as_tsibble(df, index = date, key = seriesID) # 182 items
levels(ts_df$seriesID) 
length(levels(ts_df$seriesID)) # 177 items


# Completing tstibble

ts_df_complete <- ts_df %>%
  complete(seriesID, date = seq(min(date), max(date), by = "month"))

sum(is.na(ts_df_complete)) # 16 NA

# Linear interpolation is an imputation

library(zoo)

ts_df_complete$value <- na.approx(ts_df_complete$value, na.rm = FALSE)

ts_df_complete[!complete.cases(ts_df_complete),]

ts_df_complete <- ts_df_complete %>% select(seriesID,date,value)

length(levels(ts_df_complete$seriesID)) # 177 items

str(ts_df_complete)

# data loading ------------------------------------------------------------

write.csv(df,file = 'df_base.csv')
write.csv(ts_df_complete,file = 'ts_dataframe.csv') # complete

list.files()

# to upload

#ts_df <- read.csv("ts_dataframe.csv")
#df <- read.csv("df_base.csv")

# stationarity condition --------------------------------------------------

# Lineal difference

# ts_df_diff_linear <- ts_df_complete %>%
#   group_by(seriesID) %>%
#   mutate(diff_linear = (value - lag(value)) / lag(value)) %>%
#   filter(!is.na(diff_linear)) %>%  # Eliminar filas con NA
#   ungroup()

# logarithmic difference

ts_df_diff_log <- ts_df_complete %>%
  group_by(seriesID) %>%
  mutate(diff_log_value = log(value) - lag(log(value))) %>%
  filter(!is.na(diff_log_value)) %>%
  ungroup()

# optional displays

# windows()
# ggplot(ts_df_diff_linear, aes(x = date, y = diff_linear, color = seriesID))+
#   geom_line()+labs(title = 'difference PCI')+
#   theme_bw()+theme(legend.position = "none")

windows()
ggplot(ts_df_diff_log, aes(x = date, y = diff_log_value, color = seriesID))+
  geom_line()+labs(title = 'logarithmic difference PCI')+
  theme_bw()+theme(legend.position = "none")

# Correlation graph -------------------------------------------------------

library(igraph)
library(corrr)

# Basic correlation

# Change format

ts_df_wide <- ts_df_diff_log %>%
  select(date, seriesID, diff_log_value) %>%
  pivot_wider(names_from = seriesID, values_from = diff_log_value)

# Correlation matrix

ts_df_wide_corr <- ts_df_wide %>%
  select(-date) %>%
  cor()

windows()
heatmap(ts_df_wide_corr)

write.csv(ts_df_wide_corr,file = 'Correlation_matrix.csv')

# graph with threshold ----------------------------------------------------

# Create graph an adjacency matrix an set threshold

ts_df_wide_corr <- read.csv('Correlation_matrix.csv', row.names = 1)

threshold <- 0.36  # set Threshold
adj_matrix <- ifelse(abs(ts_df_wide_corr) >= threshold, 1, 0)

g0 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

windows()
plot(g0,vertex.label = NA)

# Results

ecount(g0) # 388
vcount(g0) # 177

heatmap(adj_matrix)

# Threshold- Minimum Dominating Set (T-MDS) -------------------------------


# Crear el grafo a partir de la matriz de adyacencia
g1 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Ajustar la matriz de adyacencia con el umbral
threshold <- 0.36  # Umbral
adj_matrix_thresholded <- ifelse(abs(adj_matrix) >= threshold, 1, 0)

# Crear el grafo con la matriz ajustada
g1_thresholded <- graph_from_adjacency_matrix(adj_matrix_thresholded, mode = "undirected", diag = FALSE)

# Implementar el T-MDS
find_threshold_minimum_dominating_set <- function(g, threshold) {
  nodes <- V(g)
  dominating_set <- c()
  
  while (length(dominating_set) < length(nodes)) {
    uncovered_nodes <- setdiff(nodes, dominating_set)
    max_coverage_node <- NULL
    max_coverage_count <- -1
    
    for (node in uncovered_nodes) {
      neighbors <- neighbors(g, node)
      # Considerar vecinos dentro del umbral
      coverage_count <- length(setdiff(neighbors, dominating_set))
      if (coverage_count > max_coverage_count) {
        max_coverage_count <- coverage_count
        max_coverage_node <- node
      }
    }
    
    if (!is.null(max_coverage_node)) {
      dominating_set <- union(dominating_set, max_coverage_node)
    }
  }
  
  return(dominating_set)
}

# Aplicar la función para encontrar el conjunto dominante mínimo con umbral
dominating_set_threshold <- find_threshold_minimum_dominating_set(g1_thresholded, threshold)

# Convertir a nombres de nodos para una visualización más intuitiva
dominating_set_names_threshold <- V(g1)$name[dominating_set_threshold]

# Ver el conjunto dominante mínimo con umbral
print(dominating_set_names_threshold)

# Colorear los nodos del conjunto dominante mínimo con umbral
V(g1)$color <- ifelse(V(g1) %in% dominating_set_threshold, "white", "red")

# Dibujar el grafo
windows()
plot(g1,vertex.label = NA)

# Results
vcount(g1) #177
ecount(g1) #338
heatmap(adj_matrix)

# Fisher’s transformation and Benjamini-Hochberg adjustment ---------------


# Fisher’s transformation and Benjamini-Hochberg adjustment

z <- 0.5 * log((1 + ts_df_wide_corr) / (1 - ts_df_wide_corr))
z.vec <- z[upper.tri(z)]
n <- dim(ts_df_wide)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0,
              sqrt(1 / (n-3)), lower.tail=FALSE)

length(corr.pvals) #Number of possible couples (combinatory): 15576

# Benjamini-Hochberg adjustment

corr.pvals.adj <- p.adjust(corr.pvals, "BH")

table(corr.pvals.adj <0.05) # 1433 edges


# Comparing to a standard 0.05 significance level 

length(corr.pvals.adj[corr.pvals.adj < 0.05]) # 1433 edges

library(fdrtool)

# Estimate (Local) False Discovery Rates For Diverse Test Statistics

mycorr.vec <- ts_df_wide_corr[upper.tri(ts_df_wide_corr)]
windows()
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# Paso 2: Crear la matriz de adyacencia basada en el umbral de significancia
threshold <- 0.05
adj_matrix <- matrix(0, nrow = ncol(mycorr), ncol = ncol(mycorr))
# Convertir los p-valores ajustados en una matriz de adyacencia
# Usar un umbral para definir la presencia de un enlace
adj_matrix[upper.tri(adj_matrix)] <- (corr.pvals.adj < threshold)
adj_matrix <- t(adj_matrix) + adj_matrix  # Hacer la matriz simétrica
diag(adj_matrix) <- 0  # Eliminar autolinks

# Paso 3: Crear el grafo a partir de la matriz de adyacencia
g2 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Paso 4: Visualizar el grafo
windows()
plot(g2,
     vertex.size = 10,
     vertex.color = "lightblue",
     edge.color = "grey",
     main = "Grafo basado en correlaciones significativas",
     vertex.label = NA)  # No mostrar etiquetas en los nodos

vcount(g) # 177
ecount(g) # 1771

heatmap(adj_matrix) 

# Partial correlation network ---------------------------------------------

#Tamaño de la muestra
n <- nrow(ts_df_wide)
n # 143

#n de vertices
N <- ncol(ts_df_wide[,2:178])
N # 177


#correlaciones agregadas y obteniendo el max

# Supongamos que N es el número de filas/columnas de tu matriz
N <- nrow(ts_df_wide) 
pcorr.pvals <- matrix(NA, N, N)  # Matriz para almacenar p-valores

for (i in 1:N) {
  for (j in 1:N) {
    if (i != j) {
      # Extraer filas y convertir a vectores
      rowi <- as.numeric(ts_df_wide[i, -c(i, j)])
      rowj <- as.numeric(ts_df_wide[j, -c(i, j)])
      
      # Calcular la correlación parcial
      tmp <- (ts_df_wide[i, j] - sum(rowi * rowj)) / sqrt((1 - sum(rowi^2)) * (1 - sum(rowj^2)))
      
      # Calcular el estadístico de prueba
      tmp.zvals <- 0.5 * log((1 + tmp) / (1 - tmp))
      
      # Calcular los p-valores
      tmp.pvals <- 2 * pnorm(abs(tmp.zvals), mean = 0, sd = 1 / sqrt(n - m - 3), lower.tail = FALSE)
      
      # Guardar el valor máximo de p en la matriz de p-valores
      pcorr.pvals[i, j] <- max(tmp.pvals)
    }
  }
}
# Error!!!!

# valores p ajustados (pruebas multiples)

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# relaciones significativas

pcorr.edges <- (pcorr.pvals.adj < 0.05)
table(pcorr.edges)

fdr <-fdrtool(pcorr.pvals.vec,statistic = "pvalue",plot = FALSE,verbose = F)
pcorr.edges.2 <- (fdr$qval < 0.05)
table(pcorr.edges.2) #### REVISAR 

#grafo inducido
pcorr.A <- matrix(0,N,N)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.pvals.adj < 0.05)
pcorr.A <- pcorr.A + t(pcorr.A)

#comparacion con grafo observado
table(regDB.adj[lower.tri(regDB.adj)],pcorr.A[lower.tri(pcorr.A)])


# Modelo gaussiano --------------------------------------------------------


