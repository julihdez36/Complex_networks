
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

api_key <- '3e54893b7aa243398b4248404e48f378'

# get list of ID series

item_list <- read_excel("item_list.xlsx", 
                        sheet = "Sheet2")

nrow(item_list) # 181 items
sample_n(item_list,5)

item_list$item_code # item code's vector

# The API allows us to make a maximum of 50 queries at a time

payload1 <- list(
  'seriesid'= item_list$item_code[1:49],
  'startyear'=2012,
  'endyear'=2024,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df1 <- blsAPI(payload = payload1, api_version =  2,return_data_frame =  TRUE)

length(table(df1$seriesID)) # 49 items
nrow(df1) #7350 records
head(df1)

# second part

payload2 <- list(
  'seriesid'= item_list$item_code[50:98],
  'startyear'=2012,
  'endyear'=2024,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df2 <- blsAPI(payload = payload2, api_version =  2,return_data_frame =  TRUE)
length(table(df2$seriesID)) # 49 items
nrow(df2) # 7349 records

# third part

payload3 <- list(
  'seriesid'= item_list$item_code[99:147],
  'startyear'=2012,
  'endyear'=2024,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df3 <- blsAPI(payload = payload3, api_version =  2,return_data_frame =  TRUE)

length(table(df3$seriesID)) # 49 items
nrow(df3) # 7244 records

# fourth part

nrow(item_list)

payload4 <- list(
  'seriesid'= item_list$item_code[148:181],
  'startyear'=2012,
  'endyear'=2024,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df4 <- blsAPI(payload = payload4, api_version =  2,return_data_frame =  TRUE)

length(table(df4$seriesID)) # 34 items
nrow(df4) # 5062 records

df <- rbind(df1,df2,df3,df4)

# Removing objects 
rm(df1,df2,df3,df4)
rm(payload1,payload2,payload3,payload4)

dim(df)

# number of items
length(levels(as.factor(df$seriesID))) #181 items

# Checking data consistency
which(!item_list$item_code %in%  levels(as.factor(df$seriesID))) # 0

table(df$year) # different records by years


# Data cleansing ----------------------------------------------------------

# Missing values

sum(is.na(df)) # 0 NA

freq_table <- table(df$seriesID)
filtered_values <- freq_table[freq_table < 150]
filtered_values

length(filtered_values) # 12 items have lees records


# What kind of items are they?

items_to_remove <- names(filtered_values)

item_list[item_list$item_code %in% items_to_remove, 4]

# We chose to remove those items

df <- df[!df$seriesID %in% items_to_remove,] # from 27005 to 25350

# Change type

df$date <-parse_date_time(paste(df$year, df$periodName, "01"), 
                           orders = "ymd")

df$date <- as.Date(df$date)
df$seriesID <- as.factor(df$seriesID)
df$value <- as.numeric(df$value)
head(df)

# Creating a tsibble

ts_df <- as_tsibble(df, index = date, key = seriesID) # 169 items
levels(ts_df$seriesID) # 181 series
str(ts_df)

# data loading ------------------------------------------------------------

write.csv(df,file = 'df_base.csv')
write.csv(ts_df,file = 'ts_dataframe.csv')

# list.files()


# stationarity condition --------------------------------------------------

# Change format

# Pivotar el tsibble
pivoted_df <- ts_df %>%
  pivot_wider(names_from = seriesID, values_from = value)

# Seleccionar solo las columnas que contienen las variables de seriesID
series_data <- pivoted_df %>%
  select(-date,-year, -period, -periodName)

rownames(series_data) <- pivoted_df$date


# Calcular la diferencia logarítmica para cada serie
log_diff_data <- long_data %>%
  group_by(seriesID) %>%
  arrange(date) %>% # Asegúrate de que los datos estén ordenados por fecha
  mutate(
    log_value = log(value),
    log_diff = log_value - lag(log_value)
  ) %>%
  select(-log_value)  # Opcional: elimina la columna log_value si no es necesaria

# Convertir de nuevo a formato ancho
ts_data_diff <- log_diff_data %>%
  pivot_wider(
    names_from = seriesID,
    values_from = log_diff
  )

# Mostrar el resultado
print(ts_data_diff)



# logarithmic difference

series_data
# Correlation graph -------------------------------------------------------
library(igraph)
library(corrr)

# Correlation matrix

# Calcular la matriz de correlación
correlation_matrix <- series_data %>%
  correlate(method = "pearson",diagonal = 1)

# Set threshold

# threshold <- 0.5 #reino unido
threshold <- 0.82 # uruguay

# Create the adjacency matrix by applying the threshold

correlation_df <- as.data.frame(correlation_matrix)

# Eliminar la columna `term` y convertir a matriz numérica
correlation_matrix_num <- correlation_df %>%
  select(-term) %>%
  as.matrix()

# Asegurarse de que los nombres de las filas y columnas sean correctos
rownames(correlation_matrix_num) <- correlation_df$term
colnames(correlation_matrix_num) <- correlation_df$term

# Mostrar la matriz de correlación numérica
correlation_matrix_num

# Convert the matrix to data.frame format for display
adjacency_df <- as.data.frame(adjacency_matrix)

# Crear la matriz de adyacencia aplicando el umbral
adjacency_matrix <- abs(correlation_matrix_num) > threshold

# Convertir la matriz de adyacencia a 1s y 0s
adjacency_matrix <- adjacency_matrix * 1

# Mostrar la matriz de adyacencia
adjacency_matrix


# Crear el grafo a partir de la matriz de adyacencia
graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "undirected", diag = FALSE)

# Verificar el grafo
print(graph)

# Graficar el grafo
windows()
plot(graph, vertex.label = NA, 
     main = "Grafo basado en la matriz de correlación")
ecount(graph) # 6101
vcount(graph) #169
