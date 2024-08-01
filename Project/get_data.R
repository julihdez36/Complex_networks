
# Working libraries -------------------------------------------------------

library(fpp3) # time series
library(rjson)
library(blsAPI) # Official API

# Key Api

api_key <- '3e54893b7aa243398b4248404e48f378'

# Set working directory and identify items

setwd('C:/Users/Julian/Desktop/Cursos/Fisica/Econofísica/WorkBench')
getwd()
list.files()

# get list of ID series

id_list <- read.csv("Appendix 2. Content of CPI entr.csv")
nrow(id_list) # 160 bienes aparecen
colnames(id_list)
id_list[,4]

# Sample 

payload <- list(
  'seriesid'= id_list[,4],
  'startyear'=2012,
  'endyear'=2024,
  'catalog'=FALSE,
  'calculations'=FALSE,
  'annualaverage'=FALSE,
  'registrationKey'= api_key)

df <- blsAPI(payload = payload, api_version =  2,return_data_frame =  TRUE)
df

head(df)
colnames(df)
table(df$year)


# Series ID Formats
# https://www.bls.gov/help/hlpforma.htm#AP
# CPI Methods
# https://www.bls.gov/cpi/additional-resources/entry-level-item-descriptions.htm


length(table(df$seriesID))


?sort()
head(df[df$seriesID == 'APU0000701322',])



# Descargar el archivo
url <- "https://download.bls.gov/pub/time.series/cu/cu.item"
destfile <- "cu.item"
download.file(url, destfile)

# Leer el archivo descargado
cu_item <- read.table(destfile, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Mostrar las primeras filas del data frame
head(cu_item)

