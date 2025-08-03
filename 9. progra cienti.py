# Data frame


import pandas as pd

lista = [[1,3],[5,8],[7,2],[4,9],[6,0]]

df = pd.DataFrame(data=lista, columns= ['x','y'])
df

df.shape # dimensión
df.size # numero de elementos
df.ndim # numero de columnas

df['z'] = [3,5,7,8,9]
df

# del datos['z'] # para borrar una columna o todo, es nativa, no de pd

# Selección de elementos por fila

df.loc[:,'z']

