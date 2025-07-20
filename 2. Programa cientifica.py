# Sesión 2

# Estructuas de control de flujo

# --------------------------------------------------------
# Condicional if

# Imaginemos que queremos evalar los coeficientes de una ecuación cuadrática
# Queremos saber si tiene raices reales

# Para ello, debemos calcular el discriminante: d = b^2 - 4ac.
# Si el discriminante d  es mayor o igual a cero, la ecuación tiene raices reales

a = -1.0
b = 1.0
c = -3.2

d = b**2 - 4*a*c

if d >= 0:
    print('La ecuación tiene raices reales')
else: print('La ecuación tiene solución compleja')

# una forma de abreviar el if es a través del operador teniario

k= a if d>=0 else b


# --------------------------------------------------------
 
import random

i = 0
while i<2:
    print('hola')
    i += 1

# El while no tiene que ser un incremental, por ejemplo:
    
i = 0
while i < 5:
    i = int(10*random.random())
    print('i =', i)
    




