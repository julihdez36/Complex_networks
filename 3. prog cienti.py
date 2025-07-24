 
# Definamos una función que convierte de coordenadas polares a cartesianas

from math import sqrt, atan

def CartesianaAPolares(x,y):
    r = sqrt(x**2 + y**2)
    t = atan(y/x)
    return r,t

x, y = 2.0, 4.0
radio, theta = CartesianaAPolares(x,y)

print(f'x = {x}, y = {y}, r = {radio}, theta = {theta}')



# Definamos una función que encuentre la raiz de una función

# Se urará el método de bisección 