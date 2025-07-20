# -*- coding: utf-8 -*-
"""
Created on Sun Jul 20 21:24:13 2025

@author: Julian
"""


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

# 29:38 -- 3