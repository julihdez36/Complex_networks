# Visualización de redes estáticas y dinámicas con R

# Ognyanova, K. (2023) Visualización de redes con R. Obtenido de www. kateto.net/network-visualization.

install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")


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


