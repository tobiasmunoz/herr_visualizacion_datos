# Practica 2
# Ejercicio 5

# biplot

datos <- read.csv("vinos.csv")

X <- as.matrix( scale(datos) ) # Estandarizo los datos.

S <- cov(X)
eig <- eigen(S)

# Me quedo con dos dimensiones porque quiero hacer un biplot de dos dimensiones (puede ser de mas, biplot no es plot de dos dimensiones. Viene de la descomposicion de X = G.t(H) )

# G = X.U.L^(-1/2)

U <- eig$vectors[,1:2] 
L <- diag( eig$values[1:2] )

G <- X %*% U %*% sqrt( solve(L) )

H <- U %*% sqrt( L )

plot(G, pch = 20, col = "gray") # Proyeccion de los datos estandarizados en dos dimensiones segun la direccion de las componentes principales

arrows( rep(0,4), rep(0,4), H[,1], H[,2], col = "purple") # Proyeccion de los vectores canonicos en el plano generado por las dos dimensiones en la cual reduzco los datos
text(H, labels = colnames(datos), col = "purple", pos = 2)

