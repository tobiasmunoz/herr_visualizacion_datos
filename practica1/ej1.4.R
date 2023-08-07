# Ejercicio 1.4

# Matriz de datos X (n filas = n observaciones, k columnas = atributos)
# Vector m en R^k
# M matriz simetrica en R^kxk

calcular_di <- function(X, m, M){
  
  n <- length(X[,1])
  k <- length(m)
  
  X_tilde <- sweep( X, k, m)
  di_2 <-  X_tilde %*% solve(M) %*% t(X_tilde)
  di_2 <- diag( diag(di_2) )
  di <- sqrt(di_2)
  return( diag(di) )
  
}

# Compruebo que la funcion funcione correctamente.

# a)

# Con M = Id devuelve las distancias euclideas al vector m

datos <- read.table("placas.txt", header = TRUE)
X <- as.matrix(datos)

m <- colMeans(X)
k <- length(m)
M <- diag(k)

dist_euclidea <- calcular_di(X,m,M)

# b)

# Si M^-1 = D^-1 = diag(s11-1, s22^-1, ..., skk-1) entonces...? Es casi la mitad de la euclidea? Preguntar...

M_diag <- diag( diag( cov(X) ))

dist_M_diag <- calcular_di(X,m,M_diag)

# c)

# Para devolver la distancia de Mahalanobis de los vectores a la media muestral se debe usar m = colMeans(X)
# y M = cov(X)

dist_mahalanobis <- calcular_di(X, colMeans(X), cov(X))