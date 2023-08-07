# Ejercico 1.3

# Matriz de datos X (n filas = n observaciones, k columnas = atributos).
# Vector m en Rk.

calcular_covarianza <- function(X,m){
  n <- length(X[,1])
  k <- length(m)
  X_tilde <- sweep( X, k, m)
  Q <- t(X_tilde) %*% X_tilde
  return( Q / (n-1) )
}

# Compruebo que la funcion funcione correctamente.

# a)
# si m = colMeans(X) = media de cada columna (atributo)
# entonces se obtiene Cov(X)

datos <- read.table("placas.txt", header = TRUE)
X <- as.matrix(datos)
m <- colMeans(X)

cov_x <- calcular_covarianza(X,m) # cov(X)

# b)

# Para que devuelva el mismo resultado que cor(X) se debe
# hacer diag^(-1/2) * cov(X) * diag^(-1/2)
# diag es la matriz diagonal de la matriz de covarianza

diag_cov <- diag( diag(cov_x)^(-1/2) ) # Con el ^-1/2 ya agregado

cor_x <- diag_cov %*% cov_x %*% diag_cov # cor(X)
