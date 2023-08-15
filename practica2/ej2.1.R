# Ejercicio 2.1

library(psych)

sigma <- matrix( data = c(3, 1, 1, 1, 3, 1, 1, 1, 5), ncol = 3, nrow = 3)

# a)

autov <- eigen(sigma)

U <- autov$vectors

lambda <- diag( autov$values )

sigma_analog <- U %*% lambda %*% t(U)

# b)

# Las componentes principales estan dadas por el producto de U con X.

# c) Matriz de varianzas y covarianzas de xi.

# sigma(xi) = t(U) %*% sigma(x) %*% U

sigma_xi <- t(U) %*% sigma %*% U

sigma_xi

# Se observa que en la diagonal tiene a los autovalores de mayor a menor.
# Fuera de la diagonal los numeros son practicamente cero.
# Es decir que lambda = sigma_xi

# d)

# Variabilidad total de un vector = traza(sigma del vector)

tr(lambda) # Traza del vector x
tr(sigma_xi) # Traza de xi

# Se observa que las trazas coinciden.

# e)

# Porcentaje que se conserva de la variabilidad total si se define a xi con las primeras dos dimensiones

# Las primeras dos dimensiones consiste en tomar las dos primeras columnas (los autovectores asociados a los dos autovalores de valor maximo)

U_2 <- U[,1:2]

sigma_xi_2 <- t(U_2) %*% sigma %*% U_2

tr(sigma_xi_2) / tr(sigma_xi) # Se conserva un 82% aproximadamente de la variabilidad total

# f)

x_obs <- matrix( c(2,2,1), nrow = 3, ncol = 1)

# Valores que toman las componentes principales

xi <- t(U) %*% x_obs # Valores de las componentes principales (son 3)

# g)

# Estimar la matriz cov(x,xi)

# Ambos son vectores, esta bien esto? preguntar.
# cov(x,xi) = E( x * t(xi) ) - E( x )*E( t(xi) )

# x tiene esperanza cero entonces el segundo termino se anula

x_obs %*% t(xi) # Y la esperanza de esto? 