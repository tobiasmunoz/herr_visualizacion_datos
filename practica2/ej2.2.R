# Ejercicio 2.2

# a, b, c --> hacer cuentas en papel

# d)

library(MASS)

sigma <- matrix( data = c(3, 1, 1, 1, 3, 1, 1, 1, 5), ncol = 3, nrow = 3)
mu <- matrix( data = c(0,0,0), nrow = 1, ncol = 3)

x <- mvrnorm(1000, mu, sigma )

pairs(x) # Previo a la estandarizacion

# Estandarizacion multivariante

eigen_values <- eigen(sigma)$values
eigen_vector <- eigen(sigma)$vector

sqrt_eigenvalues <- diag( sqrt(eigen_values) )

sigma_inv_1_2 <- eigen_vector %*% solve(sqrt_eigenvalues) %*% t(eigen_vector)
  
x_standarized <- sigma_inv_1_2 %*% t(x) # x - mu = x - (0,0,0) para cada fila

x_standarized <- matrix( x_standarized, ncol = 3, nrow = 1000)

pairs(x_standarized) # Luego de estandarizar

# Luego de la estandarizacion las nubes de puntos se encuentan centradas de forma mas esferica por asi decir.
# Previo a la estandarizacion la nube se encuentra mas estirada.

