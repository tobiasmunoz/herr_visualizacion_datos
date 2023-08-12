library(MASS)

set.seed(1234)

datos <- mvrnorm(50, c(0,0), matrix( data = c(1, 1/2, 1/2, 1), nrow = 2, ncol = 2, byrow = TRUE)*2 )
plot(datos, xlim = c(-4,4), ylim = c(-4,4), col = "gray", pch = 16)

abline(0,1)

a1 <- matrix( c(1,1))
P <- a1 %*% t(a1) # Matriz de proyeccion

points( datos %*% P, datos %*% P, col = "red", pch = 16)

d <- a1 / sqrt(2) # Vector normalizado

