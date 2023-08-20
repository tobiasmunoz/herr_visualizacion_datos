# Ejercicio 6

library(MASS)

set.seed(1234)

sigma_x <- matrix( data = c(1, 1/2, 1/2, 1), nrow = 2, ncol = 2, byrow = TRUE)*2
x <- mvrnorm(50, c(0,0), sigma_x )
plot(x, xlim = c(-4,4), ylim = c(-4,4), col = "gray", pch = 16)

abline(1,1)

a1 <- matrix( c(1,1))
P <- a1 %*% t(a1) # Matriz de proyeccion

points( x %*% P, x %*% P, col = "red", pch = 16)

# d <- a1 / sqrt(2) # Vector normalizado

# a)

# x en R2. x ~ N(mu,sigma) multivariada
# a1 = (1,1)'

# Y1 = t( a1 ) * x --> E(Y1) = 0 y Var(Y1) = t( a1 ) * Sigma(x) * a1 = 6

# Y1 ~ N(0,6)

# b)

y1 <- rnorm( 50, 0, sqrt(6) )

hist(y1, freq = FALSE)

d <- density(y1, kernel = "gaussian")
lines(d)

# c)

a2 <- matrix( c(-1,1) / sqrt(2))

# Y2 = a2'X --> Y2 ~ N(0,1)

# vector aleatorio y = (Y1,Y2) dsitribuye Normal( (0,0), Sigma_y)
# Sigma_y es la matriz con diagonal Var(Y1) y Var(Y2) y fuera de la diagonal 0
# Esto pues ambas distribuciones tienen esperanza 0, y E(x) = 0.

# d)

plot(x, xlim = c(-4,4), ylim = c(-4,4), col = "gray", pch = 16)

abline(1,1, col = 'blue')
abline(-1/sqrt(2),1/sqrt(2), col = 'red')

# e)

sigma_y <-  matrix( data = c(6, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
y <- mvrnorm(50, c(0,0), sigma_y )

plot(y, col = 'gray', pch = 16)

cov(y)
cor(y)

# Se ve poca correlacion (negativa) entre Y1, Y2
# Y en el plot de Y1 vs Y2 no se observa una clara relacion entre ambas variables,
# aunque puede verse que cuando una aumenta la otra parece que disminuye.

# f) idem pero con direccion a3 = 1/sqrt(5)*(1,2)'
