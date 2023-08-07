# Ejercicio 1.2

# Placa rectangular de acero --> Largo y ancho (x1,x2) = x

# E(x) = mu = (10, 4)' Vector transpuesto

# Cov(x) = [ 5  -1
#           -1   1/2  ]

# Y1 = costo = 4(X1 + X2)
# Y2 = venta = 5X1 + 2X2

# a) Transformacion lineal de x a y con una matriz A en R2x2

# f: x -> y := f(x) = xA = y 
# Donde A = [ 4  4
#             5  2 ]
# A' = [ 4 5
#        4 2 ]

# b) Esperanza y covarianza de y

# E(y) = E(Ax) = AE(x) = A(10, 4)' = (40 + 16, 50 + 8) = (56, 58)
# Cov(y) = ACov(x)A' = [ 4*5 + 4*-1    4*-1 + 4*1/2
#                        5*5 + 2*-1    5*-1 + 2*1/2 ] A'
#                    = [ 16  -2
#                        23  -4 ] A'
#                    = [ 16*4 -2*4  16*5 -2*2
#                        23*4 -4*4  23*5 -4*2 ]
#                    = [ 56  76
#                        76  107 ]

# Y1 e Y2 no son variables independientes, pues de serlo
# los valores fuera de la diagonal en la matriz Cov(y) serian 0.

# c)

placas <- read.table('placas.txt', header = TRUE)

X <- as.matrix(placas)
A <- matrix( c(4,4,5,2), nrow = 2, ncol = 2, byrow = TRUE)

Y <- X %*% A

# d) Cov(x), Cor(X), Cov(Y), Cor(Y)

n <- length(X[,1]) # n = 14 observaciones

I <- diag( rep(1,n) ) # Identidad

unos <- matrix(1, n, 1) # Matriz llena de unos

H <- I - unos %*% t(unos) / n # Matriz de centrado
X_tilde <- H %*% X # Matriz centrada

Q <- t(X_tilde) %*% X_tilde # En la diagonal las varianzas, fuera de ella las covarianzas. Nos importa el signo.

cov_x <- 1 / (n-1) * Q # Funcion de varianzas y covarianzas S = Cov(X)

cov_x

D_12 <- diag( diag(cov_x)^(-1/2) ) # Diagonal a la -1/2

corr_x <- D_12 %*% cov_x %*% D_12 # Corr(X)

corr_x

# Como y = xA --> Cov(y) = A'Cov(x)A 

cov_y <- t(A) %*% cov_x %*% A # Cov(Y)

diag_cov_y <- diag( diag(cov_y)^(-1/2) )

corr_y <- diag_cov_y %*% cov_y %*% diag_cov_y  # Cor(Y)
