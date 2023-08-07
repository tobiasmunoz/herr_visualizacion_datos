#library(ggplot2)

# a)

datos <- read.csv("PBI.csv")
X <- as.matrix(datos) #  X en R3
# b)

pairs(datos)

# c)
n <- length(X[,1]) # n = 26 observaciones
x_bar <- colMeans(X) # Media de X (una por cada variable)
I <- diag( rep(1,n) ) # Identidad
unos <- matrix(1, n, 1) # Matriz llena de unos

H <- I - unos %*% t(unos) / n # Matriz de centrado
X_tilde <- H %*% X # Matriz centrada

# d)

Q <- t(X_tilde) %*% X_tilde # En la diagonal las varianzas, fuera de ella las covarianzas. Nos importa el signo.

S <- 1 / (n-1) * Q # Funcion de varianzas y covarianzas S

# e)

D_12 <- diag( diag(S)^(-1/2) ) # Diagonal a la -1/2

Z <- H %*% X %*% D_12 # Es una especie de estandarizacion pero marginando por columnas

pairs(Z) # Las escalas fueron uniformizadas.

# Todas estas cuentas se hacen con scale(X). Lo hicimos para entender como funciona.
# La matriz de covarianzas se hace con cov(X).

# f)

# Matriz de covarianzas y varianzas de Z = Matriz de correlaciones de X

cov(Z) # = t(Z) %*% Z / (n-1) 

# Nuevamente, todas estas cuentas se pueden hacer con cor(X) = cov(Z) = cov( scale(X) )
