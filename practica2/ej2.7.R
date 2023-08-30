# Practica 2
# Ejercicio 7

# a)

mu_x <- matrix( data = c(-3,2), nrow = 2, ncol = 1)
mu_y <- matrix( data = c(0,1), nrow = 2, ncol = 1)

Sx <- matrix( data = c(8,2,2,5), ncol = 2, nrow = 2)
Sy <- matrix( data = c(6,-2,-2,7), ncol = 2, nrow = 2)
Sxy <- matrix( data = c(3,1,-1,3), ncol = 2, nrow = 2) 

Sxeig <- eigen(Sx)
Syeig <- eigen(Sy)

SxI <- Sxeig$vectors %*% diag( 1 / Sxeig$values) %*% t(Sxeig$vectors) # Inversa de Sx = solve(Sx)
SyI <- Syeig$vectors %*% diag( 1 / Syeig$values) %*% t(Syeig$vectors) # Inversa de Sy = solve(Sy)

Xmat <- SxI %*% Sxy %*% SyI %*% t(Sxy)
Ymat <- SyI %*% t(Sxy) %*% SxI %*% Sxy

Xeig <- eigen(Xmat)
Yeig <- eigen(Ymat)

# Estandarizacion de los autovectores

XautovecST<-Xeig$vectors%*%diag(1/sqrt(diag( t(Xeig$vectors)%*%Sx%*%Xeig$vectors)))
YautovecST<-Yeig$vectors%*%diag(1/sqrt(diag( t(Yeig$vectors)%*%Sy%*%Yeig$vectors)))
XautovecST
YautovecST

alfa1 <- XautovecST[,1]
beta1 <- YautovecST[,1] 

var_U1 <- t(alfa1) %*% Sx %*% alfa1 
var_U1 # = 1
var_V1 <- t(beta1) %*% Sy %*% beta1 
var_V1 # = 1

cov_U1_V1 <- t(alfa1) %*% Sxy %*% beta1
cov_U1_V1 # Deberia dar 0... que esta mal?

# b) idem segunda variable canonica. Se ve lo mismo. Varianzas 1, covarianza = 0

alfa2 <- XautovecST[,2]
beta2 <- YautovecST[,2] 

var_U2 <- t(alfa2) %*% Sx %*% alfa2 
var_U2 # = 1
var_V2 <- t(beta2) %*% Sy %*% beta2
var_V2 # = 1

cov_U2_V2 <- t(alfa2) %*% Sxy %*% beta2
cov_U2_V2 # Deberia dar 0... que esta mal?

# c) ...
