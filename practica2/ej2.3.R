# Practica 2
# Ejercicio 3

datos <- read.table('constructora.txt', header = TRUE)
datos <- datos[ -c(1) ]

x <- as.matrix( scale(datos, scale = FALSE) ) # 10 observaciones
                
# a)

pairs(x)

# b)

eigen_x <- eigen( cov(x) )

transf_x <- x %*% eigen_x$vectors[,1:2]

# c)

plot(transf_x) # La relacion no es tan lineal como se observaba anteriormente en el pairs

eigen_x$vectors[,1:2] 
# En la primer componente contribuyen casi todas por igual. En la segunda
# se observa que la tercera variable contribuye muy poco a diferencia de las otras que en modulo
# son bastante similares pero una contribuye positivamente y la otra negativamente.

# d)

cov(eigen_x$vectors)
cov(x)

# Como se comparan las matrices de covarianzas de las componentes principales con la de los datos?
# Estan en escalas distintas.

sum( cov(eigen_x$vectors)[,1:2] ) # capturan 99% de la variabilidad total las dos primeras componentes

# e) Repetir el analisis obteniendo los scores a partir de una estimacion de Corr(x) y comentar las diferencias.


eigen_x_cor <- eigen( cor(x) )

transf_x_cor <- x %*% eigen_x_cor$vectors[,1:2]

plot(transf_x_cor) # Ahora se observa una mejor correlacion de las variables

eigen_x_cor$vectors[,1:2] # Para la primer componente, todas las variables contribuyen negativamente.

cov(eigen_x_cor$vectors)
cov(x)

sum( cov(eigen_x_cor$vectors)[,1:2] ) # Las dos primeras componentes capturan el 47% de la variabilidad total