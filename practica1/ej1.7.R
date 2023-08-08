# Ejercicio 1.7

# En un estudio sobre la contaminacion del agua, se tomaron muestras de n = 54 lagos en Estados Unidos.
# En cada observacion se registro:
#  • X1 = alcalinidad (mg/l de carbonato de calcio).
#  • X2 = clorofila e(mg/l).
#  • X3 = concentracion media de mercurio (en ppm) del tejido muscular de un grupo de peces tomados al azar.

library(plotly)

datos <- read.csv('mercurio.csv')
datos$etiqueta <- as.factor(datos$etiqueta)

# a) Visualizacion en tres dimensiones con colores por etiqueta (0 y 1)

fig <- plot_ly( datos, x = ~alcalinidad, y = ~clorofila, z = ~mercurio, color = ~etiqueta)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Alcalinidad'),
                                   yaxis = list(title = 'Clorofila'),
                                   zaxis = list(title = 'Mercurio')))
fig

# En el grafico se puede observar la gran mayoria de los datos para valores de clorofila y alcalinidad bajos.
# Donde se observa una gran variabilidad de valores de mercurio, tanto bajos como altos. Los datos con etiqueta
# 1 parecen ser datos atipicos.

# b) Transformaciones no lineales

W1 <- sqrt(datos$alcalinidad)
W2 <- sqrt(datos$clorofila)
W3 <- log(datos$mercurio)

# Visualizando como en el inciso anterior...

fig2 <- plot_ly( datos, x = W1, y = W2, z = W3, color = ~etiqueta)
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Alcalinidad'),
                                   yaxis = list(title = 'Clorofila'),
                                   zaxis = list(title = 'Mercurio')))
fig2

# Se ve que los datos ahora estan aplanados en el espacio tridimensional. Se puede observar tambien que un plano
# podria aproximar bien a estos datos en dicho espacio, a diferencia del grafico anterior.
# Nuevamente, las observaciones marcadas podria deberse a datos atipicos.

# c) Matriz de correlaciones

X_original <- as.matrix( datos[ c('alcalinidad','clorofila','mercurio')] )

X_transform <- as.matrix( cbind(W1, W2, W3)  )

cor(X_original)
cor(X_transform)

# Fuera de la diagonal se observan correlaciones mayores en modulo entre los atributos. Es decir, hay un aumento de
# la correlaciones, ya sea positivo o negativo.

# d)

# Se propone realizar una transformacion lineal al vector w = t(W1 W2 W3) = vector transpuesto
# cuyas direcciones de proyeccion estan en las columnas de A = [ -3/4  2/3
#                                                               -2/3  -4/5 
#                                                                1/5  -1/20 ]

# ¿Que dimension tienen los vectores resultantes?

# Aplicar esta transformacion a los datos y graficar usando la misma escala en el eje horizontal y el vertical.

# ¿Que observa en relacion a los puntos marcados?

# ¿Que observa en relacion a la covarianza muestral de los nuevos puntos?

A <- matrix( c(-3/4, 2/3, -2/3, -4/5, 1/5, -1/20), nrow = 3, ncol = 2, byrow = TRUE)

proyeccion_A <- ( X_transform %*% A ) %*% t(A) # Dimension 54x3

fig3 <- plot_ly( datos, x = proyeccion_A[,1], y = proyeccion_A[,2], z = proyeccion_A[,3], color = ~etiqueta)
fig3 <- fig3 %>% add_markers()
fig3 <- fig3 %>% layout(scene = list(xaxis = list(title = 'Alcalinidad'),
                                     yaxis = list(title = 'Clorofila'),
                                     zaxis = list(title = 'Mercurio')))
fig3

# Se observa que los datos quedaron perfectamente proyectados sobre un plano.
# Respecto a los puntos marcados, se ve que hay uno que se encuentra en el medio de los datos y los demas mas alejados.

cov(proyeccion_A)
cov(X_transform)

# La covarianza muestral de la proyeccion sobre A, respecto a la transformacion original, no parecen ser muy diferentes.
# Sin embargo, se puede ver que presentan valores mayores en modulo en general.

# e) Realizar un ranking con las distancias de Mahalanobis de las observaciones de w a su media muestral.

# Identificar la observacion que se encuentra a mayor distancia.

# ¿Que ocurre si se usan distancias euclıdeas?
  
# Uso funcion para calcular distancia del ejercicio 4 con modificaciones

calcular_di <- function(X, m, M){
  
  n <- length(X[,1])
  k <- length(m)
  
  X_tilde <- X
  
  for (i in 1:k){
    X_tilde[,i] <- X[,i] - m[i]
  }
  
  di_2 <-  X_tilde %*% solve(M) %*% t(X_tilde)
  di_2 <- diag( diag(di_2) )
  di <- sqrt(di_2)
  return( diag(di) )
  
}

# Para calcular la distancia de Mahalanobis de las observacions W a su media muestral uso:
# m = colMeans(w) , M = cov(w)

dist_mahalanobis_w <- calcular_di( X_transform, colMeans(X_transform), cov(X_transform))

dist_mahalanobis_w

# El mas alejado es el punto azul con mayor valor de clorofila en 'fig2'

# Usando distancia euclidea...

# Se usa M = Identidad kxk = 3x3

dist_euclidea_w <- calcular_di( X_transform, colMeans(X_transform), diag(3) )

dist_euclidea_w # Se obtienen valores mas grandes en modulo

# El mas alejado es el punto azul en la esquina mas alejada de los datos en 'fig2'
