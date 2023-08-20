# Practica 2
# Ejercicio 4

datos <- read.csv('paises_mundo.csv', row.names = 'Nombre')

# 1)

pairs( datos ) # Quito los paises

# Se observa una clara relacion lineal entre cons_energia y co2. En PNB y prod_elec tambien parece haber una
# cierta relacion lineal

# 2)

x_scaled <- as.matrix( scale(datos) )

sigma_x_scaled <- cov(x_scaled)

eigen_x_scaled <- eigen(sigma_x_scaled)

pc_scores_scaled <- x_scaled %*% eigen_x_scaled$vectors[,1:2]

plot( pc_scores_scaled ) 
# Se observa una cierta relacion lineal decreciente. Con algunos cuantos outliers.

# 3)

sum( cov( eigen_x_scaled$vectors )[,1:2] ) # 48% de variabilidad total acumulada por las dos primeras coordenadas de los scores

# 4)

matriz_correlaciones <- cor( pc_scores_scaled, x_scaled)

heatmap(matriz_correlaciones)

# El grafico permite visualizar que la primera componente tiene una alta correlacion con mortalidad infantil
# y menor con las otras. La segunda componente  presenta una correlacion tambien alta con co2 y energia, y menor
# con las demas variables.

# a)

# i) Datos originales sin estandarizarlos.

x <- as.matrix(datos)

sigma_x <- cov(x)

eigen_x <- eigen(sigma_x)

pc_scores <- x %*% eigen_x$vectors[,1:2]

plot( pc_scores ) # Gran concentracion de los datos en el (0,0)

sum( cov( eigen_x$vectors )[,1:2] ) # 43% variabilidad total.

matriz_correlaciones_standard <- cor( pc_scores, x)

heatmap(matriz_correlaciones_standard) 
# Aca se observa una fuerte correlacion de la componente 1 con energia, co2 y mort.inf.
# Y para la componente 2 con respecto a la gran mayoria de las variables, salvo prod_elec

# ii) Tomar logaritmo natural de los datos y usarlos sin estandarizar por columnas.
# idem...

# iii) Tomar logaritmo natural a los datos y luego estandarizarlos por columnas.

x_log <- log(x)

pairs(x_log) # Se puede ver claramente las relaciones lineales entre las variables gracias a la transformacion.

x_log_scaled <- scale(x_log)

sigma_x_log <- cov(x_log_scaled)

eigen_x_log <- eigen(sigma_x_log)

pc_scores_log_scaled <- x_log_scaled %*% eigen_x_log$vectors[,1:2]

plot( pc_scores_log_scaled ) # Nube de puntos muy dispersa.

sum( cov( eigen_x_log$vectors )[,1:2] ) # ~49% de variabilidad total acumulada por las dos primeras coordenadas de los scores

matriz_correlaciones_log <- cor( pc_scores_log_scaled, x_log_scaled)

heatmap(matriz_correlaciones)

# Fuerte correlacion entre la primer componente y mortalidad infantil.
# Componente 2 un poco menos fuertemente correlacionada con energia y co2