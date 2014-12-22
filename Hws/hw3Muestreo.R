#########################################################################################
#   
#  hw3Muestreo.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
# 
######################################################################################### 
# A nivel poblacional:
# L: Número de estratos
# N_h: Número de unidades muestrales en el estrato h
# N = sum(N_h): Número de unidades de la población
# y_hi: Valor de la medición u_hi
# Y_barra_h = sum(y_hi)/N_h: Media poblacional del estrato h
# Y_h = sum(y_hi): Total poblacional del estrato h
# Y = sum(Y_h): Total poblacional
# Y_barra = Y/N: Media poblacional
# S2_h = var(y_hi): Varianza poblacional del estrato h
# W_h = N_h/N: Peso del estrato h
# 
# A nivel muestral:
# n_h: Tamaño muetral del estrato h
# n = sum(n_h): Tamaño de la muestra
# Y_barra_h_Est = mean(y_hi): Estimador de la media del estrato h
# Y_h_Est = N_h * mean(y_hi): Estimador de la media del estrato h

############################### Problema 1 ###############################
N_h <- c(3500, 2000, 2000)
N <- sum(N_h)
n_h <- c(500, 300, 200)
p_Est_h <- c(0.13, 0.45, 0.5)
alpha <- 0.05
# Obtenga P_Est
W_h <- N_h / N
est <- sum(W_h * p_Est_h)
# Construya un intervalo del 95 % de confianza para P
varEst <- sum((W_h**2) * (1 - (n_h/N_h)) * ((est_h * (1 - est_h))/(n_h - 1)))
precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
ic <- est + (c(-1, 1) * precision)
ic
# Considera que el criterio de estratificación fue adecuado? Explique el 
# porqué de su respuesta
# (...)


############################### Problema 2 ###############################
N_h <- c(60, 40)
Y_barra_h <- c(6, 4)
S2_h <- c(4, 2.25)
Y <- sum(Y_barra_h * N_h)
N <- sum(N_h)
Y_barra <- Y/N
# Calcule la varianza poblacional para la variable "peso del elefante"
S2 <- sum(((N_h - 1) * S2_h)/(N - 1)) + 
    sum((N_h * (Y_barra_h - Y_barra)**2)/(N - 1))
# Si asumimos que el peso del elefante no ha cambiado significativamente de 
# un año a otro, obtenga el estimador de la varianza del total de peso del 
# lote, considere una m.a.s. de tamaño 10
n <- 10
varEstTot_mas <- N**2 * (1 - (n/N)) * (S2/n)
# Considere un muestreo estratificado (distribución proporcional) de tamaño 
# 10 y obtenga el estimador de la varianza del total del peso del lote
# Bajo distribución proporcional
n_h_Prop <- round((n * N_h)/sum(N_h))
n_h <- n_h_Prop
varEstTot_EstratosProp <- sum(N_h**2 * (1 - (n_h/N_h)) * (S2_h/n_h))
# Dé sus conclusiones


############################### Problema 3 ###############################
N_h <- c(482, 590, 532, 479, 166)
Y_barra_h <- c(6.2, 11.7, 18.5, 26.3, 49.3)
Y <- sum(Y_barra_h * N_h)
N <- sum(N_h)
Y_barra <- Y/N
S_h <- c(3.5, 4.8, 6.7, 8.1, 9.3)
S2_h <- S_h**2
W_h <- N_h/N
N <- sum(N_h)
# Para una muestra de 150 granjas, distribuya la muestra en los 5 estratos 
# utilizando:
n <- 150
# La distribución de Neyman
n_h_Ney <- round((n * N_h * S_h)/sum(N_h * S_h))
# La distribución proporcional
n_h_Prop <- round((n * N_h)/sum(N_h))
# Compare las precisiones de estos métodos con la del m.a.s.
# Calculamos la varianza poblacional
S2 <- sum(((N_h - 1) * S2_h)/(N - 1)) + 
    sum((N_h * (Y_barra_h - Y_barra)**2)/(N - 1))
# m.a.s
varEstMed_mas <- (1 - (n/N)) * (S2/n)
# Neyman
n_h <- n_h_Ney
varEstMed_EstratosNey <- sum((W_h**2) * (1 - (n_h/N_h)) * (S2_h/n_h))
# Proporcional
n_h <- n_h_Prop
varEstMed_EstratosProp <- sum((W_h**2) * (1 - (n_h/N_h)) * (S2_h/n_h))


############################### Problema 4 ###############################
n <- 6
y_i <- c(1, 1, 0, 1, 2, 0)
y_i_tv <- c(6.1, 2.6, 8.1, 6.4, 4.7, 5.6)
M_i <- c(1, 2, 3, 2, 3, 1)
N <- 0.74 * 1000000
M <- 1.3 * 1000000
M_barra_Est <- sum(M_i)/n
alpha <- 0.1
# Estime la proporción y número de adultos que sufren indigestión en la
# región
Y_est <- N * mean(y_i)
p_Est <- Y_est/M

# Estime el número promedio de horas que los adultos ven televisión
Y_barra_Est <- mean (y_i_tv)

# Calcule intervalos del 90% de confianza para las características de la 
# población estimadas en a) y b)
varEst_Tot <- (N**2) * (1 - (n/N)) * (var(y_i)/n)
varEst_p <- (N**2/M**2) * (1 - (n/N)) * (var(y_i)/n)
varEst_MedTV <- (1 - (n/N)) * (var(y_i_tv)/n)


precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst_Tot)
ic_Tot <- Y_est + (c(-1, 1) * precision)
ic_Tot

precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst_p)
ic_p <- p_Est + (c(-1, 1) * precision)
ic_p

precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst_MedTV)
ic_MedTV <- Y_barra_Est + (c(-1, 1) * precision)
ic_MedTV

############################### Problema 5 ###############################
n <- 25
N <- 109
M_i <- c(34, 32, 28, 39, 17, 34, 24, 30, 28, 21, 33, 20, 23, 44, 42, 31, 
         19, 24, 45, 35, 39, 21, 32, 20, 34)
y_i <- c(1749, 1661, 1639, 1771, 880, 1892, 1441, 1570, 1419, 946, 1782, 
         781, 1254, 2178, 2189, 1562, 990, 1188, 2211, 1914, 1925, 979, 
         1617, 1001, 1914)
# Estime la calificación promedio por estudiante. Obtenga un intervalo del
# 90% de confianza para esta calificación
alpha <- 0.1
Y_est <- N * mean(y_i)
M_barra_est <- mean(M_i)
M_est <- N * M_barra_est
Y_est_elem <- Y_est/M_est

varEst_elem <- (1 - (n/N)) * (1/n) * (1/(M_barra_est)**2) * 
    (sum((y_i - (Y_est_elem * M_i))**2)/(n - 1))

precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst_elem)
ic_elem <- Y_est_elem + (c(-1, 1) * precision)
ic_elem


