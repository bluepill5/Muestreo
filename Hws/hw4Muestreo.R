#########################################################################################
#   
#  hw4Muestreo.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
# 
######################################################################################### 

############################### Problema 1 ###############################
# Es decir:
# UPM: 32 Estados + 1 entidad federativa
# USM: Municipios
N <- 33
n <- 4
m_i <- 3
M_i <- c(10, 121, 16, 106)
y_ij <- matrix(c(2, 8, 6, 3, 5, 10, 11, 8, 6, 4, 5, 12), ncol = 3, 
               nrow = 4, byrow = T)

# Calculamos las medias de las UPM
y_barra_i <- rowMeans(y_ij)
# Calculamos los estimadores de los totales de las UPM
y_tot_i <- M_i * y_barra_i
# Calculamos el promedio de los estimadores de los totales de las UPM
mean_y_tot_est <- mean(y_tot_i)
# Calculamos el estimador del total
Y_est <- N * mean_y_tot_est
Y_est
# Calculamos la estimación de las varianzas de las UPM
S2wi <- apply(y_ij, 1, var)
# Calculamos la estimación de la varianza entre totales de UPM
S2b <- var(y_tot_i)
# Calculamos el estimador de la varianza del estimador total
var_UPM <- (N**2) * ((1/n) - (1/N)) * S2b
var_noUPM <- (N/n) * sum((M_i**2) * ((1/m_i) - (1/M_i)) * S2wi)
varEst <- var_noUPM + var_UPM
# Donde el porcentaje de la variabilidad del estimador debido a las UPM
# es:
(var_UPM / varEst) * 100

############################### Problema 2 ###############################
# Fijamos la semilla
set.seed(1)
# Calculamos ui, pi y zi
data <- matrix(c(789, 38, 400, 11, 198, 3, 356, 6, 215, 9, 390, 6, 540, 8,
                 481, 13, 78, 3, 285, 12, 432, 6, 152, 4), ncol = 2, byrow = T)
data <- data.frame(data)
data[, 3] <- cumsum(data[, 1])
X <- sum(data[, 1])
data[, 4] <- data[, 1]/X
data[, 5] <- data[, 2] / data[, 4]
names(data) <- c("Xi", "Yi", "Ui", "pi", "zi")
data
# Considerando n = 3
n <- 3
alpha <- 0.05
muest <- round(runif(n, 1, X))
subData <- c()
for (i in 1:n) {
    subData <- rbind(subData, data[min(which(muest[i] <= data$Ui)), ])
}
# Obtenemos el estimador del total de desempleados:
Y_est <- mean(subData$zi)
Y_est
# Calculamos el intervalo del 95% de confianza
varEst <- var(subData$zi)/n
precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
ic <- Y_est + (c(-1, 1) * precision)
ic
# Considerando n = 4
# Fijamos la semilla
set.seed(2)
n <- 4
alpha <- 0.1
muest <- round(runif(n, 1, X))
subData <- c()
for (i in 1:n) {
    subData <- rbind(subData, data[min(which(muest[i] <= data$Ui)), ])
}
# Obtenemos el estimador del total de desempleados:
Y_est <- mean(subData$zi)
Y_est
# Calculamos el intervalo del 90% de confianza
varEst <- var(subData$zi)/n
precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
ic <- Y_est + (c(-1, 1) * precision)
ic

# Utilizando un ppt sitemático y considerando n = 3
# Fijamos la semilla
set.seed(1)
n <- 3
k <- round(X/n)
alpha <- 0.05
muest <- round(runif(1, 1, k)) + ((0:(n - 1)) * k)
subData <- c()
for (i in 1:n) {
    subData <- rbind(subData, 
                     data[min(which((muest[i]) <= data$Ui)), ])
}
# Obtenemos el estimador del total de desempleados:
Y_est <- mean(subData$zi)
Y_est
# Calculamos el intervalo del 95% de confianza
varEst <- var(subData$zi)/n
precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
ic <- Y_est + (c(-1, 1) * precision)
ic
# Utilizando un ppt sitemático y considerando n = 4
# Fijamos la semilla
set.seed(2)
n <- 4
k <- round(X/n)
alpha <- 0.1
muest <- round(runif(1, 1, k)) + ((0:(n - 1)) * k)
subData <- c()
for (i in 1:n) {
    subData <- rbind(subData, 
                     data[min(which((muest[i]) <= data$Ui)), ])
}
# Obtenemos el estimador del total de desempleados:
Y_est <- mean(subData$zi)
Y_est
# Calculamos el intervalo del 90% de confianza
varEst <- var(subData$zi)/n
precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
ic <- Y_est + (c(-1, 1) * precision)
ic