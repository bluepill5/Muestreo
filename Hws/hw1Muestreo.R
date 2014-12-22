#########################################################################################
#   
#  hw1Muestreo.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
# 
######################################################################################### 
dirWD <- readline("Dame la ruta de tu directorio de trabajo: ")
setwd(dirWD)
# Librerias requeridas:
library(plotrix)

# 1. Considere una variable aleatoria X que se distribuye Normal con 
# media = 75 y varianza = 256

# Idea:
# Crear una función que dada una función y un soporte, te colore el área
# debajo de la curva
shadeNorm <- function(mean, sd, q1, q2) {
    # Genera la gráfica de un función normal con media 'mean' y desviación
    # estándar 'sd', coloreando el área debajo de la curva de q1 a q2, es
    # decir, se debe cumplir que q1 < q2
    #
    # Args:
    #   mean: Media de la distribución normal
    #   sd: Desviación estándar de la distribución normal
    #   q1, q2: Intervalo en el que se colorarea la distribución normal
    #
    # Returns:
    #   Nada
    #
    cord.x <- c(q1, seq(q1, q2, 0.01), q2) 
    cord.y <- c(0, dnorm(seq(q1, q2, 0.01), mean = mean, sd = sd), 0) 
    curve(dnorm(x, mean = mean, sd = sd), from = mean - (sd * 3), 
          to = mean + (sd * 3),  col = "#104E8B", lwd = 2, 
          main = paste("Distribución Normal con", "media =", mean, 
                       "y sd =", sd), 
          xlab = "Valores", ylab = "f(x)",
          font.lab = "3", family = "mono")
    polygon(cord.x, cord.y, col = "#1E90FF", border = "#104E8B", lwd = 2)
}

mean <- 75
sd <- 16
# a) P(X > 80)
pa <- pnorm(80, mean = mean, sd = sd, lower.tail = FALSE)
shadeNorm(mean = mean, sd = sd, q1 = 80, q2 = mean + (sd * 3))
text(105, 0.023, paste("P(X > 80) =", format(pa, digits = 3)), cex = 1, 
     family = "mono")
# b) P(73 < X < 83)
pb <- pnorm(83, mean = mean, sd = sd) - pnorm(73, mean = mean, sd = sd)
shadeNorm(mean = mean, sd = sd, q1 = 73, q2 = 83) 
text(105, 0.023, paste("P(73 < X < 83) =", format(pb, digits = 3)), 
     cex = 1, family = "mono")
# c) Encuentre x tal que, P(X <= x) = 0.6
qc <- qnorm(0.6, mean = mean, sd = sd)
shadeNorm(mean = mean, sd = sd, q1 = mean - (sd * 3), q2 = qc)
text(105, 0.023, paste("P(X <= ", format(qc, digits = 3),") = 0.6", 
                       sep = ""), cex = 1, family = "mono")
# d) Encuentre x tal que, P(X >= x) = 0.25
qd <- qnorm(0.25, mean = mean, sd = sd, lower.tail = FALSE)
shadeNorm(mean = mean, sd = sd, q1 = qd, q2 = mean + (sd * 3)) 
text(105, 0.023, paste("P(X >= ", format(qd, digits = 3),") = 0.25", 
                       sep = ""), cex = 1, family = "mono")


# 2. En una escuela se levanta una encuesta con la que se registra el
# número de alumnos que manifiestan tener problemas de salud visual en
# cada grupo:
grupo <- c(1, 2, 3, 4, 5, 6, 7, 8)
vals <- c(0, 3, 8, 3, 5, 2, 6, 3)

# Calcule la media y la varianza poblacional
meanP <- mean(vals)
varP <- var(vals)
meanP
varP

# Calcule la media y varianza muestral para todas las posibles muestras 
# aleatorias simples
allComb <- combn(grupo, m = 3)
meansM <- numeric(ncol(allComb))
varsM <- numeric(ncol(allComb))

for (e in 1:ncol(allComb)) {
    meansM[e] <- mean(vals[allComb[1:3, e]])
    varsM[e] <- var(vals[allComb[1:3, e]])
}
meansM
varsM
# Determine la distribución muestral del número promedio de alumnos con
# problemas visuales por grupo
x <- as.numeric(names(table(meansM)))
y <- as.numeric(table(meansM)) / ncol(allComb)
plot(x, y, type = "h", lwd = 2, col = "#104E8B", 
     main = "Distribución Muestral del número promedio de alumnos 
     con problemas visuales por grupo ", xlab = "Valores", ylab = "p",
     font.lab = "3", family = "mono")
points(x, y, col = "#00B2EE", pch = 16)


# 3. Con respecto al programa "HOY NO CIRCULA" en la Secretaría de 
# Movilidad del DF se tiene la hipótesis de que la distribución de
# terminaciones de placas es uniforme, por lo cual se pide realicen la 
# realicen la siguiente investigación: muestrar las terminaciones de
# placas (mínimo 100 unidades en un día) y realizar un histograma con los
# datos recolectados de acuerdo con los días que "tendrían" que descansar
# (5-6 lunes, 7-8 martes, 3-4 miércoles, 1-2 jueves y 9-0 viernes) y
# realice sus observasiones respecto a la distribución de placas en el DF
placas <- sample(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1000, replace = TRUE)

decodeP <- function(lastDigitPlaca) {
    # Devuelve un valor entre (1, 1.5, 2, 2.5, 3), dependiendo el valor
    # del valor de 'lasrDigitPlaca' la idea es que estos números mapeen
    # a los valores ("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")
    #
    # Args:
    #   lastDigitPlaca: Representa el último dígito de una placa 
    #                   vehicular, es decir un valor de 0-9
    #
    # Returns:
    #   Un valor ya sea, 1, 1.5, 2, 2.5 o 3
    #
    lastDigitPlaca <- lastDigitPlaca + 1
    switch(lastDigitPlaca, 3, 2.5, 2.5, 2, 2, 1, 1, 1.5, 1.5, 3)
}

hist(unlist(lapply(placas, function(x) decodeP(x))), 
     main = "Histograma de las placas vehiculares en el D.F.",
     xlab = "Día", ylab = "Frecuencia", col = "#1E90FF", xaxt = "n",
     border = "#FFFFFF", font.lab = "3", family = "mono")
axis(side = 1, at = seq(from = 1, to = 3, by = 0.5), 
     lab = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes"))


# 6. El archivo datos.rdata contiene la información de la estatura de 400
# alumnos de educación media superior media en cms. Calcule la estatura
# promedio de esta población

# Idea:
# Generar una función que obtenga m muestras de tamaño n de un conjunto
# de datos de tamaño N, con o sin reemplazo

# Crear una función que genere el intervalo de confianza del (1 - alpha)
# porciento de una muestra, ya sea a través de una distribución normal
# o t-student

# Generar una función que grafique los intervalos de confianza y muestre
# la proporción de los que contienen al verdadero valor de la media
# poblacional
#
load("datos.Rdata")
genCIs <- function(datos, nMuest, size, alpha, N, meanP, varP) {
    # Genera una gráfica con 'nMuest'  intervalos de (1- alpha) * 100 de
    # confianza, apartir de 'nMuest' de tamaño 'size' de un conjunto de
    # datos de tamaño 'N', con media poblacional 'meanP', y el valor
    # opcional de su varianza poblacional 'varP', la gráfica muestra si
    # el intervalo contiene o no, al valor verdadero 'meanP'
    #
    # Args:
    #   datos: Conjunto de datos de los que se obtienen las muestras
    #   nMuest: Número de muestras
    #   size: Tamaño de cada muestra
    #   alpha: Sirve para indicar la confiabilidad del intervalo
    #   N: Tamño de la población
    #   meanP: Media poblacional
    #   varP: Varianza poblacional
    #
    # Returns:
    #   Lista de tamaño 'nMuest' con tamaño 'size' cada elemento
    #
    genMuestras <- function(datos, nMuest, size, replace = FALSE) {
        # Genera 'nMuest' número de muestras de tamaño 'size' del conjunto
        # de datos 'datos' con o sin reemplazo, dependiendo del parámetro
        # 'replace'
        #
        # Args:
        #   datos: Conjunto de datos de los que se obtienen las muestras
        #   nMuest: Número de muestras
        #   size: Tamaño de cada muestra
        #   replace: TRUE o FALSE, dependiendo si se quiere reemplazo
        #
        # Returns:
        #   Lista de tamaño 'nMuest' con tamaño 'size' cada elemento
        #
        muestras <- list()
        for (m in 1:nMuest)
            muestras[[m]] <- sample(datos, size, replace)
        muestras    
    }
    
    makeCI <- function(muestra, alpha, N, varP) {
        # Genera un intervalo de confianza a partir de una muestra de alguna
        # población con (1 - alpha) * 100% de confianza, este intervalo será
        # apartir de una distribución Normal si se indica la varianza 
        # poblacional 'varP', o con uns distribución t-student en caso 
        # contrario
        #
        # Args:
        #   muestra: Muestra con la que se estima el intervalo
        #   alpha: Sirve para indicar la confiabilidad del intervalo
        #   N: Tamño de la población
        #   varP: Varianza poblacional
        #
        # Returns:
        #   Vector númerico de tamaño dos donde el primer elemento es el
        #   límite inferior del intervalo, y el segundo el límite superior
        #
        n <- length(muestra)
        meanM <- mean(muestra)
        if (missing(varP)) { 
            varM <- var(muestra)
            sdMeanM <- sqrt((1 - (n / N)) * (varM / n))
            ci <- meanM + (c(-1, 1) * 
                (qt(1 - (alpha / 2), df = n - 1) * sdMeanM))
        } else {
            sdMeanM <- sqrt((1 - (n / N)) * (varP / n))
            ci <- meanM + (c(-1, 1) * 
                (qnorm(1 - (alpha / 2)) * sdMeanM))
        }
        ci
    }
    
    genCIs <- function(muestras, alpha, N, varP) {
        # Genera un "lista" de intervalos de confianza a partir de un 
        # conjunto de muestras de alguna población con (1 - alpha) * 100% de 
        # confianza, estos intervalos serán apartir de una distribución 
        # Normal si se indica la varianza poblacional 'varP', o con uns 
        # distribución t-student en caso contrario
        #
        # Args:
        #   muestras: Lista de muestra con la que se estiman los intervalos
        #   alpha: Sirve para indicar la confiabilidad del intervalo
        #   N: Tamño de la población
        #   varP: Varianza poblacional
        #
        # Returns:
        #   Data frame que contiene los intervalos de confianza donde en 
        #   cada fila el primer elemento es el límite inferior "li" del 
        #   intervalo, y el segundo el límite superior "ls"
        #
        if (missing(varP)) {
            cis <- lapply(muestras, function(x) makeCI(x, alpha, N))
        } else {
            cis <- lapply(muestras, function(x) makeCI(x, alpha, N, varP))
        }
        cis <- data.frame(matrix(unlist(cis), nrow = length(muestras), 
                                 byrow = T))
        names(cis) <- c("li", "ls")
        cis
    }
    
    ciColor <- function(ci, mean) {
        # Genera "Red" si el intervalo ci no contiene al valor mean, es 
        # decir, mean < ci[1] || mean > ci[2], y "Black" en caso contrario
        #
        # Args:
        #   ci: Vector de tamaño 2 que representa un intervalo
        #   mean: Valor que queremos verificar si está o no en el intrvalo
        #
        # Returns:
        #   "Black" o "Red"
        #
        if (mean < ci[1] || mean > ci[2]) {
            "Red"
        } else {
            "Black"
        }
    }
    
    # Crea los intervalos
    muestras <- genMuestras(datos, nMuest, size)
    if (missing(varP)) {
        cis <- genCIs(muestras, alpha, N)
    } else {
        cis <- genCIs(muestras, alpha, N, varP)
    }
    # calcula el porcentaje de intervalos que contienen el valor 'MeanP'
    cols <- apply(cis, 1, function(x) ciColor(x, meanP))
    tasaExito <- length(cols[cols == "Black"]) / nMuest
    # Genera el gráfico
    y <- as.numeric(lapply(muestras, mean))
    plotCI(x = 1:nMuest, y = y, li = cis[ , 1], ui = cis[ ,2], col = cols, 
           main = paste("Intervalos de ", (1 - alpha) * 100, 
                        "% de confianza\n", tasaExito * 100, 
                        "% de los intervalos contienen el valor real", 
                        sep = ""), 
           xlab = "Intervalo", ylab = "",font.lab = "3", family = "mono",
           lwd = 3)
    abline(h = meanP, lwd = 3)
    
    list(muestras = muestras, intervalos = cis, tasaExito = tasaExito)
}

N <- length(datos)
nMuest <- 100
alpha <- 0.05
varP <- var(datos)
meanP <- mean(datos)

set.seed(1)
ics <- genCIs(datos, nMuest, 50, alpha, N, meanP)
set.seed(2014)
ics <- genCIs(datos, nMuest, 50, alpha, N, meanP);
set.seed(2014)
ics <- genCIs(datos, nMuest, 75, alpha, N, meanP);
