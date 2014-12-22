#########################################################################################
#   
#  hw2Muestreo.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
# 
######################################################################################### 
# Idea:
# Crear un intervalo de confianza de 1 - alpha * 100% de confianza
# ya sea "m.a.s" "razon" "regresion"
# Crear una función para generar intervalos de confianza para estimadores por estratos:
# Se puede extender la función anterior para "m.a.s" "razon" "regresion" (?)
# Args:
L: Número de estratos
N_h: Número de unidades muestrales en el estrato h
N = sum(N_h): Número de unidades de la población


genCI <- function(muestra, N, alpha, method, type, XMuestra, X, XMean) {
    # Calcula el estimador, la desviación estándar del estimador, la 
    # precision y el intervalo de confianza. Para lo que se debe 
    # especificar la 'muestra', el tamaño de la población 'N', 'alpha' 
    # para la confiabilidad, el método 'method' (m.a.s, razon o 
    # regresion), y el tipo de estimador 'type' (media, total, proporcion, 
    # proporcionTot o r)
    #
    # Args:
    #   muestra: Conjunto de datos de los que se obtienen las muestras
    #   N: Tamaño de la población
    #   alpha: Sirve para indicar la confiabilidad del intervalo
    #   method: Método del estimador: 'm.a.s', 'razon' o 'regresion'
    #   type: Tipo del estimador: 'media', 'total' y dependiendo del
    #         método 'proporcion', 'r' o 'proporcionTot'
    #   XMuestra: En caso del método 'razon' o 'regresion' se requiere
    #             la muestra auxiliar 'XMuestra'
    #   X: En caso del método 'razon' o 'regresion' se requiere el
    #      total poblacional de la muestra 'XMuestra'
    #   XMean: En caso del método 'razon' o 'regresion' se requiere la
    #          media poblacional de la muestra 'Xmuestra'
    #
    # Returns:
    #   Lista con estimador, desviación estándar del estimador, intervalo 
    #   y precision
    #
    n <- length(muestra)
    # Calcula tanto el estimador como la varianza del estimador
    # dependiendo el método
    if (method == "m.a.s") { # m.a.s (únicamente)
        # Asigna el valor del estimador
        est <- switch(type,
                      media = mean(muestra),
                      total = N * mean(muestra),
                      proporcion = mean(muestra), 
                      proporcionTot = N * mean(muestra))
        # Asigna el valor de la estimación de la varianza
        varEst <- switch(type,
                         media = (1 - (n/N)) * var(muestra)/n,
                         total = (N**2) * (1 - (n/N)) * var(muestra)/n,
                         proporcion = (1 - (n/N)) * ((est * (1 - est))/(n - 1)),
                         proporcionTot = (N**2) * (1 - (n/N)) * ((est * (1 - est))/(n - 1)))
    } else if (method == "razon") { # m.a.s (razón)
        if (missing(XMuestra))
            stop("Falta muestra auxiliar.")
        # Se calcula el estimador de R y su varianza
        REst <- sum(muestra)/sum(XMuestra)
        varError <- sum((muestra - (REst * XMuestra))**2)/(n - 1)
        varREst <- (1 - (n/N)) * (varError/n) * (1/mean(XMuestra)**2)
        # Asigna el valor del estimador
        est <- switch(type,
                      media = if(!missing(XMean)) REst * XMean else stop("Falta X barra."),
                      total = if(!missing(X)) REst * X else stop("Falta X."),
                      r = REst)
        # Asigna el valor de la estimación de la varianza
        varEst <- switch(type,
                         media = XMean**2 * varREst,
                         total = X**2 * varREst,
                         r = varREst)
    } else if (method == "regresion") { # m.a.s (regresión)
        if (missing(XMuestra))
            stop("Falta muestra auxiliar.")
        bEst <- cov(muestra, XMuestra)/var(XMuestra)
        roEst <- cor(muestra, XMuestra)
        # Asigna el valor del estimador
        est <- switch(type,
                      media = if(!missing(XMean)) mean(muestra) + bEst * (XMean - mean(XMuestra)) else stop("Falta X barra."),
                      total = if(!missing(X)) (N * mean(muestra)) + bEst * (X - (N * mean(XMuestra))) else stop("Falta X."))
        # Asigna el valor de la estimación de la varianza
        varEst <- switch(type,
                         media = (1 - roEst**2) * (1 - (n/N)) * var(muestra)/n,
                         total = (1 - roEst**2) * (N**2) * (1 - (n/N)) * var(muestra)/n)
    }
    
    precision <- qnorm(1 - (alpha / 2)) * sqrt(varEst)
    # Agrega el factor de correción
    if (type == "proporcion") {
        ic <- (est + (c(-1, 1) * (precision))) + c(-1 / (2*n), 1 / (2*n))
    }
    else {
        ic <- est + (c(-1, 1) * precision)
    }
    # return
    list(estimacion = est, sdEstimador = sqrt(varEst), intervalo = ic, 
         precision = precision)
}

nEst <- function(alpha, precision, N, S2, method, type) {
    # Calcula el tamaño de la muestra estimado, indicando la 'precision',
    # el tamaño de la población 'N', varianza estimada 'S2', 'alpha'
    # para la confiabilidad, el método 'method' (m.a.s, razon o 
    # regresion), y el tipo de estimador 'type' (media, total, proporcion, 
    # proporcionTot o r)
    #
    # Args:
    #   alpha: Sirve para indicar la confiabilidad del intervalo
    #   precision: Precision
    #   N: Tamaño de la población
    #   S2: Varianza estimada
    #   method: Método del estimador: 'm.a.s', 'razon' o 'regresion'
    #   type: Tipo del estimador: 'media', 'total' y dependiendo del
    #         método 'proporcion', 'r' o 'proporcionTot'
    #
    # Returns:
    #   'n' tamaño de la muestra
    #
    z <- qnorm(1 - (alpha / 2))
    # Calcula el tamaño de la muestra dependiendo el método
    if (method == "m.a.s") {
        n0 <- switch(type,
                     media = precision**2/((z**2) * S2),
                     total = precision**2/((N**2) * (z**2) * S2),
                     proporcion = precision**2/((z**2) * S2), 
                     proporcionTot = precision**2/((N**2) * (z**2) * S2))
    } else if (method == "razon") {
        stop("En construcción :)")
    } else if (method == "regresion") {
        stop("En construcción :)")
    }
    n <- 1/(n0 + (1/N))
    round(n)
}

# Parámetros globales
alpha <- 0.05
############################### Ejercio 1 ###############################
# Parámetros
N <- 29661
muestra1 <- c(rep(1, times = 468), rep(0, times = 117))
precision <- 0.03
P <- 0.75
S2 <- (N * P * (1 - P))/(N - 1)
# Intervalo de Confianza
genCI(muestra = muestra1, N = N, alpha = alpha, method = "m.a.s", 
      type = "proporcion")

# Tamaño de la muestra
nEst(alpha = alpha, precision = precision, N = N, S2 = S2, 
     method = "m.a.s", type = "proporcion")

############################### Ejercio 2 ###############################
# Parámetros
N <- 226
muestra2 <- c(40, 42, 44, 45, 49, 51, 54, 53, 57, 58, 60, 61, 61, 61, 62, 
              61, 63, 63, 66, 67, 68, 70, 74, 77, 84, 85)
precision <- 750
S2 <- 10.35**2
# Intervalo de Confianza
genCI(muestra = muestra2, N = N, alpha = alpha, method = "m.a.s", 
      type = "total")
# Tamaño de la muestra
nEst(alpha = alpha, precision = precision, N = N, S2 = S2, 
     method = "m.a.s", type = "total")

############################### Ejercio 3 ###############################
# Parámetros
N <- 123
muestra <- c(702, 897, 1840, 1185, 690, 1208, 1124, 1656, 1806, 2542, 1127,
             995, 1967)

XMuestra <- c(633, 828, 1725, 1173, 713, 1127, 1067, 1380, 1553, 2013, 771, 
              838, 1760)
X <- 128200
# Intervalo de Confianza
genCI(muestra = muestra, N = N, alpha = alpha, method = "razon", 
      type = "total", XMuestra = XMuestra, X = X)

genCI(muestra = muestra, N = N, alpha = alpha, method = "m.a.s", 
      type = "total")


############################### Ejercio 4 ###############################
# Parámetros
N <- 150
muestra <- c(4560, 6120, 5040, 7440, 6960, 4920, 4680, 4320, 4560, 4920, 
             5400, 6120, 5040, 4800)
XMuestra <- c(30120, 38640, 35520, 42000, 41280, 31800, 34440, 33840, 
              41520, 39240, 37800, 36720, 33240, 34200)
# Intervalo de Confianza
genCI(muestra = muestra, N = N, alpha = alpha, method = "razon", 
      type = "r", XMuestra = XMuestra)

############################### Ejercio 5 ###############################
# Parámetros
N <- 1132
muestra <- c(125, 119, 83, 85, 99, 117, 69, 133, 154, 168, 61, 80, 114, 
             147, 122, 106, 82, 88, 97, 99)
XMuestra <- c(12, 11.4, 7.9, 9.0, 10.5, 7.9, 7.3, 10.2, 11.7, 11.3, 5.7, 
              8, 10.3, 12, 9.2, 8.5, 7, 10.7, 9.3, 8.2)
XMean <- 10.3
# Intervalo de Confianza
genCI(muestra = muestra, N = N, alpha = alpha, method = "razon", 
      type = "media", XMuestra = XMuestra, XMean = XMean)

genCI(muestra = muestra, N = N, alpha = alpha, method = "regresion", 
      type = "media", XMuestra = XMuestra, XMean = XMean)

