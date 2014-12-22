#########################################################################################
#   
#  actividadPresentación.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
#  Creado: Noviembre 11, 2014 
#  Última modificación: Noviembre 11, 2014
# 
######################################################################################### 
# Librerias necesarias
library(pheatmap)
# Fijamos una semilla
set.seed(55)
# Valores iniciales
tag <- 2
N <- 1000
n1 <- 200
n2 <- 100
nResamp <- 1000
sizeResamp <- 100
# FUNCIONES AUXILIARES
# Función que permuta a los elementos distintos de 0 de un vector
shufflePob <- function(v, grid) {sample(v, grid, replace = FALSE)}
# Función que sirve para muestrear de la población
samplePob <- function(pobVector, n) {
    samp <- sample(which(pobVector != 0), n, replace = FALSE)
    pobVector[samp]
}
# Función que sirve para marcar a los elementos de la población sin reemplazo
mark <- function(pobVector, n, tag) {
    marked <- sample(which(pobVector != 0), n, replace = FALSE)
    pobVector[marked] <- tag
    pobVector
}
# Funciones para realizar las estimaciones
estPeteFun <- function(n1, n2, m) { (n1 * n2) / m }
estChapFun <- function(n1, n2, m) { (((n1 + 1)*(n2 + 1))/(m + 1)) - 1 }
# Vectorizamos las funciones para los estimadores
estPeteFun <- Vectorize(estPeteFun)
estChapFun <- Vectorize(estChapFun)
# Master function
genEstPete <- function(N, n1, n2) {
    grid <- N * 10
    # Se crea el vector de 1's y 0's pero sin estar aleatorizados
    pobVector <- c(rep(0, grid - N), rep(1, N))
    # Se permutan los elementos en la matriz
    pobVector <- shufflePob(pobVector)
    # Se marca la población de la primera captura
    pobMarked <- mark(pobVector, n1, tag = tag)
    # La población se "mueve"
    pobVector <- shufflePob(pobMarked)
    # Tomamos una segunda muestra
    secondMuest <- samplePob(pobVector, n2)
    # Obtenemos el valor de m
    m <- sum(secondMuest == tag)
    NEstPete <- estPeteFun(n1, n2, m)
    # Obtenemos la varianza del estimador
    varEstPete <- ((n1**2) * n2 * (n2 - m)) / (m**3)
    # Obtenemos los intervalos de (1 - alpha)% de confianza aproximando a una normal
    alpha <- 0.05
    precisionPete <- qnorm(1 - (alpha / 2)) * sqrt(varEstPete)
    icPete <- NEstPete + (c(-1, 1) * precisionPete)
    
    list(NEstPete = NEstPete,
         varEstPete = varEstPete,
         icPete = icPete,
         m = m)
}

genEstChap <- function(N, n1, n2) {
    grid <- N * 10
    # Se crea el vector de 1's y 0's pero sin estar aleatorizados
    pobVector <- c(rep(0, grid - N), rep(1, N))
    # Se permutan los elementos en la matriz
    pobVector <- shufflePob(pobVector)
    # Se marca la población de la primera captura
    pobMarked <- mark(pobVector, n1, tag = tag)
    # La población se "mueve"
    pobVector <- shufflePob(pobMarked)
    # Tomamos una segunda muestra
    secondMuest <- samplePob(pobVector, n2)
    # Obtenemos el valor de m
    m <- sum(secondMuest == tag)
    # Y obtenemos una estimación del tamaño de la población
    NEstChap <- estChapFun(n1, n2, m)
    # Obtenemos la varianza del estimador
    varEstChap <- ((n1 + 1)*(n2 + 1)*(n1 - m)*(n2 - m))/(((m + 1)**2) * (m + 2))
    # Obtenemos los intervalos de (1 - alpha)% de confianza aproximando a una normal
    alpha <- 0.05
    precisionChap <- qnorm(1 - (alpha / 2)) * sqrt(varEstChap)
    icChap <- NEstChap + (c(-1, 1) * precisionChap)
    
    list(NEstChap = NEstChap,
         varEstChap = varEstChap,
         icChap = icChap,
         m = m)
}

genPlot <- function(N, n1, n2) {
    grid <- N * 10
    # Se crea el vector de 1's y 0's pero sin estar aleatorizados
    pobVector <- c(rep(0, grid - N), rep(1, N))
    # Se permutan los elementos en la matriz
    pobVector <- shufflePob(pobVector)
    # Se marca la población marcada
    pobMarked <- mark(pobVector, n1, tag = tag)
    pobVector <- pobMarked
    # Gráficamos la matriz
    pob <- matrix(pobVector, nrow = sqrt(grid), ncol = sqrt(grid), 
                  byrow = TRUE)
    pheatmap(pob, cluster_row = FALSE, cluster_col = FALSE, 
             color = c("#FFFFFF", "#000000", "#8B0000"))
}

genICBoots <- function(N, n1, n2, nResamp, sizeResamp) {
    grid <- N * 10
    # Se crea el vector de 1's y 0's pero sin estar aleatorizados
    pobVector <- c(rep(0, grid - N), rep(1, N))
    # Se permutan los elementos en la matriz
    pobVector <- shufflePob(pobVector)
    # Se marca la población de la primera captura
    pobMarked <- mark(pobVector, n1, tag = tag)
    # La población se "mueve"
    pobVector <- shufflePob(pobMarked)
    # Tomamos una segunda muestra
    secondMuest <- samplePob(pobVector, n2)
    # Los intervalos de confianza usando bootstrap
    remuest <- vector("list", nResamp)
    for(i in 1:nResamp)
        remuest[[i]] <- sample(secondMuest, sizeResamp, replace = TRUE)
    
    ms <- lapply(remuest, function(x){ sum(x == tag) })
    ests <- lapply(ms, function(m) { estChapFun(n1, n2, m) })
    ests <- Reduce(c, ests)
    qs <- quantile(ests, probs = c(0.025, 0.975))
    list(estimadores = ests, qs = qs)
}

genPlot(N, n1, n2)
genEstPete(N, n1, n2)
genEstChap(N, n1, n2)
genICBoots(N, n1, n2, nResamp, sizeResamp)$qs
hist(genICBoots(N, n1, n2, nResamp, sizeResamp)$estimadores,
     main = "Histograma de las estimaciones de N usando bootstrapping:",
     xlab = "Estimación", ylab = "Frecuencia", col = "#1E90FF", 
     border = "#FFFFFF", font.lab = "3", family = "mono")



