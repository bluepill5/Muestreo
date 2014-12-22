###################################
##### Captura-recaptura  – ui.R #####
###################################
# Librerias requeridas
library(shiny)
shinyUI(pageWithSidebar( # standard shiny layout, controls on the
    # left, output on the right
    headerPanel("Captura y Recaptura"), # give the interface a title
    sidebarPanel( # all the UI controls go in here
        h4("Datos para obtener los estimadores:"),
        numericInput(inputId = "N",
                     label = "Tamaño de la población:",
                     min = 100, max = 10000, value = 1000),
        numericInput(inputId = "n1",
                     label = "Tamaño de la primera muestra:",
                     min = 10, max = 10000, value = 200),
        numericInput(inputId = "n2",
                     label = "Tamaño de la segunda muestra:",
                     min = 10, max = 10000, value = 100),
        h4("Datos para usar Bootstrapping:"),
        numericInput(inputId = "nResamp",
                     label = "Número de muestras:",
                     min = 10, max = 10000, value = 1000),
        numericInput(inputId = "sizeResamp",
                     label = "Tamaño de las muestras:",
                     min = 10, max = 10000, value = 100)
    ),
    mainPanel( # all of the output elements go in here
        # element as defined in server.R
        plotOutput("plot"),
        h3("Estimadores usando el modelo de Petersen:"),
        tableOutput("resultsPete"),
        h3("Estimadores usando la modificación de Chapman:"),
        tableOutput("resultsChap"),
        h3("Intervalo de confianza usando bootstrapping:"),
        tableOutput("resultsBoots"),
        h3("Histograma de las estimaciones de N usando bootstrapping:"),
        plotOutput("plotNEst")
    )
))
