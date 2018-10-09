library(data.table)
library(scatterplot3d)
library(Rtsne)
library(shiny)

# trabalho final de aprendizado de máquina

# TODO implementar o tsne com plots graduais
# -> comparar com PCA
# fazer clustering com k++

ui <- fluidPage(
  
  titlePanel('Visualizando dados multidimensionais com T-SNE'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "perplexity",
                  label = "Perplexity",
                  min = 5,
                  max = 49,
                  value = 30)
    ),
    mainPanel(
      plotOutput(outputId = 'tsne')
    )
  )
)
    
#      selectInput(inputId = 'dims',
#                  label = h3('Número reduzido de dimensões'),
#                  choices = list('1' = 1, '2' = 2, '3' = 3))
#      selectInput(inputId = 'dataset',
#                  label = h3('Conjunto de dados'),
#                  choices = list('iris' = 'iris')),

# fazer fluid rows?


server <- function(input, output) {
  output$tsne <- renderPlot({
    set.seed(06061998)
    iris <- datasets::iris
    iris <- as.data.table(iris)
    labels <- iris[, Species]
    colors <- rainbow(length(unique(labels)))
    names(colors) = unique(labels)
    iris[, Species := NULL]
    dims <- 2 # fazer como input
    tsne <- Rtsne(iris, dims = dims, perplexity = input$perplexity, check_duplicates = FALSE)
    if (dims == 3) {scatterplot3d(tsne$Y, pch = 20, bty = 'n')}
    else {
      plot(tsne$Y, pch = 20, bty = 'n')
      points(tsne$Y, pch = 19, col = colors[labels])
      legend('topright', bty = 'n', legend = unique(labels), text.col = unique(colors[labels]))
    }
  })
}
# colors[labels]
shinyApp(ui = ui, server = server)