#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

setwd("C:/Users/vtrai/Desktop/Docs POS/Materias/11 - Ciencia de Dados aplicada a Saude/Task1")

load("dataset.RData")
dataset.df

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Gráfico de barras interativo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Selecione o ano:",
                  choices = c(2020, 2025, 2030, 2035, 2040),
                  selected = 2020)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({

    sel <- which(c(2020, 2025, 2030, 2035, 2040) == input$year)
    #-----------------------------------------------------------------
    #Grafico de Barras
    dtA <- dataset.df[dataset.df$Year==input$year, ]
    barplot(Prediction~Population, data=dtA, las=3)
    title(main=paste0("Predictions for ", input$year))

  })
}

# Run the application
shinyApp(ui = ui, server = server)
