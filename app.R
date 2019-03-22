#### Shiny app 01 ####
library(shiny)
library(ggplot2)
library(shinythemes)
data("iris")

# Define UI for application 
ui <- fluidPage(
  titlePanel(" ",
             windowTitle = "Exploring Iris data"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species",
                  label = "Filter by species of interest",
                  multiple = TRUE,
                  choices = levels(iris$Species),
                  selected = "setosa"),
      br(),
      hr(),
      helpText("Choose variables for individual histograms and scatterplot"),
      selectInput(inputId = "y",
                  label = "y-axis",
                  choices = colnames(iris),
                  selected = "Sepal.Length"),
      selectInput(inputId = "x",
                  label = "x-axis",
                  choices = colnames(iris),
                  selected = "Sepal.Width"),
      br(),
      hr(),
      helpText("Inputs below are only applicable to scatterplot"),
      numericInput(inputId = "size",
                   label = "point size",
                   min = 1, max = 5,
                   value = 3),
      sliderInput(inputId = "alpha",
                  label = "transparency of points",
                  min = 0, max = 1,
                  value = 0.6),
      checkboxInput(inputId = "diffBycolor",
                    label = "Differentiate by Species",
                    value = FALSE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data table", 
                           br(),
                           br(),
                           p("10 rows of no specific order are showed"),
                           dataTableOutput("datatable")
                           ),
                  tabPanel("Summary Stats", dataTableOutput("summary"),
                           br(), 
                           br(),
                           p(strong("Correlation between Numeric Variables")),
                           br(),
                           tableOutput("Cor")),
                  tabPanel("Distribution", plotOutput("Hist1"), plotOutput("Hist2")),
                  tabPanel("Scatterplot", plotOutput("Scatterplot"))
  )
)))

# Define server logic required to construct tables and plots
server <- function(input, output) {
  irissub <- reactive({
      subset(iris, iris$Species %in% input$species)
    })
  output$datatable <- renderDataTable({
    maxrow <- nrow(irissub())
    ranm <- runif(10, min = 10, max = maxrow)
    ranm
    irissub()[ranm,]
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$summary <- renderDataTable({
    summary(irissub())
  }, options = list(paging = FALSE, searching = FALSE))
  
  output$Cor <- renderTable({cor(irissub()[,1:4])
  }, rownames = TRUE)
  
  output$Hist1 <- renderPlot({
    ggplot(irissub(), aes_string(x = input$y)) +
      geom_bar() +
      ggtitle(paste0("Distribution of ", input$y))
    }, height = 350)
  
  output$Hist2 <- renderPlot({
    ggplot(irissub(), aes_string(x = input$x)) +
      geom_bar() +
      ggtitle(paste0("Distribution of ", input$x))
  }, height = 350)
  
  output$Scatterplot <- renderPlot({
     # color by Species if checked
     if (input$diffBycolor){
       ggplot(irissub(), aes_string(x = input$x, y = input$y, color = "Species"))+
         geom_point(alpha = input$alpha, size = input$size)
     } else {
       ggplot(irissub(), aes_string(x = input$x, y = input$y)) + 
         geom_point(alpha = input$alpha, size = input$size)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

