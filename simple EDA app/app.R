library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Automating Simple EDA"),
   mainPanel(tabsetPanel(id = "Tabs",
                         tabPanel("Data Input",
                                  sidebarPanel(fileInput("file", "Choose file",
                                                         multiple = FALSE, accept = 'text/csv'),
                                               numericInput("maxclass", 
                                                            "Maximum number of class for categorical variables",
                                                            min = 2, max = 20, value = 2),
                                               width = 12), # adjust width of the side bar panel
                                  mainPanel(h4('Data Preview'), dataTableOutput('dataprev'))),
                         tabPanel("Univariate",
                                  sidebarPanel(htmlOutput("var1"),
                                               verbatimTextOutput("var1_type"),
                                               htmlOutput("var2"),
                                               verbatimTextOutput("var2_type"),
                                               width = 12),
                                  mainPanel(
                                    sidebarPanel(tableOutput('var1_stats'), width = 4), 
                                    mainPanel(plotOutput("var1_plot"), width = 8),
                                    sidebarPanel(tableOutput('var2_stats'), width = 4),
                                    mainPanel(plotOutput("var2_plot"), width = 8)
                                    , width = 12)),
                         tabPanel("Bivariate",
                                  sidebarPanel(htmlOutput("var3"),
                                               verbatimTextOutput("var3_type"),
                                               htmlOutput("var4"),
                                               verbatimTextOutput("var4_type"),
                                               width = 12),
                                  mainPanel(plotOutput("bivar_plot"), width = 12))
                         ))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #### Reactive start#### 
  # read in data
  data <- reactive({
    infile <- input$file
    if (is.null(infile)) return (NULL)
    df <- read.csv(infile$datapath)
    
    # differentiate type of variable in data
    typ <- NULL
    for (i in 1:ncol(df)){
      if (is.numeric(df[,i]) && length(unique(df[,i])) > input$maxclass){ # both conditions have to be satisfied
        typ <- c(typ, "Num")
      } else {typ <- c(typ, "Cat")}
    }
    names(typ) <- colnames(df)
    return (list(df, typ))
  })
  #### Reactive end ####
  
  # note: 
  # data()[[1]] is the data itself, 
  # data()[[2]] contains the type of variable
  output$dataprev <- renderDataTable({ # data table changes if input data changes
    data.frame(data()[[1]][1:10,])
  }, options = list(searching = FALSE, paging = FALSE))
  
  #### Univariate Tab ####
  output$var1 <- renderUI({
    selectInput("var1", "Select a variable to visualize", colnames(data()[[1]]))
  })
  output$var1_type <- renderText({
    if (data()[[2]][input$var1] == "Num")
      type <- "Numerical Variable selected"
    else type <- "Categorical Variable selected"
    type
  })
  output$var2 <- renderUI({
    selectInput("var2", "Select a variable to visualize", colnames(data()[[1]]))
  })
  output$var2_type <- renderText({
    if (data()[[2]][input$var2] == "Num")
      type <- "Numerical Variable selected"
    else type <- "Categorical Variable selected"
    type
  })
  
  output$var1_stats <- renderTable({
    if (data()[[2]][input$var1] == "Num"){
      summ_stats <- as.data.frame(cbind(c("Min", "25th", "50th", "75th", "Max"),
                                        fivenum(data()[[1]][,input$var1])))
      colnames(summ_stats) <- c("Statistic", input$var1)
      summ_stats
    } else {
      summ_stats <- as.data.frame(table(data()[[1]][,input$var1]))
      colnames(summ_stats) <- c(input$var1, "Freq")
      summ_stats
    }
  })

  output$var2_stats <- renderTable({
    if (data()[[2]][input$var2] == "Num"){
      summ_stats <- as.data.frame(cbind(c("Min", "25th", "50th", "75th", "Max"),
                                        fivenum(data()[[1]][,input$var2])))
      colnames(summ_stats) <- c("Statistic", input$var2)
      summ_stats
    } else {
      summ_stats <- as.data.frame(table(data()[[1]][,input$var2]))
      colnames(summ_stats) <- c(input$var2, "Freq")
      summ_stats
    }
  })

  output$var1_plot <- renderPlot({
    tmp <- data.frame(data()[[1]][,input$var1])
    if (data()[[2]][input$var1] == "Num"){
      bw <- 2 * IQR(tmp[,1])/ nrow(tmp)^(1/3)
      ggplot(tmp, aes(x = tmp[,1])) + 
        geom_histogram(binwidth = bw) + theme_bw() +
        ggtitle(paste0("Histogram for ", input$var1))+
        xlab(input$var1)}
    else{
    ggplot(tmp, aes(x = tmp[,1])) + 
        geom_bar() + theme_bw() +
        ggtitle(paste0("Bar chart for ", input$var1)) +
        xlab(input$var1)}
  })
  
  output$var2_plot <- renderPlot({
    tmp <- data.frame(data()[[1]][,input$var2])
    if (data()[[2]][input$var2] == "Num"){
      bw <- 2 * IQR(tmp[,1])/ nrow(tmp)^(1/3)
      ggplot(tmp, aes(x = tmp[,1])) + 
        geom_histogram(binwidth = bw) + theme_bw() +
        ggtitle(paste0("Histogram for ", input$var2)) +
        xlab(input$var2)}
    else{
      ggplot(tmp, aes(x = tmp[,1])) + 
        geom_bar() + theme_bw() +
        ggtitle(paste0("Bar chart for ", input$var2)) +
        xlab(input$var2)}
  })
  
  #### Bivariate Tab ####
  output$var3 <- renderUI({
    selectInput("var3", "Select a variable to visualize", colnames(data()[[1]]))
  })
  output$var3_type <- renderText({
    if (data()[[2]][input$var3] == "Num")
      type <- "Numerical Variable selected"
    else type <- "Categorical Variable selected"
    type
  })
  
  output$var4 <- renderUI({
    selectInput("var4", "Select a variable to visualize", colnames(data()[[1]]))
  })
  output$var4_type <- renderText({
    if (data()[[2]][input$var4] == "Num")
      type <- "Numerical Variable selected"
    else type <- "Categorical Variable selected"
    type
  })
  
  output$bivar_plot <- renderPlot({
    tmp <- data.frame(data()[[1]][,c(input$var3, input$var4)])
    if (data()[[2]][input$var3] == "Num" & data()[[2]][input$var4] == "Num"){
      ggplot(tmp, aes(x = tmp[,1], y = tmp[,2])) + geom_point() + theme_bw() +
        ggtitle(paste0("Scatterplot of ", input$var3, " and ", input$var4)) +
        xlab(input$var3) + ylab(input$var4)
    } else if (data()[[2]][input$var3] == "Num" & data()[[2]][input$var4] != "Num"){
      ggplot(tmp, aes(x = tmp[,2], y = tmp[,1])) + geom_boxplot() + theme_bw() +
        ggtitle(paste0("Boxplot of ", input$var3, " and ", input$var4)) +
        xlab(input$var4) + ylab(input$var3)
    } else if (data()[[2]][input$var3] != "Num" & data()[[2]][input$var4] == "Num"){
      ggplot(tmp, aes(x = tmp[,1], y = tmp[,2])) + geom_boxplot() + theme_bw() +
        ggtitle(paste0("Boxplot of ", input$var3, " and ", input$var4)) +
        xlab(input$var3) + ylab(input$var4)
    } else {
      ggplot(tmp, aes(x = tmp[,1], fill = tmp[,2])) + geom_bar() + theme_bw() +
        ggtitle(paste0("Stacked bar chart of ", input$var3, " and ", input$var4)) +
        xlab(input$var3) + labs(fill = "Legend")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

