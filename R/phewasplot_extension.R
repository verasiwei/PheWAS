#' This is a simple extension of PheWAS Manhattan plot that easy for clinicians to have a look
#'
library(PheWAS)
library(ggplot2)
library(shiny)

ui <- shinyUI(fluidPage(
  #Title
  headerPanel("PheWAS Manhattan Plot"),

  #Siderbar
  sidebarPanel(

    fileInput("result",
    "Input PheWAS Result Data:",
    accept = c("text/csv","text/comma-separated-values,text/plain",
               'text/tab-separated-values',".csv")),

    sidebarPanel(numericInput("pvalue","p-value threshold:",5*10^-5),width = 10)

  ),

  #Main panel for output
  mainPanel(plotOutput("manhattanplot", width = 800, height = 500))
))

server <- shinyServer(function(input, output){
  data <- reactive({
    req(input$result)
    if(is.null(input$result))     return(NULL)
    inFile <- input$result
    df=read.table(inFile$datapath, header = TRUE,sep = "\t",
                  colClasses = c("character","character","integer","integer",rep("numeric",4)))
    return(df)
  })
  p <- reactive({
    pvalue=as.numeric(input$pvalue)
    return(pvalue)
  })

  output$manhattanplot <- renderPlot(
    phewasManhattan(data(), annotate.angle=0,annotate.level=p(),title="PheWAS Manhattan Plot"))

})


shinyApp(ui = ui, server = server)

