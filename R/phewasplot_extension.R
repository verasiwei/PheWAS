#' This is a simple extension of PheWAS Manhattan plot that easy for clinicians to have a look
#'
library(PheWAS)
library(ggplot2)
library(shiny)

ui <- shinyUI(navbarPage(
  #Title
  titlePanel("PheWAS and GWAS Plot"),
  tabPanel("PheWAS",fluidRow(
      column(3,
               fileInput("result",
                         "Input PheWAS Result Data:",
                         accept = c("text/csv","text/comma-separated-values,text/plain",
                                    'text/tab-separated-values',".csv")),
               sidebarPanel(numericInput("pvalue","p-value threshold:",5*10^-5),width = 10)
             ),
      column(7,mainPanel(plotOutput("phewasplot", width = 800, height = 500),
                         downloadButton("downphewas","Download PheWAS Plot"),
                         tags$br(),
                         tags$hr(),
                         DT::dataTableOutput("phewastable",width = "80%")
      ))
    )),
  tabPanel("GWAS",fluidRow(
    column(3,
           fileInput("gwasresult",
                     "Input GWAS Result Data:",
                     accept = c("text/csv","text/comma-separated-values,text/plain",
                                'text/tab-separated-values',".csv")),
           sidebarPanel(numericInput("gwaspvalue","p-value threshold:",5*10^-5),width = 10)
    ),
    column(7,mainPanel(plotOutput("gwasplot", width = 1000, height = 500),
                       downloadButton("downgwas","Download GWAS Plot"),
                       tags$br(),
                       tags$hr(),
                       DT::dataTableOutput("gwastable",width = "80%")
    ))
  ))
))





server <- shinyServer(function(input, output){
  options(shiny.maxRequestSize=1000*1024^2)
  data <- reactive({
    req(input$result)
    if(is.null(input$result))     return(NULL)
    inFile <- input$result
    df=read.table(inFile$datapath, header = TRUE,sep = "\t",
                  colClasses = c("character","character","integer","integer",rep("numeric",4)))
    colnames(df)[1]="phenotype"
    colnames(df)[8]="p"
    return(df)
  })
  p <- reactive({
    pvalue=as.numeric(input$pvalue)
    return(pvalue)
  })

  output$phewasplot <- renderPlot(
    phewasManhattan(data(), annotate.angle=0,annotate.level=p(),title="PheWAS Manhattan Plot"))
  output$phewastable <- DT::renderDataTable(data(),options=list(pageLength=5))
  output$downphewas <- downloadHandler(
    filename = function(){
      paste("PheWAS","png",sep = ".")
    },
    content = function(file){
      png(file)
      renderPlot(
        phewasManhattan(data(), annotate.angle=0,annotate.level=p(),title="PheWAS Manhattan Plot"))
      dev.off()
      contentType = 'image/png'
    }
  )

  #GWAS
  gwasdata <- reactive({
    req(input$gwasresult)
    if(is.null(input$gwasresult))     return(NULL)
    inFile <- input$gwasresult
    df=read.table(inFile$datapath, header = TRUE)
    df=df[which(!is.na(df$P)),]
    df=df[,c("CHR","BP","P","SNP")]
    return(df)
  })
  gwasp <- reactive({
    pvalue=as.numeric(input$gwaspvalue)
    return(pvalue)
  })

  output$gwasplot <- renderPlot(
    manhattan(gwasdata(),col=c("#FF0000FF","#FF4600FF", "#FF8B00FF" ,"#FFD100FF","#824acd",
                                  "#2ac075","#3b5998","#b58096","#a958a5","#d1a258",
                                  "#00FFB9FF", "#f06261" ,"#00B9FFFF" ,"#0074FFFF" ,"#002EFFFF",
                                  "#1700FFFF" ,"#5D00FFFF" ,"#A200FFFF" ,"#E800FFFF" ,"#FF00D1FF",
                                  "#FF008BFF", "#FF0046FF"),suggestiveline=FALSE,genomewideline = -log10(5e-08),chrlabs = as.character(1:22),cex.axis=0.9,cex.lab=1.5))
  output$gwastable <- DT::renderDataTable(gwasdata()[which(gwasdata()$P<=gwasp()),],options=list(pageLength=5))

})


shinyApp(ui = ui, server = server)

