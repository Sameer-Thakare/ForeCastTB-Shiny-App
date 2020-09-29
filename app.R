## app.R

library(shiny)
library(shinydashboard)
library(PSF)
library(forecast)
library(ForecastTB)
library(GlobalOptions)
library(gridExtra)
library(shape)
library(circlize)
library(decomposedPSF)
library(curl)
library(tinytex)
library(tidyverse)
library(broom)
library(dplyr)
library(DT)
library(htmlwidgets)
library(crosstalk)
library(lazyeval)
source("helpers.R", echo = TRUE)

#add this file and collapsible nature should work.
includeScript(path = "app.js") # 


ui <- dashboardPage(
    dashboardHeader(title = "ForecastTB Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dataset", tabName = "dataset", icon = icon("database")),
            menuItem("Forecast", tabName = "forecast", icon = icon("th"), badgeLabel = "new", badgeColor = "green"),
            menuItem("Description", tabName = "descri", icon = icon("dashboard")),
            menuItem("About ForecastTB", tabName = "about", icon = icon("dashboard")),
            menuItem("How to Use", tabName = "how", icon = icon("dashboard")),
            menuItem("References", tabName = "ref", icon = icon("dashboard")),
            menuItem("Credits", tabName = "credits", icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidPage(
            tabItems(
                tabItem(tabName = "dataset",
                        
                        fileInput(inputId = "file", "Upload a Time-series dataset", placeholder = "ex. nottem.csv", width = '100%'),
                            checkboxInput("use_default", "Use default dataset : nottem.csv", FALSE),
                        box(title="Entered Dataset", status = "primary", solidHeader = TRUE, width = "100%", height = NULL, collapsible = TRUE, collapsed = FALSE, DT::dataTableOutput("table")),
                        box(title="Summary of Entered Dataset", status = "primary", solidHeader = TRUE, width = "100%", height = NULL, collapsible = TRUE, collapsed = FALSE, DT::dataTableOutput("summary_table"))
                ),
                tabItem(tabName = "forecast",
                        
                        box(title = "Foreorecasting using ForecastTB Package", 
                            tabBox(

                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1",
                                tabPanel("Parameters", 
                                         uiOutput("select_method"), uiOutput("select_column"),
                                         sliderInput("k", "Number of Predictions :", min = 12, max = 100, value = 2, step = 1, width = "800px"),
                                         sliderInput("length", "Length of Data Subset :", min = 20, max = 100, value = 2, step = 1, width = "800px"),
                                         selectInput("strategy", "Strategies :", c("Recursive" = "Recursive","DirRec" = "DirRec") , multiple =FALSE, width = "800px"),
                                         ),
                                tabPanel("Add New Method",
                                         
                                         textAreaInput("User_method", "User Defined Method:", value = "", width ="500px" , placeholder = NULL, resize = "both"),
                                         HTML(paste(h6("For Ex: Add ETS Method (Copy and Paste)"),
                                               h6("test3 <- function(data, nval){"),
                                               h6("b <- as.numeric(forecast(ets(data), h = nval)$mean)"),
                                               h6("return(b)}")
                                         )),
                                         
                                         textInput("name_para", "Method Name with Parameters:", value = "", width ="500px" , placeholder = NULL ),
                                         HTML(paste(h6("For Ex: test3(data, nval) "))),
                                         textInput("name", "Method name:", value = "", width = "500px" , placeholder = NULL ),
                                         HTML(paste(h6("For Ex: ETS"))),
                                         actionButton("click", "ADD", width ="500px"),
                                         textOutput("met_added")
                                         )
                            ),   width = "100%", height = "100%", status = "primary", solidHeader = TRUE),
                        
                        box(title = "Error Values, Execution Time, Forecasted Values", width = "100%", status = "success", solidHeader = TRUE, plotOutput("predict")),
                        box(title = "Numerical Results", 
                            tabBox(
                                
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset2",
                                tabPanel(title = "Error Parameters", DT::dataTableOutput("error_results")),
                                tabPanel(title = "Predicted Values", div(style="overflow-x:scroll",DT::dataTableOutput("predicted_results"))), width = "100%", selected = NULL
                            ),width = "100%", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE),
                        
                        box(title = "Polar Plot", width = "100%", height = "100%", status = "success", solidHeader = TRUE, plotOutput("bars")),
                        downloadButton("report", "Generate report")
                ),
                
                tabItem(tabName = "descri", infoBox(title=NULL, includeHTML("https://cran.r-project.org/web/packages/ForecastTB/index.html"), width = 12)),
                tabItem(tabName = "about", infoBox(title=NULL, includeMarkdown("intro1.rmd"), width = NULL)),
                tabItem(tabName = "how", infoBox(title= h1("How To Use ForecastTB in R"), width=12)),
                tabItem(tabName = "ref", infoBox(title= h1("References"), width=12)),
                tabItem(tabName = "credits", infoBox(title= h1("Credits"), width = 12))
            )
            
        )
    )
)


##############################SERVER_SIDE#######################################

server <- function(input, output, session) {
    
    #input_file put into data
    data <- reactive({
        file1 <- input$file
        if(input$use_default==TRUE){
            read.csv(file="new/nottem.csv", header = TRUE,sep = ",", fill = TRUE)
        }
        else if(is.null(file1)){return()}
        else
            read.csv(file=file1$datapath, header = TRUE, sep = ",", fill = TRUE)
    })
    
    #displaying_dataset
    output$table <- DT::renderDataTable({
        if(is.null(data())){return()}
        data()
    })
    
    #summary_of_data
    output$summary_table <- DT::renderDataTable({
        if(is.null(data())){return()}
        summary(data())
    })
    
    #select_parameter_to_be predicted
    output$select_column <- renderUI({
        columns <- colnames(data(), do.NULL = TRUE, prefix = "col")
        selectInput(inputId = "select_header", "Prediction Parameter:", columns)
    })
    
    observeEvent(input$click, {
        #writing new method to helpers.R
        write(input$User_method,file="helpers.R",append=TRUE)
        source("helpers.R", echo = TRUE)
        choices1 <<- choices1 %>% add_row(methodsnames = input$name, methods = input$name_para)
        updateSelectInput(session, "method", "Method :", choices1$methodsnames, selected = "ARIMA")
        updateTextAreaInput(session, inputId = "User_method", label = "User Defined Method:", value = " ")
        updateTextInput(session, inputId = "name_para", label = "Method Name with Parameters:", value = " ")
        updateTextInput(session, inputId = "name", label = "Method name:", value = " ")
        output$met_added <- renderText({"Method Successfully Added!"})
        #choices1 <- isolate(choices1)
    })
    
    
    output$select_method <- renderUI({
        choices1 <<- data.frame(methodsnames = c("ARIMA", "LPSF", "PSF"),
                               methods= c("ARIMA", "test1(data, nval)", "test2(data, nval)"))
        selectInput("method", "Method(s) :", choices1$methodsnames , multiple =TRUE, selected = "ARIMA", width = "800px")
        #choices1 <- isolate(choices1)
    })
    
    
    #prediction_using_forecastTB
    output$predict <- renderPlot({
        if(is.null(data())){return()}
        header_name <- input$select_header
        x <- data()[,header_name]
        x <- as.numeric(unlist(x))
        
        
            index <- match(input$method, choices1$methodsnames)
            methods_1 <- choices1[index, ]$methods
            methodnames_1 <- choices1[index, ]$methodsnames
            
                a1 <- prediction_errors(data = x, nval = input$k, dval = (input$length/100)*(nrow(data())),
                                        Method = c(methods_1), MethodName = c(methodnames_1),
                                        strats = input$strategy,
                                        append_ = 0)
                plot(a1)
            
        
        # name_method <- input$name
        # if(is.null(name_method)){
        #     a1 <- append_(object = a1, Method = c("test3(data, nval)"), MethodName = c(name_method))
        #     plot(a1)
        # }
        # 
        
        #plotting circle of errors
        output$bars <-renderPlot({
            if(is.null(data())){return()}
            plot_circle(a1)
            })
        
        output$predicted_results <- DT::renderDataTable({
            if(is.null(data())){return()}
            a1@output$Predicted_Values
        })
        
        output$error_results <- DT::renderDataTable({
            if(is.null(data())){return()}
            a1@output$Error_Parameters
        })
        
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "report.html",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
                params <- list(n = input$strategy, m = input$k)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )})
        
    })

}

shinyApp(ui, server)