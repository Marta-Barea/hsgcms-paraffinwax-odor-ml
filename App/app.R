###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
################################# Shiny APP ###################################
###############################################################################

# This is a Shiny web application. You can run the application by clicking the 
# 'Run App' button above.

# Loading required packages ----

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(rJava)
library(xlsx)
library(xlsxjars)
library(openxlsx)
library(data.table)
library(stringr)
library(caret)
library(kernlab)

# Loading saved model ----

model_1 <- readRDS("SVM.rds") 
model_2 <- readRDS("SVR.rds")

# Server ----
addResourcePath("static", "static")

server <- function(input, output) {
  
  # Create a table output element
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    df <- if(stringr::str_ends(input$file1$datapath, "csv")) {
      read.csv(input$file1$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote)
    } 
    else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      openxlsx::read.xlsx(input$file1$datapath, 
                          rowNames = input$header,
                          sheet = as.numeric(input$sheet))
    }
    df <- df[,unique(names(df))]
    
    if(input$disp == "head") {
      return(head(cbind.data.frame(Sample = rownames(df),df)))
    }
    else {
      return(cbind.data.frame(Sample = rownames(df),df))
    }
    
  }
  )
  
  contents1 <- reactive({
    
    req(input$file1)
    
    df <- if(stringr::str_ends(input$file1$datapath, "csv")) {
      read.csv(input$file1$datapath,
               eader = input$header,
               sep = input$sep,
               quote = input$quote)
    } 
    else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      openxlsx::read.xlsx(input$file1$datapath, 
                          rowNames = input$header,
                          sheet = as.numeric(input$sheet))
    }
    df <- df[,unique(names(df))]

    
    # Make predictions with the SVM and SVR models
    predictions <- cbind.data.frame(`Sample` = rownames(df),`What is the odor intensity?`= predict(model_1, df),`Odor Level (%)`= predict(model_2, df))
    
    print(predictions)
  }
  )
  
  # Status/Output Text Box
  output$contents_1 <- renderText({
    if (input$submitbutton > 0) { 
      isolate("Complete calculation.") 
    } else {
      return("Waiting for calculation.")
    }
  })
  
  # Status/Output Text Box
  output$contents_2 <- renderText({
    if (input$submitbutton > 0) { 
      isolate("The prediction results are as follows:") 
    }
  })
  
  # Prediction results table
  output$predictions <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(contents1()) 
    } 
  })
  
  # Test data
  
  data <- openxlsx::read.xlsx("test_data.xlsx", 
                              sheet = 1)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(paste0("test_data", Sys.Date(), sep = ""),".xlsx",sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(data, file)
    }
  )
}
# User Interface ----

# Add icons along with the title in the shinydashboard header

title_uca <- tags$a(href = 'https://www.uca.es/',
                    tags$img(src = "static/logo-uca.jpg", 
                             height = '50',
                             width = '40'),
                    "University of Cadiz", 
                    target = "_blank")

title_ivagro <- tags$a(href = 'https://ivagro.uca.es/',
                       tags$img(src = "static/logo-ivagro.jpg", 
                                height = '50',
                                width = '40'),
                       "Viticulture and Agri-Food Research Institute", 
                       target = "_blank")

ui <- fluidPage(
  
  # Logos
  titlePanel(fluidRow(column(3, title_uca),column(9, title_ivagro))),
  
  # Theme 
  theme = shinytheme("cosmo"),
  
  # Navigation bar
  navbarPage("Detection and Quantification of Odor in Paraffin Waxes"),
  
  # Input: File type and preprocessing instructions 
  helpText(style = "text-align: justify;", "This Shiny app enables the prediction of 
           odor intensity and its percentage in paraffin waxes. The estimations are carried out employing 
           two predictive models based on the Gaussian-SVM and Gaussian-SVR algorithms on the Total Ion
           Spectra (TIS) data from the HS-GC/MS analyses."),
  
  
  # Dashboard
  
  # Social media 
  dashboardHeader(
    title = "", 
    tags$li(class = "dropdown", 
            tags$a(href = 'https://github.com/Marta-Barea',
                   "Source Code",
                   icon("github"),
                   target = "_blank")
    ),
    tags$li(class = "dropdown", 
            tags$a(href = 'https://orcid.org/0000-0003-3508-6038',
                   "Orcid",
                   icon("orcid"),
                   target = "_blank")
    ),
    tags$li(class = "dropdown", 
            tags$a(href = 'https://www.researchgate.net/profile/Marta-Barea-Sepulveda',
                   "ResearchGate",
                   icon("researchgate"),
                   target = "_blank")
    )
  ),
  
  # Title: Data upload title
  titlePanel("Upload Data"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Input: Download test set
      
      downloadButton("downloadData", label = "Download"),
      
      # Input: Download button instructions 
      helpText("Clicking on the 'Download' button will automatically download a test 
               dataset for using the App."),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Select a file 
      fileInput("file1", "File Input:",
                multiple = FALSE,
                accept = c("text/csv", "xlsx/xls",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls")),
      
      # Input: File type and preprocessing instructions 
      helpText(style = "text-align: justify;", "This App supports uploading files in .csv/txt and .xlsx/xls formats."),
      helpText(style = "text-align: justify;", "Note: TIS data must be normalized to the base peak (maximum signal)."),
      
      # Horizontal line 
      tags$hr(),
      
      # Submit data
      actionButton("submitbutton", "Submit"),
      
      # Input: File type instructions 
      helpText("Please click on the submit button once the data file has been uploaded."),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Checkbox if file has header 
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator 
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select sheet number 
      radioButtons("sheet", "Sheet",
                   choices = c("Sheet 1" = 1,
                               "Sheet 2" = 2,
                               "Sheet 3" = 3),
                   selected = 1),
      
      # Input: Select quotes 
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select number of rows to display and submit button 
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: Text
      tags$label(h5('Data uploaded are:')),
      
      # Output: Data 
      
      tableOutput("contents"),
      
      #
      verbatimTextOutput('contents_1'),
      
      #
      verbatimTextOutput('contents_2'),
      
    
      # Output: Predictions
      tableOutput("predictions")),
  )
)

# Run the application 
shinyApp(ui = ui, server = server)