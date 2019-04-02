if (!require("shiny")){
  install.packages("shiny")
}

if(!require("finreportr")){
  install.packages("finreportr")
}

library(shiny)
library(finreportr)

#UI
shinyUI(fluidPage(titlePanel(h1(
  "Financial Metrics Analysis"
)),

sidebarLayout(
  sidebarPanel( 
    
    #This is the search area for inputting the name of the company 
    textInput("searchInput", "Enter Name of company (StockSymbol)",   ""),
    
    #options for choosing the type of the sheet
    checkboxGroupInput("checkboxGroup", "Choose the type of sheet:", choiceNames = list("Balance Sheet", "Income Statement", "Cash Flow") ,choiceValues = list("Balance Sheet", "Income Statement",
                  "Cash Flow")),
    
    #Year selection
    selectInput(
      inputId =  "datePicker", 
      label = "Select year:", 
      choices = 2009:as.numeric(format(Sys.Date(),"%Y"))
    ),
    
    actionButton("searchBtn", "Search"),
    
    fileInput( 'datafile', 'Choose CSV file',
               accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
  
  
  
  
  
  #this is the area where the output will be displayed
  mainPanel(
    
    ("Search results:"),
    htmlOutput("searchOutput"),
    htmlOutput("sheetTypeOutput"),
    htmlOutput("datePickedOutput"),
    htmlOutput("compInfo"),
    htmlOutput("fileNameOutput"),
    tableOutput("compForm"),
    
    #outputs the .csv file
    tableOutput("filetable")
    
    
    
  ))))
