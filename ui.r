if (!require("shiny")){
  install.packages("shiny")
}

if(!require("finreportr")){
  install.packages("finreportr")
}

library(shiny)
library(finreportr)

# Get the start year
year <- format(Sys.Date(),"%Y")
year <- as.numeric(year)
year <- year - 9

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
      label = "Select start year:",
      choices = year:as.numeric(format(Sys.Date(),"%Y"))
    ),
    
    selectInput(
      inputId = "datePicker2",
      label = "Select end year:",
      choices = year:as.numeric(format(Sys.Date(),"%Y"))
    ),
    
    actionButton("searchBtn", "Search"),
    
    fileInput( 'datafile', 'Choose CSV file',
               accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
  
  #this is the area where the output will be displayed
  mainPanel(
    
    ("Search results:"),
    htmlOutput("downloadProgress"),
    htmlOutput("searchOutput"),
    htmlOutput("sheetTypeOutput"),
    htmlOutput("datePickedOutput"),
    htmlOutput("compInfo"),
    htmlOutput("fileNameOutput"),
    tableOutput("compForm"),
    
    #outputs the .csv file
    tableOutput("filetable")
    
    
    
  ))))
