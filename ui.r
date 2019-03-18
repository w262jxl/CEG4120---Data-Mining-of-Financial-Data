library(shiny)
library(finreportr)

#UI
shinyUI(fluidPage(titlePanel(h1(
  "Income/Balance sheets finder"
)),

sidebarLayout(
  sidebarPanel(
    #This is the search area for inputting the name of the company 
    textInput("searchInput", "Enter Name of company (StockSymbol)",   ""),
    
    actionButton("searchBtn", "Search"),
    
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
  
    
    
  #options for chossing the type of the sheet
    radioButtons("sheetTypeBtn", "Choose the type of sheet:", list("Balance Sheet", "Income Statement", "Cash Flow"),
                 selected = "Balance Sheet"),
    
  #slider to choose the date looking for
    sliderInput("datePicker","Select the date Desired: ",min = 2009,max = 2019,value = "")),
  
  #this is the area where the output will be displayed
  mainPanel(
    
    ("Docs are viewed here!"),
    textOutput("searchOutput"),
    
    #outputs the .csv file
    tableOutput("filetable")
    
  
  
))))
