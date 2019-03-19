library(shiny)
library(finreportr)

shinyServer(
  function(input,output,session){
    
    #outputs
    observeEvent(input$searchBtn, {
      output$searchOutput <- renderUI({
        str1 <- "Company Searched:"
        str2 <- isolate(input$searchInput)
        HTML(paste(str1, str2, sep = ' '))
      })
      output$sheetTypeOutput <- renderUI({
        str1 <- "Form Selected:"
        str2 <- isolate(input$sheetTypeBtn)
        HTML(paste(str1, str2, sep=' '))
      })
      output$datePickedOutput <- renderUI({
        str1 <- "Year Selected:"
        str2 <- isolate(input$datePicker)
        HTML(paste(str1, str2, sep=' '))
      })
      output$compInfo <- renderUI({
        str <- isolate(CompanyInfo(input$searchInput))
        HTML(paste(str, sep=''))
      })
      
      output$compForm <- renderUI({
        if(input$sheetTypeBtn == "Balance Sheet"){
          #companyFile <- GetBalanceSheet(input$searchInput, input$datePicker)
          str <- "BS"
       }
        else if(input$sheetTypeBtn == "Income Statement"){
          #companyFile <- GetIncome(input$searchInput, input$datePicker)
          str <- "IS"
        }
        else{
          #companyFile <- GetCashFlow(input$searchInput, input$datePicker)
          str <- "CF"
        }
        filename <- paste(input$searchInput, str, input$datePicker, ".CSV", sep='')
        #write.csv(companyFile, filename)
        
        HTML(filename)
      })
    })
    
    #RadioButton Selector for Balance or Income
    
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      
      output$searchOutput <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
      output$sheetTypeOutput <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
      output$datePickedOutput <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
      output$compInfo <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
      read.csv(infile$datapath)
    })
    
    #This previews the CSV data file
    output$filetable <- renderTable({
      filedata()
    })
    
    }
  
)
    
    