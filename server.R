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
      
      outputFileName <- reactive({
        output$fileNameOutput <- renderUI({
          if(input$sheetTypeBtn == "Balance Sheet"){
            str <- "BS"
          }
          else if(input$sheetTypeBtn == "Income Statement"){
            str <- "IS"
          }
          else{
            str <- "CF"
          }
          userInput <- toupper(input$searchInput)
          fileName <- paste(userInput, str, input$datePicker, sep = '_')
          fullFileName <- paste(fileName, ".CSV", sep = '')
          HTML(paste("The file is saved as ", fullFileName, sep = ''))
        })
      })
      
      displayWarning <- reactive({
        output$warningDisplay <- renderUI({
          HTML("The specified document could not be found")
        })
      })
      
      output$compForm <- renderTable({
        if(input$searchInput == "Balance Sheet"){
          str <- "BS"
        }
        else if(input$sheetTypeBtn == "Income Statement"){
          str <- "IS"
        }
        else{
          str <- "CF"
        }
        
        userInput <- toupper(input$searchInput)
        fileName <- paste(userInput, str, input$datePicker, sep = '_')
        fullFileName <- paste(fileName, ".CSV", sep = '')
        if(file.exists(fileName)){
          outputFileName()
          read.csv(file = fileName)
        }
        else{
          #result <- tryCatch({
            if(input$sheetTypeBtn == "Balance Sheet"){
              companyFile <- GetBalanceSheet(input$searchInput, input$datePicker)
              if(is.null(companyFile)){
                displayWarning()
              }
            }
            else if(input$sheetTypeBtn == "Income Statement"){
              companyFile <- GetIncome(input$searchInput, input$datePicker)
            }
            else{
              companyFile <- GetCashFlow(input$searchInput, input$datePicker)
            }
            
            write.csv(companyFile, fileName)
            outputFileName()
            read.csv(file = fileName)
          #}, warning = function(war) {
          #  displayWarning()
          #}
          #)
        }
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
    
    