if (!require("shiny")){
  install.packages("shiny")
}

if(!require("finreportr")){
  install.packages("finreportr")
}

if(!require("openxlsx")){
  install.packages("openxlsx")
}

if(!require("tools")){
  install.packages("tools")
}

library(shiny)
library(finreportr)
library(openxlsx)
library(tools)

shinyServer(
  function(input,output,session){
    
    #outputs
    observeEvent(input$searchBtn, {
      output$searchOutput <- renderUI({
        str1 <- "Company Searched:"
        str2 <- isolate(toupper(input$searchInput))
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
        isolatedSearchInput <- isolate(input$searchInput)
        isolatedSheetType <- isolate(input$sheetTypeBtn)
        isolatedDate <- isolate(input$datePicker)
        
        output$fileNameOutput <- renderUI({
          if(isolatedSheetType == "Balance Sheet"){
            str <- "BS"
          }
          else if(isolatedSheetType == "Income Statement"){
            str <- "IS"
          }
          else{
            str <- "CF"
          }
          userInput <- toupper(isolatedSearchInput)
          fileName <- paste(userInput, str, isolatedDate, sep = '_')
          fullFileName <- isolate(paste(fileName, ".CSV", sep = ''))
          HTML(paste("The file is saved as ", fullFileName, " in the ", userInput, " folder.", sep = ''))
        })
      })
      
      output$compForm <- renderTable({
        isolatedSearchInput <- isolate(input$searchInput)
        isolatedSheetType <- isolate(input$sheetTypeBtn)
        isolatedDate <- isolate(input$datePicker)
        
        if(isolatedSheetType == "Balance Sheet"){
          str <- "BS"
        }
        else if(isolatedSheetType == "Income Statement"){
          str <- "IS"
        }
        else{
          str <- "CF"
        }
        
        userInput <- toupper(isolatedSearchInput)
        fileName <- paste(userInput, str, isolatedDate, sep = '_')
        fullFileName <- paste(userInput, "/", fileName, ".CSV", sep = '')
        
        subDir <- userInput
        
        if(file.exists(fullFileName)){
          outputFileName()
          read.csv(file = fullFileName)
        }
        else{
          if(isolatedSheetType == "Balance Sheet"){
            companyFile <- GetBalanceSheet(isolatedSearchInput, isolatedDate)
          }
          else if(isolatedSheetType == "Income Statement"){
            companyFile <- GetIncome(isolatedSearchInput, isolatedDate)
          }
          else{
            companyFile <- GetCashFlow(isolatedSearchInput, isolatedDate)
          }
          
          if(!file.exists(subDir)){
            dir.create(subDir)
          }
          write.csv(companyFile, fullFileName)
          outputFileName()
          write.xlsx(read.csv(fullFileName), "test.xlsx")
          read.csv(file = fullFileName)
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
      output$fileNameOutput <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
      
      extension <- file_ext(infile$datapath)
      
      if(extension == "CSV"){
        read.csv(infile$datapath)
      }
      else if(extension == "xlsx"){
        read.xlsx(infile$datapath)
      }
      # Else read txt
    })
    
    #This previews the CSV data file
    output$filetable <- renderTable({
      filedata()
    })
    
  }
  
)
  

    
    
