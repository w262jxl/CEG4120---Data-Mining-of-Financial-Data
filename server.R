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
        str1 <- "Form(s) Selected:"
        str2 <- isolate(input$checkboxGroup)
        HTML(paste(str1, str2, sep=' '))
      })
      
      output$datePickedOutput <- renderUI({
        str1 <- "Years Selected:"
        str2 <- isolate(input$datePicker)
        str3 <- isolate(input$datePicker2)
        HTML(paste(str1,str2,str3, sep='  '))
      })
      
      output$compInfo <- renderUI({
        str <- isolate(CompanyInfo(input$searchInput))
        HTML(paste(str, sep=''))
      })
      
      
      outputFileName <- reactive({
        isolatedSearchInput <- isolate(input$searchInput)
        isolatedSheetType <- isolate(input$checkboxGroup)
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
          HTML(paste("The files are saved in the ", userInput, " folder.", sep = ''))
        })
      })
      
      getFileInfo <- function(form, year, company) {
        fileInfo <- c(form, year, company)
        return(fileInfo)
      }
      
      downloadFile <- function(isolatedSheetType, isolatedDate, isolatedSearchInput) {
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
          return(fullFileName)
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
          return(fullFileName)
        }
      }
      
      output$compForm <- renderTable({
        isolatedSearchInput <- isolate(input$searchInput)
        isolatedSheetType <- isolate(input$checkboxGroup)
        isolatedDate <- isolate(input$datePicker)
        isolatedDate2 <- isolate(input$datePicker2)
        year <- as.numeric(isolatedDate)
        
        filesToDownload <- c()
        fileCount <- 0
        yearCount <- 1
        
        while(year != isolatedDate2){
          yearCount <- yearCount + 1
          year <- year + 1
        }
        
        if(is.null(isolatedSheetType[1]) == TRUE){
          print("No sheet type selected")
        }
        else if(is.na(isolatedSheetType[2]) == TRUE){
          selectedSheet1 <- isolatedSheetType[1]
          yearPlaceholder <- isolatedDate
          
          for(i in 1: yearCount) {
            newFile <- getFileInfo(selectedSheet1, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            yearPlaceholder <- as.numeric(yearPlaceholder)
            yearPlaceholder <- yearPlaceholder + 1
            yearPlaceholder <- as.character(yearPlaceholder)
            next
          }
        }
        else if(is.na(isolatedSheetType[3]) == TRUE){
          selectedSheet1 <- isolatedSheetType[1]
          selectedSheet2 <- isolatedSheetType[2]
          yearPlaceholder <- isolatedDate
          
          for(i in 1:yearCount) {
            newFile <- getFileInfo(selectedSheet1, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            newFile <- getFileInfo(selectedSheet2, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            yearPlaceholder <- as.numeric(yearPlaceholder)
            yearPlaceholder <- yearPlaceholder + 1
            yearPlaceholder <- as.character(yearPlaceholder)
            next
          }
        }
        else{
          selectedSheet1 <- isolatedSheetType[1]
          selectedSheet2 <- isolatedSheetType[2]
          selectedSheet3 <- isolatedSheetType[3]
          yearPlaceholder <- isolatedDate
          
          for(i in 1:yearCount) {
            newFile <- getFileInfo(selectedSheet1, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            newFile <- getFileInfo(selectedSheet2, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            newFile <- getFileInfo(selectedSheet3, yearPlaceholder, isolatedSearchInput)
            filesToDownload <- c(filesToDownload, newFile)
            fileCount <- fileCount + 1
            yearPlaceholder <- as.numeric(yearPlaceholder)
            yearPlaceholder <- yearPlaceholder + 1
            yearPlaceholder <- as.character(yearPlaceholder)
            next
          }
        }
        
        for (i in 1:fileCount) {
          x <- (i - 1) * 3 + 1
          print(x)
          isolatedSheetType <- filesToDownload[c(x)]
          x <- x + 1
          isolatedDate <- filesToDownload[c(x)]
          x <- x + 1
          isolatedSearchInput <- filesToDownload[c(x)]
          print(isolatedSheetType)
          print(isolatedDate)
          print(isolatedSearchInput)
          csvFile <- downloadFile(isolatedSheetType, isolatedDate, isolatedSearchInput)
          next
        }
        
        read.csv(csvFile)
        #downloadFile(isolatedSheetType, isolatedDate, isolatedSearchInput)
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
  

    
    
