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
      
      displayCompanySearched <- function(){
        output$searchOutput <- renderUI({
          str1 <- "Company Searched:"
          str2 <- isolate(toupper(input$searchInput))
          HTML(paste(str1, str2, sep = ' '))
        })
      }
      
      displaySheetTypeSelected <- function(sheet1, sheet2, sheet3){
        if(is.na(sheet2) == TRUE){
          str <- sheet1
        }
        else if(is.na(sheet3) == TRUE){
          str <- paste(sheet1, "and", sheet2, sep = ' ')
        }
        else{
          str <- paste(sheet1, ", ", sheet2, " and ", sheet3, sep = '')
        }
        output$sheetTypeOutput <- renderUI({
          str2 <- "Sheet(s) Selected: "
          HTML(paste(str2, str, sep = ''))
        })
      }
      
      displayYearsSelected <- function(){
        year1 <- input$datePicker
        year2 <- input$datePicker2
        
        if(year1 == year2){
          str <- year1
        }
        else{
          str <- paste(year1, "-", year2, sep =' ')
        }
        
        output$datePickedOutput <- renderUI({
          str2 <- "Year(s) Selected: "
          HTML(paste(str2, str, sep = ''))
        })
      }
      
      displayCompanyInfo <- function(){
        output$compInfo <- renderUI({
          str <- isolate(CompanyInfo(input$searchInput))
          HTML(paste(str, sep = ''))
        })
      }
      
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
      
      noSheetSelectedMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "Please select a sheet type"
          HTML(paste(str, sep = ''))
        })
      }
      
      noCompanyNameMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "Please enter a company stock symbol"
          HTML(paste(str, sep=''))
        })
      }
      
      yearNotRightMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "The end year must be the same or after the start year"
          HTML(paste(str, sep=''))
        })
      }
      
      displayDownloadProgress <- function(count, fileCount){
        output$downloadProgress <- renderUI({
          count2 <- as.character(count)
          str <- "out of"
          str2 <- "files downloaded."
          HTML(paste(count2, str, fileCount, str2, sep = ' '))
        })
      }
      
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
        yearForIfCondition <- as.numeric(isolatedDate)
        yearForIfCondition2 <- as.numeric(isolatedDate2)
        
        filesToDownload <- c()
        fileCount <- 0
        yearCount <- 1
        status <- 1
        
        if(yearForIfCondition > yearForIfCondition2){
          status <- 0
          print("The end year must be the same or after the start year")
          yearNotRightMessage()
        }
        else{
          while(year != isolatedDate2){
            yearCount <- yearCount + 1
            year <- year + 1
          }
          
          if(isolatedSearchInput == ""){
            status <- 0
            print("No company name entered")
            noCompanyNameMessage()
          }
          else if(is.null(isolatedSheetType[1]) == TRUE){
            status <- 0
            print("No sheet type selected")
            noSheetSelectedMessage()
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
        }
        
        if(status == 0){
          
        }
        else{
          count <- 0
          displayDownloadProgress(count, fileCount)
          displayCompanySearched()
          displaySheetTypeSelected(isolatedSheetType[1], isolatedSheetType[2], isolatedSheetType[3])
          displayYearsSelected()
          displayCompanyInfo()
          
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
            count <- count + 1
            displayDownloadProgress(count, fileCount)
            next
          }
          
          read.csv(csvFile)
          #downloadFile(isolatedSheetType, isolatedDate, isolatedSearchInput)
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
  

    
    
