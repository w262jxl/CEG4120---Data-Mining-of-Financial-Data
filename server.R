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
      
      # Function - displays the company searched
      displayCompanySearched <- function(){
        output$searchOutput <- renderUI({
          str1 <- "Company Searched:"
          str2 <- isolate(toupper(input$searchInput))
          HTML(paste(str1, str2, sep = ' '))
        })
      }
      
      # Function - displays the sheet types searched
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
      
      # Function - displays the years searched
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
      
      # Function - displays the company information
      displayCompanyInfo <- function(){
        output$compInfo <- renderUI({
          str <- isolate(CompanyInfo(input$searchInput))
          HTML(paste(str, sep = ''))
        })
      }
      
      # Creates the file name and displays where the files are saved
      outputFileName <- reactive({
        isolatedSearchInput <- isolate(input$searchInput)
        isolatedSheetType <- isolate(input$checkboxGroup)
        isolatedDate <- isolate(input$datePicker)
        
        # Creates the file name
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
      
      # Function - Prints no sheet type selected message
      noSheetSelectedMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "Please select a sheet type"
          HTML(paste(str, sep = ''))
        })
      }
      
      # Function - Prints no company name entered message
      noCompanyNameMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "Please enter a company stock symbol"
          HTML(paste(str, sep=''))
        })
      }
      
      # Function - Prints year selected error message
      yearNotRightMessage <- function(){
        output$searchOutput <- renderUI({
          str <- "The end year must be the same or after the start year"
          HTML(paste(str, sep=''))
        })
      }
      
      # Function - Displays how many files were downloaded
      displayDownloadProgress <- function(count, fileCount){
        output$downloadProgress <- renderUI({
          count2 <- as.character(count)
          str <- "out of"
          str2 <- "files downloaded."
          HTML(paste(count2, str, fileCount, str2, sep = ' '))
        })
      }
      
      # Function - Returns file info in a form of a list
      getFileInfo <- function(form, year, company) {
        fileInfo <- c(form, year, company)
        return(fileInfo)
      }
      
      # Function - Downloads the file
      downloadFile <- function(isolatedSheetType, isolatedDate, isolatedSearchInput) {
        
        # Creating the file name
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
        
        # If file already exists, then open it
        if(file.exists(fullFileName)){
          outputFileName()
          return(fullFileName)
        }
        
        # If file doesn't exist, then download it
        else{
          
          # Call the correct function to download based off of sheet type
          if(isolatedSheetType == "Balance Sheet"){
            companyFile <- GetBalanceSheet(isolatedSearchInput, isolatedDate)
          }
          else if(isolatedSheetType == "Income Statement"){
            companyFile <- GetIncome(isolatedSearchInput, isolatedDate)
          }
          else{
            companyFile <- GetCashFlow(isolatedSearchInput, isolatedDate)
          }
          
          # If company file directory doesn't exist, create one
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
        
        # Get variables
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
        
        # Check if years are entered correctly (end year should be same as or after start year)
        if(yearForIfCondition > yearForIfCondition2){
          status <- 0
          print("The end year must be the same or after the start year")
          yearNotRightMessage()
        }
        else{
          
          # Find how many years are selected
          while(year != isolatedDate2){
            yearCount <- yearCount + 1
            year <- year + 1
          }
          
          # Check if a company name is entered
          if(isolatedSearchInput == ""){
            status <- 0
            print("No company name entered")
            noCompanyNameMessage()
          }
          
          # Check if a sheet type is selected
          else if(is.null(isolatedSheetType[1]) == TRUE){
            status <- 0
            print("No sheet type selected")
            noSheetSelectedMessage()
          }
          
          # Process if one sheet type is selected
          else if(is.na(isolatedSheetType[2]) == TRUE){
            selectedSheet1 <- isolatedSheetType[1]
            yearPlaceholder <- isolatedDate
            
            # For loop to check which files need to be downloaded.. the loop is for the years
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
          
          # Process if two sheet types are selected
          else if(is.na(isolatedSheetType[3]) == TRUE){
            selectedSheet1 <- isolatedSheetType[1]
            selectedSheet2 <- isolatedSheetType[2]
            yearPlaceholder <- isolatedDate
            
            # For loop to check which files need to be downloaded.. the loop is for the years
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
          
          # Process if all three sheet types are selected
          else{
            selectedSheet1 <- isolatedSheetType[1]
            selectedSheet2 <- isolatedSheetType[2]
            selectedSheet3 <- isolatedSheetType[3]
            yearPlaceholder <- isolatedDate
          
            # For loop to check which files need to be downloaded.. the loop is for the years
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
        
        # If status is 0, then there is some sort of input error (no company name entered, no sheet type
        # selected, or end year is before start year), in this case, do nothing. Else display and download files
        if(status == 0){
        }
        else{
          count <- 0
          displayDownloadProgress(count, fileCount)
          displayCompanySearched()
          displaySheetTypeSelected(isolatedSheetType[1], isolatedSheetType[2], isolatedSheetType[3])
          displayYearsSelected()
          displayCompanyInfo()
          
          # For loop - Downloads each file that needs to be downloaded
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
      
      # Remove display values
      output$downloadProgress <- renderUI({
        str <- ""
        HTML(paste(str, sep=''))
      })
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
  

    
    
