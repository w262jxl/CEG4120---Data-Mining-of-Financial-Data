library(shiny)

shinyServer(
  function(input,output){
    
    #outputs
    output$searchOutput <- renderText(input$searchInput)
    
    output$sheetTypeOutput <- renderText(input$sheetTypeBtn)
    
    output$datePickedOutput <- renderText(input$datePicker)
    
    
    #RadioButton Selector for Balance or Income
    
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile$datapath)
    })
    
    #This previews the CSV data file
    output$filetable <- renderTable({
      filedata()
    })
    
  }
  
  
)