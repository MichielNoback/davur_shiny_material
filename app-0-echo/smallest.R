
    ui <- fluidPage(
        title = "First Page",
               h2("Echo me!"),
               textInput(inputId = "my_input", label="Input: "),
               textOutput(outputId = "echo_out")
    )
    
    server <- function(input,output,session){
        output$echo_out <- renderText(input$my_input)
    }
    
    shinyApp(ui, server)
    
