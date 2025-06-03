library(shiny)

iw <- 100

ui<-
    fluidPage(
        fluidRow(
            column(4,
                   h2("\"$input\""),
                   textInput(inputId = "regular_input1", label="$input1", width = iw),
                   textInput(inputId = "regular_input2", label="$input2", width = iw),
                   textInput(inputId = "regular_input3", label="$input3", width = iw),
                   tableOutput("basic_reactivity_out")
            ),
            column(4,
                   
                   h2("reactive"),
                   textInput(inputId = "reactive1", label="reactive1", width = iw),
                   textInput(inputId = "reactive2", label="reactive2", width = iw),
                   textInput(inputId = "reactive3", label="reactive3", width = iw),
                   tableOutput("reactive_out")
            ),
            column(4,
                   
                   h2("reactiveVal"),
                   actionButton(inputId = "reactiveVal_go", label = "reactiveVal"),
                   textOutput("reactiveVal_out"),
            )
            
        ),
        fluidRow(
            column(4,
                   
                   h2("reactiveValues"),
                   textInput(inputId = "reactiveVals1", label="reactiveVals1", width = iw),
                   textInput(inputId = "reactiveVals2", label="reactiveVals2", width = iw),
                   textInput(inputId = "reactiveVals3", label="reactiveVals3", width = iw),
                   tableOutput("reactiveVals_out")
            ),
            column(4,
                   
                   h2("observe"),
                   textInput(inputId = "observe1", label="observe1", width = iw),
                   textInput(inputId = "observe2", label="observe2", width = iw),
                   textInput(inputId = "observe3", label="observe3", width = iw),
                   tableOutput("observe_out")
            ),
            column(4,
                   
                   h2("observeEvent"),
                   textInput(inputId = "observeEvent1", label="observeEvent1", width = iw),
                   textInput(inputId = "observeEvent2", label="observeEvent2", width = iw),
                   textInput(inputId = "observeEvent3", label="observeEvent3", width = iw),
                   tableOutput("observeEvent_out"),
                   actionButton(inputId = "observeEvent_go", label = "observeEvent")
            )
        ),
        
       #  fluidRow(
       #      column(12,
       #             h4("Note that observe and reactive work very much the same on the surface,
       # it is when we get into the server where we see the differences, and how those
       # can be exploited for diffrent uses.")
       #      ))
        
    )

server<-function(input,output,session){
    # Using regular reactivity of the $input elements. Can be combined into a more complex structure.
    
    output$basic_reactivity_out <- renderTable(
        data.frame("data" = c(input$regular_input1,
              input$regular_input2,
              input$regular_input3))
    )
    
    
    # Create a reactive Environment. Note that we can call the variable outside same place
    # where it was created by calling reactive_environment(). When the variable is called by
    # renderTable is when it is evaluated. No real difference on the surface, all in the server.
    
    reactive_environment <- reactive({
            c(input$reactive1, 
              input$reactive2, 
              input$reactive3)
        }
    )
    
    output$reactive_out <- renderTable({
        # will be called whenever one of the reactive 
        # values within the environment changes
        reactive_environment() 
    })
    
    
    # a reactiveVal wraps a single value that can be used as read/write "memory" for a user-bound
    # variable in your app
    
    reactive_str <- reactiveVal(value = "0")
    observeEvent(input$reactiveVal_go, {
        reactive_str(paste0(reactive_str(), "+0"))
        print(reactive_str())
    }) 
    output$reactiveVal_out <- renderText(reactive_str())
    
    
    
    reactiveVal_table <- reactiveValues(df = data.frame("data" = c(0, 0, 0)))
    
    observeEvent(c(input$reactiveVals1, input$reactiveVals2, input$reactiveVals3), {
        reactiveVal_table$df[1, 1] <- input$reactiveVals1
        reactiveVal_table$df[2, 1] <- input$reactiveVals2
        reactiveVal_table$df[3, 1] <- input$reactiveVals3
    })

    output$reactiveVals_out <- renderTable(reactiveVal_table$df)
    
    # Create an observe Environment. Note that we cannot access the created "df" outside 
    # of the env. A, B,and C will update with any input into any of the three Text Fields.
    
    observe({
        A <- input$observe1
        B <- input$observe2
        C <- input$observe3
        df <- c(A, B, C)
        output$observe_out <- renderTable({df})
    })
    
    #We can change any input as much as we want, but the code wont run until the trigger
    # input$Go is pressed.
    
    observeEvent(input$observeEvent_go, {
        A <- input$observeEvent1
        B <- input$observeEvent2
        C <- input$observeEvent3
        df <- c(A, B, C)
        output$observeEvent_out <- renderTable({df})
    })
    
}
shinyApp(ui, server)