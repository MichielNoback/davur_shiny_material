library(shiny)
library(dplyr)
library(ggplot2)
library(here)

#setwd(here())

read_data <- function(data_file) {
    data <- read.table(data_file, sep=",", head = T)
    data$r <- factor(data$r)
    data <- subset(data, select=-ID)
    data
}

data <- read_data("./urine.csv")

ui <- fluidPage(
    titlePanel("Explore variables"),
    sidebarLayout(
        sidebarPanel(
            selectInput("variable_1", label = "variable 1", choices = NULL),
            selectInput("variable_2", label = "variable 2", choices = NULL),
            sliderInput("point_size", 
                        label = "plot character size", 
                        value = 1,
                        min = 0.1, 
                        max = 4)
        ),
        mainPanel(
            plotOutput(outputId = "my_plot")
        )
    )
)

numeric_names <- colnames(data[unlist(lapply(data, FUN = is.numeric))])

server <- function(input, output, session) {
    observeEvent(input$variable_1, {
        updateSelectInput("variable_1", 
                                 choices = numeric_names, 
                                 session = session, 
                                 selected = input$variable_1)
        second_selection <- numeric_names != input$variable_1
        updateSelectInput("variable_2", 
                                 choices = numeric_names[second_selection], 
                                 session = session)
    })
    
    observeEvent(input$variable_2, {
        req(input$variable_1)
        output$my_plot <- shiny::renderPlot({
            ggplot(data = data, 
                   mapping = aes_string(x=input$variable_1, 
                                        y=input$variable_2, 
                                        color = "r")) +
                geom_point(size = input$point_size)
        })
    })
}

shinyApp(ui, server, options=list("launch.browser"=TRUE))
