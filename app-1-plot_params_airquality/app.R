#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

airquality <- tidyr::drop_na(airquality, c(Solar.R, Ozone))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Air Quality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "pointsize",
                         label = "Size",
                         choices = c(1, 2, 3, 4))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("airquality")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$airquality <- renderPlot({
        pointsize <- as.integer(input$pointsize)
        print(pointsize)
        ggplot(data = airquality, 
               mapping = aes(x = Solar.R, y = Ozone)) + 
            geom_point(size = pointsize)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
