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

data <- read_data("urine.csv")

ui <- fluidPage(
    titlePanel("Explore point size"),
    sidebarLayout(
        sidebarPanel(
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


server <- function(input, output, session) {
        output$my_plot <- shiny::renderPlot({
            ggplot(data = data,
                   mapping = aes(gravity,
                                 osmo,
                                 color = r)) +
                geom_point(size = input$point_size)
        })
}

shinyApp(ui, server, options=list("launch.browser"=TRUE))



