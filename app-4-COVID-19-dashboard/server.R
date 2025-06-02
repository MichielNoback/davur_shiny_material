library(shiny)

source("R/dashboard_utilities.R")
all_data <- load_data()
all_data <- merge_ROAZ_region_counts(all_data)
all_data <- normalize_covid_counts(all_data)

server <- function(input, output, session) {
    updateSelectizeInput(session,
                         inputId = 'selected_municipalities', 
                         choices = sort(unique(all_data$Municipality_name)), 
                         server = TRUE)
    observe({
        req(input$selected_municipalities)
        output$covid_occurrences <- renderPlot({
            area_plot(data = all_data,
                      municipalities = input$selected_municipalities,
                      date_from = input$date_range[1],
                      date_to = input$date_range[2],
                      normalized = input$use_normalized_counts,
                      type = input$cases_deaths_admissions)
        })
    })
}
