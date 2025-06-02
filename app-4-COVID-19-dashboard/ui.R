library(shiny)
ui <- fluidPage(
    titlePanel("COVID-19 dashboard for Dutch municipalities"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "cases_deaths_admissions",
                         label = "Input",
                         choices = c("Cases", "Hospital", "Deaths"),
                         selected = "Cases"),
            dateRangeInput(
                inputId = "date_range",
                label = "Date range",
                start = "2020-02-01",
                end = "2021-05-31"
            ),
            selectizeInput(
                inputId = "selected_municipalities", 
                label = "Select Municipalities", 
                choices = NULL, 
                multiple = TRUE),
            checkboxInput(
                inputId = "use_normalized_counts",
                label = "Use normalized counts",
                value = TRUE
            )
        ),
        mainPanel(
            plotOutput("covid_occurrences")
        )
    )
)
