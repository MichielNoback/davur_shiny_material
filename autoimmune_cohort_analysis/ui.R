#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    
    # Application title
    titlePanel("Autoimmune Cohort analysis"),
    
    # Sidebar with controls  
    sidebarLayout(
        sidebarPanel(
            sliderInput("age_selector",
                        "Age:",
                        min = 18,
                        max = 68,
                        value = c(30,60)
            ),
            checkboxGroupInput("gender_selector", "Gender:",
                               choices=list('Female'='F',
                                            'Male'  ='M'),
                               selected = 'F'
            ),
            selectInput("protein_selector", "Protein:",
                        choices=list('IL-6'='IL6',
                                     'CRP' ='CRP',
                                     'TNFa'='TNF'),
                        selected = 'IL6'
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Boxplot",
                         checkboxInput("show_points","Show Points"),
                         plotOutput("boxPlot")
                ),
                
                tabPanel("Data",
                         DT::dataTableOutput("mydata")
                )
            )
        )
    )
)