#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

generate_data <- function(cohort_size){
    nrow <- cohort_size
    mydata <- tibble("Subject"   = paste0("S",1:nrow),
                     "Age"       = sample(18:68, size = nrow, replace=TRUE),
                     "Diagnosis" = sample(c("RA","SLE","IBD"), size = nrow, replace=TRUE),
                     "CRP"       = sample(seq(5,10, length.out=50),   size = nrow, replace=TRUE),
                     "TNF"       = rnorm(mean = 4, sd = 0.8, n = nrow),
                     "Gender"    = sample(c('M','F'), size = nrow, replace=TRUE)
    ) %>%  # Age-dependent IL-6
        mutate("IL6" = Age / rnorm(mean = 2, sd = 0.4, n = nrow))
    # Diseases-dependent CRP
    mydata$CRP[mydata$Diagnosis =='RA'] <-  mydata$CRP[mydata$Diagnosis =='RA'] /2                
    mydata
}
mydata <- generate_data(1000)

filter_data <- function(x, age, gender){
    filtered <- x %>% filter(Age > age[1],
                             Age < age[2], 
                             Gender %in% gender)
}


# Define server logic required to draw a histogram
function(input, output, session) {
    output$boxPlot <- renderPlot({
        # Make a selection based on the age selector; function filter_data is a custom one
        tp <- filter_data(mydata, input$age_selector, input$gender_selector) %>% 
            rename(readout = input$protein_selector)
        
        p <- ggplot(tp, aes(x = Diagnosis,  
                            y = readout, 
                            fill = Gender)) 
        
        if (input$show_violin == TRUE) {
            p <- p + geom_violin()
        } else {
            p <- p + geom_boxplot()
        }
        
        if (input$show_points == TRUE) {
            p <- p + geom_jitter(width=0.2, color="darkred", alpha=0.7) 
        }
        p <- p + 
            theme_bw() + 
            labs(y = input$protein_selector)
        p
    })
    
    output$mydata <- DT::renderDataTable({
        # Make a selection based on the age selector
        tp <- filter_data(mydata, input$age_selector, input$gender_selector)
        print(tp)
        DT::datatable(tp)
    })

}
