library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#' Downloads a new copy of the covid data
#'
download_new_covid_data <- function(
        local_file = "COVID-19_aantallen_gemeente_per_dag.csv",
        local_folder = "data/",
        url = paste0("https://data.rivm.nl/covid-19/", local_file)
    ) {
    download.file(url = url, destfile = paste0(local_folder, "/", local_file))
}

#' Loads and combines covid and population data
#'
load_data <- function(
        covid_data_file = "data/COVID-19_aantallen_gemeente_per_dag.csv",
        population_data_file = "data/Bevolking__geslacht__leeftijd__regio_21042021_115647.csv"
    ) {
    #covid data
    covid_data <- suppressMessages(read_delim(file = covid_data_file, delim = ";"))

    #population data
    pop_data_raw <- suppressMessages(read_delim(file = population_data_file, delim = ";"))
    #change colnames to remove spaces
    tmp_colnames <- colnames(pop_data_raw)
    tmp_colnames[3] <- "Burgerlijke_staat" # replaces 'Burgerlijke staat'
    tmp_colnames[5] <- "Regios"     #replaces Regio's
    tmp_colnames[6] <- "Bevolking"  #replaces 'Bevolking op 1 januari (aantal)'
    colnames(pop_data_raw) <- tmp_colnames

    pop_data <- pop_data_raw %>%
        filter(Perioden == 2020) %>%
        select(Regios, Bevolking) %>% 
        drop_na()
    
    #correct on name differences
    pop_data <- pop_data %>% 
    mutate(Regios = ifelse(Regios == "Groningen (gemeente)", "Groningen", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Utrecht (gemeente)", "Utrecht", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Laren (NH.)", "Laren", Regios)) %>%
    mutate(Regios = ifelse(Regios == "'s-Gravenhage (gemeente)", "'s-Gravenhage", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Rijswijk (ZH.)", "Rijswijk", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Middelburg (Z.)", "Middelburg", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Beek (L.)", "Beek", Regios)) %>%
    mutate(Regios = ifelse(Regios == "Stein (L.)", "Stein", Regios))
    
    all_data <- covid_data %>% 
        left_join(pop_data, by = c("Municipality_name" = "Regios"))
    
    #filter(!is.na(Bevolking))
    
    
    
    
    
    all_data 
}

#' Adds columns with normalized covid counts
#' 
#' @param data the tibble containing covid and population data
#' @param per the number to normalize on
#' 
normalize_covid_counts <- function(data, per = 100000) {
    normalize_counts <- function(x, normalizer) {(x / normalizer) * per}
    data %>% 
        mutate(Total_norm = normalize_counts(Total_reported, Bevolking)) %>%
        mutate(Hospital_norm = normalize_counts(Hospital_admission, Bevolking)) %>%
        mutate(Deceased_norm = normalize_counts(Deceased, Bevolking))
}

#' A general-purpose data filtering function
#' 
#' @param data the central dataframe
#' @param municipalities the cities to select
#' @param date_from the lower date boundary which defaults to the lowest in the data set
#' @param date_to the upper date boundary which defaults to the highest in the data set
filter_data <- function(data, 
                        municipalities,
                        date_from = min(data$Date_of_publication), 
                        date_to = max(data$Date_of_publication)) {
    data %>%
        filter(Municipality_name %in% municipalities &
               Date_of_publication >= date_from &
               Date_of_publication <= date_to)
}

merge_ROAZ_region_counts <- function(data) {
    data %>% 
        group_by(Date_of_publication, Municipality_name) %>%
        mutate(Total_reported = sum(Total_reported),
               Hospital_admission = sum(Hospital_admission),
               Deceased = sum(Deceased)) %>% 
        distinct(Date_of_publication, .keep_all = T) %>%
        ungroup()
}


#' Creates an area plot after selection of municipalities data in date range
#' 
#' @param data the central dataframe
#' @param municipalities the cities to select
#' @param date_from the lower date boundary which defaults to the lowest in the data set
#' @param date_to the upper date boundary which defaults to the highest in the data set
area_plot <- function(data, 
                      municipalities,
                      date_from = min(data$Date_of_publication), 
                      date_to = max(data$Date_of_publication),
                      normalized = FALSE,
                      type = "Cases"
    ) {
    selected_data <- filter_data(data, municipalities, date_from, date_to)
    
    if (type == "Cases") {
        y_axis_label <- "Cases"
        if (normalized) {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Total_norm))
            y_axis_label <- paste0(y_axis_label, " per 100,000")
        } else {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Total_reported))
        }
    } else if(type == "Deaths") {
        y_axis_label <- "Deaths"
        if (normalized) {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Deceased_norm))
            y_axis_label <- paste0(y_axis_label, " per 100,000")
        } else {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Deceased))
        }
    } else { 
        y_axis_label <- "Hospital admissions"
        if (normalized) {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Hospital_norm))
            y_axis_label <- paste0(y_axis_label, " per 100,000")
        } else {
            p <- ggplot(selected_data, aes(x = Date_of_publication, y = Hospital_admission))
        }
    }
    p + 
        geom_area(aes(color = Municipality_name, fill = Municipality_name),
               alpha = 0.8) +
        facet_wrap(. ~ Municipality_name, ncol = 1) +
        labs(x = "Date", y = y_axis_label, fill = "City", color = "City") +
        theme_minimal() + 
        theme(legend.position = "none")
}


#' Creates a smoothed plot after selection of municipalities data in date range
#' 
#' @param data the central dataframe
#' @param municipalities the cities to select
#' @param date_from the lower date boundary which defaults to the lowest in the data set
#' @param date_to the upper date boundary which defaults to the highest in the data set
smoothed_plot <- function(data, 
                         municipalities,
                         date_from = min(data$Date_of_publication), 
                         date_to = max(data$Date_of_publication),
                         loess_span = 0.05
                         
    ) {
    selected_data <- filter_data(data, municipalities, date_from, date_to)
    selected_data <- selected_data %>%
        group_by(Municipality_name) %>%
        mutate(loess_predict = predict(loess(Total_reported ~
                                      as.numeric(Date_of_publication),
                                  span = loess_span)))

    ggplot(selected_data, aes(x = Date_of_publication, y = Total_reported)) + 
        geom_smooth(aes(color = Municipality_name, 
                        fill = Municipality_name),
                    se = FALSE, 
                    span = loess_span, 
                    method = "loess", 
                    formula = y ~ x) +
        geom_ribbon(aes(ymin = 0, 
                        ymax = loess_predict, 
                        fill = Municipality_name),
                    alpha = 0.3) +
        facet_wrap(. ~ Municipality_name, ncol = 1) +
        labs(x = "date", y = "cases", fill = "City", color = "City") +
        theme_minimal() + 
        theme(
              strip.text.x = element_blank()
        ) 
}
