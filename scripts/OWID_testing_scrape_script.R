library(rvest)
library(tidyverse)
library(countrycode)

# Script with a function to scrape the corona tests table from "Our World in Data". 
# For Australia and Canada there are records by region too, which I have kept just in case. 


scrape_tests_data <- function(html_adress = "https://ourworldindata.org/coronavirus-testing-source-data") {
  
  # read the html 
  testin_facilities_html <- xml2::read_html(html_adress)
  
  # scrape the table
  raw_table <- testin_facilities_html %>%
    html_node("table") %>%
    html_table() 
  
  # turn the scraped table into a tibble
  tibble_table <-  tibble("country" = raw_table$X1, 
         "total_tests" = raw_table$X2, 
         "date" = raw_table$X3,
         "source" = raw_table$X4,
         "source_date" = raw_table$X5,
         "remarks" = raw_table$X6)
  
  # remove the top row with the previous column names
  tibble_table_only_data <- tibble_table[-1, ]
  
  tibble_table_iso3s = add_column(tibble_table_only_data,
                                  countryiso3 = countrycode(tibble_table_only_data$country,"country.name","iso3c"), 
                                  .after = "country")
  # turn the total_tests column into numeric 
  tibble_table_iso3s$total_tests <- as.numeric(gsub(",","",tibble_table_iso3s$total_tests ))
  
  return(tibble_table_iso3s)

}


# Scrape data
scraped_data <- scrape_tests_data("https://ourworldindata.org/coronavirus-testing-source-data")

# Write the data
write_csv(scraped_data, "data/OWID_Tests/OurWorldInData_Tests.csv")
  









