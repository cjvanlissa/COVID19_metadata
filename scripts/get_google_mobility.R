library(textreadr)
get_google_mobility <- function(){
  data <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors = FALSE)
  names(data) <- gsub("_percent_change_from_baseline", "", names(data))
  names(data) <- gsub("_and", "", names(data))
  data$country_region_code <- NULL
  data$date <- as.Date(data$date, format = "%Y-%m-%d")

  data <- pivot_wider(data, id_cols = c("country_region", "sub_region_1", "sub_region_2"), names_from = date, values_from = c(retail_recreation, grocery_pharmacy, parks, transit_stations, workplaces, residential))
  names(data)[1] <- "country"
  data$countryiso3 <- countrycode(data$country, origin = "country.name", destination = "iso3c")
  data$region <- data$sub_region_1
  if(any(data$sub_region_2 == "")){
    data$region[!data$sub_region_2 == ""] <- paste(data$region[!data$sub_region_2 == ""], data$sub_region_2[!data$sub_region_2 == ""], sep = ", ")
  }
  data <- select(data, -sub_region_1, -sub_region_2)
  data <- data[, c(1, (ncol(data)-1), ncol(data), 2:(ncol(data)-2))]
  data$region[data$region == ""] <- NA
  checkfilewrite(data, "google_mobility", "google_mobility.csv")
  return()
  
  
  file_list <- readLines("https://www.google.com/covid19/mobility/")
  file_list <- file_list[grepl("\\.pdf", file_list)]
  file_list <- gsub("^.+?(?=https)", "", file_list, perl = TRUE)
  file_list <- gsub("(?<=pdf).*$", "", file_list, perl = TRUE)
  dates_on_google <- as.Date(gsub("^https://www.gstatic.com/covid19/mobility/(.+?)_.*$", "\\1", file_list), "%Y-%m-%d")
  dates_stored <- as.Date(gsub("^.+?_(.+)\\.csv$", "\\1", list.files(file.path("data", "google_mobility"), "\\.csv")), "%B_%d_%Y")
  get_data <- FALSE
  if(length(dates_stored) == 0){
    get_data <- TRUE
  } else {
    if(max(dates_stored) < max(dates_on_google)){
      get_data <- TRUE
    }
  }
  
  if(get_data){
    files <- lapply(file_list, textreadr::read_pdf)
    
    parsed <- sapply(files, function(lines){
      #lines <- files[[4]]
      lines2 <- lines <- lines$text
      lines <- unlist(strsplit(lines, split = "(?=.[\\+-]\\d{1,2}%)", perl = TRUE))
      lines <- lines[!grepl("* Not enough data for this date:", lines)]
      lines <- unlist(strsplit(lines, split = "(?=Not enough data)", perl = TRUE))
      
      headers <- lines[grep("\\+80%", lines)-1][1:6]
      values <- lines[grep("([\\+-]{0,1}\\d+%|ot enough data)", lines)]
      out1 <- values[!grepl("(80|40)", values)]
      out2 <- values[grep("\\+80%", values)+1][1:6]
      if(!length(out1) == 6){
        out <- out2
      } else {
        out <- out1
      }
      out <- matrix(as.numeric(gsub("%", "", out)), nrow = 6)
      #the_date <- gsub("^.+?(\\b[a-zA-Z]+ \\d+, \\d{4})$", "\\1", head(lines)[2])
      #the_date <- gsub("[ ,]+", "_", the_date)
      #colnames(out) <- paste(trimws(gsub("^(.+?)(\\b[a-zA-Z]+ \\d+, \\d{4})$", "\\1", head(lines)[2])), the_date, sep = ".")
      colnames(out) <- trimws(gsub("^(.+?)(\\b[a-zA-Z]+ \\d+, \\d{4})$", "\\1", head(lines)[2]))
      regional <- values[grepl("(compared to base|ot enough data)", values)]
      if(length(regional) > 0 & length(regional) %% 6 == 0){
        regional <- gsub(".{0,}([\\+\\-]\\d+%|ot enough data).{0,}", "\\1", regional)
        region_headers <- lines[grep("Retail & recreation", lines)-1][-1]
        if(length(region_headers) != length(regional) %/% 6) browser() # Solve problem
        reg_out <- matrix(as.numeric(gsub("%", "", regional)), nrow = 6)
        #colnames(reg_out) <- paste(region_headers, the_date, sep = ".")
        colnames(reg_out) <- region_headers
        out <- cbind(out, reg_out)
      }
      out
    })
    
    output <- data.frame(t(do.call(cbind, parsed)))
    names(output) <- c("retail_recreation", "grocery_pharmacy", "parks", "transit_stations", 
                       "workplaces", "residential")
    output$country <- gsub("\\.", " ", rownames(output))
    output$countryiso3 <- countrycode(output$country, origin = "country.name", destination = "iso3c")
    
    the_date <- gsub("^.+?(\\b[a-zA-Z]+ \\d+, \\d{4})$", "\\1", head(files[[1]]$text)[2])
    the_date <- gsub("[ ,]+", "_", the_date)
    
    checkfilewrite(output, "google_mobility", paste0("mobility_", the_date, ".csv"))
  }
}