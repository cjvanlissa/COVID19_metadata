##########

########## TO DO ##########

# TODO: 

########## Packages ##########
library(dplyr)
library(countrycode)
library(tidyr)

########## FUNCTIONS ##########

# from PsyCorona-DataCleaning
merge_file_by_countryiso3 <- function(df, file_path){
  # ISO3 country code (incl XKV for Kosovo)
  if(is.null(df[["countryiso3"]])){
    df$countryiso3 <- ifelse(df$coded_country == "Kosovo", 
                             "XKV", 
                             countrycode::countrycode(df$coded_country, origin = "country.name", destination = "iso3c", warn = FALSE))
  }

  df_dat <- read.csv(file_path, stringsAsFactors = FALSE)
  # Add XKV as current Kosovo ISO3 code 
  df_dat[which(df_dat$country=="Kosovo"),]$countryiso3 <- "XKV"
  df_dat <- df_dat[!is.na(df_dat$countryiso3), ]
  if(!all(df$countryiso3 %in% df_dat$countryiso3)){
    message("Not all countries in df are available in ", file_path, ".")
  }
  if(any(duplicated(df_dat$countryiso3))){
    message("There are some duplicated country names in ", file_path, ".")
    if(!is.null(df_dat[["region"]])){
      message("Dropping region to see if this solves the problem.")
      df_dat <- df_dat[is.na(df_dat$region), ]
    }
    if(any(duplicated(df_dat$countryiso3))){
      message("There are still duplicated country names in ", file_path, 
              ". Using the row with most available data. Manual merging might be required. The duplicates are:\n  ", 
              paste0(df_dat$countryiso3[which(duplicated(df_dat$countryiso3))], collapse = "\n  "))
      the_dups <- which(duplicated(df_dat$countryiso3) | duplicated(df_dat$countryiso3, fromLast = TRUE))
      df_dups <- df_dat[the_dups, ]
      df_dat <- df_dat[-the_dups, ]
      df_dups$miss <- rowSums(is.na(df_dups))
      for(countr in unique(df_dups$countryiso3)){
        df_dups <- df_dups[-which(df_dups$countryiso3 == countr)[-which.min(df_dups$miss[df_dups$countryiso3 == countr])], ]
      }
      df_dat <- rbind(df_dat, df_dups[, -ncol(df_dups)])
      message("For duplicates, used row with most complete information.")
    }
  }
  merge(df, df_dat, all.x = TRUE, by = "countryiso3")
}

calc_dayno <- function(posixdate){
  as.integer(as.numeric(posixdate - as.POSIXct("2020-01-01 00:00:00")))
}

########## PREPARATION ##########

data_path <- "data"
input_filename <- "df_train_imputed.csv"
#input_filename <- "df_train_imputed_small.csv"
output_filename <- "df_train_imputed_merged.csv"
df <- read.csv(paste(data_path, input_filename, sep = "/"), stringsAsFactors = FALSE)
names(df)[1]<-"responseid" # fix utf-8 issue

# get startdate and responseid 
master_filename <- "RMD30_Caspar van Lissa_2020-05-21 15-21 CEST.csv"
read.csv(paste(data_path, master_filename, sep = "/"), stringsAsFactors = FALSE) %>%
  select(StartDate,ResponseId) -> df_date
names(df_date) <- tolower(names(df_date)) 

# add dayresponse variable
df <- merge(df, df_date, by = "responseid", all = FALSE, sort = FALSE)
df[which(names(df) == "startdate.x")] <- NULL
df$dayresponse <- calc_dayno(as.POSIXct(df$startdate))
df$startdate <- NULL
  
########## MERGE DATA ##########

########## CSSE ##########

file_path <- paste(data_path, "CSSE/CSSE.csv", sep = "/")
merge_file_by_countryiso3(df, file_path) -> df

# replace date variables names with dayno variable names
for (col in names(df)){
  if (isTRUE(grep("[0-9][0-9]_[0-9][0-9]_[0-9][0-9][0-9][0-9]", col)==1)){
    dayno <- calc_dayno(as.POSIXct(gsub("[^.]*[.]([0-9][0-9])_([0-9][0-9])_([0-9][0-9][0-9][0-9])", 
                                        "\\3/\\1/\\2", col, 
                                        perl=TRUE)))
    col_type <- gsub("([^.]*)[.].*", "\\1", col, perl=TRUE)
    new_col <- paste(col_type, dayno, sep =".")
    colnames(df)[which(names(df) == col)] <- paste(new_col, sep=".")
  }
}

for (row in 1:nrow(df)) {
  # add variables "recovered.date" columns with "recovered.00", "recovered.01", "recovered.02", "recovered.07", "recovered.15", 
  #               "recovered.mean.1508", "recovered.mean.0700",
  #               "recovered.delta.0001", "recovered.delta.0102"
  #               "recovered.delta.mean0715"
  dayresponse.00 <- df[row, "dayresponse"]
  recovered.00 <- df[row, paste("recovered", dayresponse.00, sep=".")]
  dayresponse.01 <- dayresponse.00 - 1
  recovered.01 <- df[row, paste("recovered", dayresponse.01, sep=".")]
  dayresponse.02 <- dayresponse.00 - 2
  recovered.02 <- df[row, paste("recovered", dayresponse.02, sep=".")]
  dayresponse.03 <- dayresponse.00 - 3
  recovered.03 <- df[row, paste("recovered", dayresponse.03, sep=".")]
  dayresponse.04 <- dayresponse.00 - 4
  recovered.04 <- df[row, paste("recovered", dayresponse.04, sep=".")]
  dayresponse.05 <- dayresponse.00 - 5
  recovered.05 <- df[row, paste("recovered", dayresponse.05, sep=".")]
  dayresponse.06 <- dayresponse.00 - 6
  recovered.06 <- df[row, paste("recovered", dayresponse.06, sep=".")]
  dayresponse.07 <- dayresponse.00 - 7
  recovered.07 <- df[row, paste("recovered", dayresponse.07, sep=".")]
  dayresponse.08 <- dayresponse.00 - 8
  recovered.08 <- df[row, paste("recovered", dayresponse.08, sep=".")]
  dayresponse.09 <- dayresponse.00 - 9
  recovered.09 <- df[row, paste("recovered", dayresponse.09, sep=".")]
  dayresponse.10 <- dayresponse.00 - 10
  recovered.10 <- df[row, paste("recovered", dayresponse.10, sep=".")]
  dayresponse.11 <- dayresponse.00 - 11
  recovered.11 <- df[row, paste("recovered", dayresponse.11, sep=".")]
  dayresponse.12 <- dayresponse.00 - 12
  recovered.12 <- df[row, paste("recovered", dayresponse.12, sep=".")]
  dayresponse.13 <- dayresponse.00 - 13
  recovered.13 <- df[row, paste("recovered", dayresponse.13, sep=".")]
  dayresponse.14 <- dayresponse.00 - 14
  recovered.14 <- df[row, paste("recovered", dayresponse.14, sep=".")]
  dayresponse.15 <- dayresponse.00 - 15
  recovered.15 <- df[row, paste("recovered", dayresponse.15, sep=".")]

  c <- na.omit(recovered.15, recovered.14, recovered.13, recovered.12, recovered.11, recovered.10, recovered.09, 
        recovered.08)
  if (!is.null(c)){
    recovered.mean.1508 <- mean(c, na.rm = TRUE)
  } else {
    recovered.mean.1508 <- NA
  }
  c <- na.omit(recovered.07, recovered.06, recovered.05, recovered.04, recovered.03, recovered.02, recovered.01,
        recovered.00)
  if (!is.null(c)){
    recovered.mean.0700 <- mean(c, na.rm = TRUE)
  } else {
    recovered.mean.0700 <- NA
  }
  
  recovered.delta.0001 <- recovered.00 - recovered.01
  recovered.delta.mean.0715 <- recovered.mean.0700 - recovered.mean.1508
  
  if (is.null(recovered.00) | length(recovered.00) == 0){ recovered.00 <- NA }
  df[row, "recovered.00"] <- recovered.00
  if (is.null(recovered.01) | length(recovered.01) == 0){ recovered.01 <- NA }
  df[row, "recovered.01"] <- recovered.01
  if (is.null(recovered.02) | length(recovered.02) == 0){ recovered.02 <- NA }
  df[row, "recovered.02"] <- recovered.02
  if (is.null(recovered.07) | length(recovered.07) == 0){ recovered.07 <- NA }
  df[row, "recovered.07"] <- recovered.07
  if (is.null(recovered.15) | length(recovered.15) == 0){ recovered.15 <- NA }
  df[row, "recovered.15"] <- recovered.15
  if (is.null(recovered.mean.1508) | length(recovered.mean.1508) == 0){ recovered.mean.1508 <- NA }
  df[row, "recovered.mean.1508"] <- recovered.mean.1508
  if (is.null(recovered.mean.0700) | length(recovered.mean.0700) == 0){ recovered.mean.0700 <- NA }
  df[row, "recovered.mean.0700"] <- recovered.mean.0700
  if (is.null(recovered.delta.0001) | length(recovered.delta.0001) == 0){ recovered.delta.0001 <- NA }
  df[row, "recovered.delta.0001"] <- recovered.delta.0001
  if (is.null(recovered.delta.mean.0715) | length(recovered.delta.mean.0715) == 0){ recovered.delta.mean.0715 <- NA }
  df[row, "recovered.delta.mean.0715"] <- recovered.delta.mean.0715
  
  # add variables "confirmed.date" columns with "confirmed.00", "confirmed.01", "confirmed.02", "confirmed.07", "confirmed.15", 
  #               "confirmed.mean.1508", "confirmed.mean.0700",
  #               "confirmed.delta.0001", "confirmed.delta.0102"
  #               "confirmed.delta.mean0715"
  dayresponse.00 <- df[row, "dayresponse"]
  confirmed.00 <- df[row, paste("confirmed", dayresponse.00, sep=".")]
  dayresponse.01 <- dayresponse.00 - 1
  confirmed.01 <- df[row, paste("confirmed", dayresponse.01, sep=".")]
  dayresponse.02 <- dayresponse.00 - 2
  confirmed.02 <- df[row, paste("confirmed", dayresponse.02, sep=".")]
  dayresponse.03 <- dayresponse.00 - 3
  confirmed.03 <- df[row, paste("confirmed", dayresponse.03, sep=".")]
  dayresponse.04 <- dayresponse.00 - 4
  confirmed.04 <- df[row, paste("confirmed", dayresponse.04, sep=".")]
  dayresponse.05 <- dayresponse.00 - 5
  confirmed.05 <- df[row, paste("confirmed", dayresponse.05, sep=".")]
  dayresponse.06 <- dayresponse.00 - 6
  confirmed.06 <- df[row, paste("confirmed", dayresponse.06, sep=".")]
  dayresponse.07 <- dayresponse.00 - 7
  confirmed.07 <- df[row, paste("confirmed", dayresponse.07, sep=".")]
  dayresponse.08 <- dayresponse.00 - 8
  confirmed.08 <- df[row, paste("confirmed", dayresponse.08, sep=".")]
  dayresponse.09 <- dayresponse.00 - 9
  confirmed.09 <- df[row, paste("confirmed", dayresponse.09, sep=".")]
  dayresponse.10 <- dayresponse.00 - 10
  confirmed.10 <- df[row, paste("confirmed", dayresponse.10, sep=".")]
  dayresponse.11 <- dayresponse.00 - 11
  confirmed.11 <- df[row, paste("confirmed", dayresponse.11, sep=".")]
  dayresponse.12 <- dayresponse.00 - 12
  confirmed.12 <- df[row, paste("confirmed", dayresponse.12, sep=".")]
  dayresponse.13 <- dayresponse.00 - 13
  confirmed.13 <- df[row, paste("confirmed", dayresponse.13, sep=".")]
  dayresponse.14 <- dayresponse.00 - 14
  confirmed.14 <- df[row, paste("confirmed", dayresponse.14, sep=".")]
  dayresponse.15 <- dayresponse.00 - 15
  confirmed.15 <- df[row, paste("confirmed", dayresponse.15, sep=".")]

  c <- na.omit(confirmed.15, confirmed.14, confirmed.13, confirmed.12, confirmed.11, confirmed.10, confirmed.09, 
               confirmed.08)
  if (!is.null(c)){
    confirmed.mean.1508 <- mean(c, na.rm = TRUE)
  } else {
    confirmed.mean.1508 <- NA
  }
  c <- na.omit(confirmed.07, confirmed.06, confirmed.05, confirmed.04, confirmed.03, confirmed.02, confirmed.01,
               confirmed.00)
  if (!is.null(c)){
    confirmed.mean.0700 <- mean(c, na.rm = TRUE)
  } else {
    confirmed.mean.0700 <- NA
  }

    confirmed.delta.0001 <- confirmed.00 - confirmed.01
  confirmed.delta.mean.0715 <- confirmed.mean.0700 - confirmed.mean.1508

  if (is.null(confirmed.00) | length(confirmed.00) == 0){ confirmed.00 <- NA }
  df[row, "confirmed.00"] <- confirmed.00
  if (is.null(confirmed.01) | length(confirmed.01) == 0){ confirmed.01 <- NA }
  df[row, "confirmed.01"] <- confirmed.01
  if (is.null(confirmed.02) | length(confirmed.02) == 0){ confirmed.02 <- NA }
  df[row, "confirmed.02"] <- confirmed.02
  if (is.null(confirmed.07) | length(confirmed.07) == 0){ confirmed.07 <- NA }
  df[row, "confirmed.07"] <- confirmed.07
  if (is.null(confirmed.15) | length(confirmed.15) == 0){ confirmed.15 <- NA }
  df[row, "confirmed.15"] <- confirmed.15
  if (is.null(confirmed.mean.1508) | length(confirmed.mean.1508) == 0){ confirmed.mean.1508 <- NA }
  df[row, "confirmed.mean.1508"] <- confirmed.mean.1508
  if (is.null(confirmed.mean.0700) | length(confirmed.mean.0700) == 0){ confirmed.mean.0700 <- NA }
  df[row, "confirmed.mean.0700"] <- confirmed.mean.0700
  if (is.null(confirmed.delta.0001) | length(confirmed.delta.0001) == 0){ confirmed.delta.0001 <- NA }
  df[row, "confirmed.delta.0001"] <- confirmed.delta.0001
  if (is.null(confirmed.delta.mean.0715) | length(confirmed.delta.mean.0715) == 0){ confirmed.delta.mean.0715 <- NA }
  df[row, "confirmed.delta.mean.0715"] <- confirmed.delta.mean.0715
  
  
  # add variables "deaths.date" columns with "deaths.00", "deaths.01", "deaths.02", "deaths.07", "deaths.15", 
  #               "deaths.mean.1508", "deaths.mean.0700",
  #               "deaths.delta.0001", "deaths.delta.0102"
  #               "deaths.delta.mean0715"
  dayresponse.00 <- df[row, "dayresponse"]
  deaths.00 <- df[row, paste("deaths", dayresponse.00, sep=".")]
  dayresponse.01 <- dayresponse.00 - 1
  deaths.01 <- df[row, paste("deaths", dayresponse.01, sep=".")]
  dayresponse.02 <- dayresponse.00 - 2
  deaths.02 <- df[row, paste("deaths", dayresponse.02, sep=".")]
  dayresponse.03 <- dayresponse.00 - 3
  deaths.03 <- df[row, paste("deaths", dayresponse.03, sep=".")]
  dayresponse.04 <- dayresponse.00 - 4
  deaths.04 <- df[row, paste("deaths", dayresponse.04, sep=".")]
  dayresponse.05 <- dayresponse.00 - 5
  deaths.05 <- df[row, paste("deaths", dayresponse.05, sep=".")]
  dayresponse.06 <- dayresponse.00 - 6
  deaths.06 <- df[row, paste("deaths", dayresponse.06, sep=".")]
  dayresponse.07 <- dayresponse.00 - 7
  deaths.07 <- df[row, paste("deaths", dayresponse.07, sep=".")]
  dayresponse.08 <- dayresponse.00 - 8
  deaths.08 <- df[row, paste("deaths", dayresponse.08, sep=".")]
  dayresponse.09 <- dayresponse.00 - 9
  deaths.09 <- df[row, paste("deaths", dayresponse.09, sep=".")]
  dayresponse.10 <- dayresponse.00 - 10
  deaths.10 <- df[row, paste("deaths", dayresponse.10, sep=".")]
  dayresponse.11 <- dayresponse.00 - 11
  deaths.11 <- df[row, paste("deaths", dayresponse.11, sep=".")]
  dayresponse.12 <- dayresponse.00 - 12
  deaths.12 <- df[row, paste("deaths", dayresponse.12, sep=".")]
  dayresponse.13 <- dayresponse.00 - 13
  deaths.13 <- df[row, paste("deaths", dayresponse.13, sep=".")]
  dayresponse.14 <- dayresponse.00 - 14
  deaths.14 <- df[row, paste("deaths", dayresponse.14, sep=".")]
  dayresponse.15 <- dayresponse.00 - 15
  deaths.15 <- df[row, paste("deaths", dayresponse.15, sep=".")]

  if (length(deaths.00) == 0) { deaths.00 <- NA }
  if (length(deaths.01) == 0) { deaths.01 <- NA }
  if (length(deaths.02) == 0) { deaths.02 <- NA }
  if (length(deaths.03) == 0) { deaths.03 <- NA }
  if (length(deaths.04) == 0) { deaths.04 <- NA }
  if (length(deaths.05) == 0) { deaths.05 <- NA }
  if (length(deaths.06) == 0) { deaths.06 <- NA }
  if (length(deaths.07) == 0) { deaths.07 <- NA }
  if (length(deaths.08) == 0) { deaths.08 <- NA }
  if (length(deaths.09) == 0) { deaths.09 <- NA }
  if (length(deaths.10) == 0) { deaths.10 <- NA }
  if (length(deaths.11) == 0) { deaths.11 <- NA }
  if (length(deaths.12) == 0) { deaths.12 <- NA }
  if (length(deaths.13) == 0) { deaths.13 <- NA }
  if (length(deaths.14) == 0) { deaths.14 <- NA }
  if (length(deaths.15) == 0) { deaths.15 <- NA }
  
  c <- na.omit(deaths.15, deaths.14, deaths.13, deaths.12, deaths.11, deaths.10, deaths.09, 
               deaths.08)
  if (!is.null(c)){
    deaths.mean.1508 <- mean(c, na.rm = TRUE)
  } else {
    deaths.mean.1508 <- NA
  }
  c <- na.omit(deaths.07, deaths.06, deaths.05, deaths.04, deaths.03, deaths.02, deaths.01,
               deaths.00)
  if (!is.null(c)){
    deaths.mean.0700 <- mean(c, na.rm = TRUE)
  } else {
    deaths.mean.0700 <- NA
  }

    deaths.delta.0001 <- deaths.00 - deaths.01
  deaths.delta.mean.0715 <- deaths.mean.0700 - deaths.mean.1508

  if (is.null(deaths.00) | length(deaths.00) == 0){ deaths.00 <- NA }
  df[row, "deaths.00"] <- deaths.00
  if (is.null(deaths.01) | length(deaths.01) == 0){ deaths.01 <- NA }
  df[row, "deaths.01"] <- deaths.01
  if (is.null(deaths.02) | length(deaths.02) == 0){ deaths.02 <- NA }
  df[row, "deaths.02"] <- deaths.02
  if (is.null(deaths.07) | length(deaths.07) == 0){ deaths.07 <- NA }
  df[row, "deaths.07"] <- deaths.07
  if (is.null(deaths.15) | length(deaths.15) == 0){ deaths.15 <- NA }
  df[row, "deaths.15"] <- deaths.15
  if (is.null(deaths.mean.1508) | length(deaths.mean.1508) == 0){ deaths.mean.1508 <- NA }
  df[row, "deaths.mean.1508"] <- deaths.mean.1508
  if (is.null(deaths.mean.0700) | length(deaths.mean.0700) == 0){ deaths.mean.0700 <- NA }
  df[row, "deaths.mean.0700"] <- deaths.mean.0700
  if (is.null(deaths.delta.0001) | length(deaths.delta.0001) == 0){ deaths.delta.0001 <- NA }
  df[row, "deaths.delta.0001"] <- deaths.delta.0001
  if (is.null(deaths.delta.mean.0715) | length(deaths.delta.mean.0715) == 0){ deaths.delta.mean.0715 <- NA }
  df[row, "deaths.delta.mean.0715"] <- deaths.delta.mean.0715
}  

# remove date variables
df <- df[,!grepl("^recovered[.][0-9]+", names(df))]
df <- df[,!grepl("^confirmed[.][0-9]+", names(df))]
df <- df[,!grepl("^deaths[.][0-9]+", names(df))]

write.csv(df, paste(data_path, output_filename, sep = "/"), row.names = FALSE)
