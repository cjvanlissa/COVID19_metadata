within_time_frame_CSSE <- function(time = NULL) {
  
  if(is.null(time)) stop("Argument 'time' must be specified!")
  
  csse <- read.csv("data/CSSE/CSSE.csv", stringsAsFactors = FALSE)
  csse <- csse[!is.na(csse$countryiso3), ]
  csse <- csse[is.na(csse$region), ]
  csse[c("country", "region")] <- NULL
  the_dups <- which(duplicated(csse$countryiso3) | duplicated(csse$countryiso3, fromLast = TRUE))
  df_dups <- csse[the_dups, ]
  csse <- csse[-the_dups, ]
  df_dups$miss <- rowSums(is.na(df_dups))
  for(countr in unique(df_dups$countryiso3)){
    df_dups <- df_dups[-which(df_dups$countryiso3 == countr)[-which.min(df_dups$miss[df_dups$countryiso3 == countr])], ]
  }
  csse <- rbind(csse, df_dups[, -ncol(df_dups)])
  time_frame_CSSE <- csse %>%
    pivot_longer(names_to = "var", values_to = "value", -c("countryiso3")) %>%
    mutate(date = str_sub(var, -10),
           date = as.Date(paste(str_sub(date, -4), 
                                str_sub(date, 1, 2), 
                                str_sub(date, 4, 5), sep = "-")),
           code = ifelse(str_detect(var, "confirmed"), "cases", 
                         ifelse(str_detect(var, "deaths"), "deaths", "recovered"))) %>%
    pivot_wider(id_cols = c("countryiso3", "date"), names_from = code, values_from = value) %>%
    group_by(countryiso3) %>%
    summarise(cases_within_time = max(c(0, last(cases[date <= first(date[cases > 0 & !is.na(cases)]) + time & !is.na(date) & !is.na(cases)]))),
              deaths_within_time = max(c(0, last(deaths[date <= first(date[deaths > 0 & !is.na(deaths)]) + time & !is.na(date) & !is.na(deaths)]))),
              recovered_within_time = max(c(0, last(recovered[date <= first(date[recovered > 0 & !is.na(recovered)]) + time & !is.na(date) & !is.na(recovered)]))))
  
  return(time_frame_CSSE)
}

within_14_csse <- within_time_frame_CSSE(14)

checkfilewrite(within_14_csse, "CSSE", "csse_within_14.csv")