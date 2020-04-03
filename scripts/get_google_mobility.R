get_file_mobility <- function(){
  file_list <- readLines("https://www.google.com/covid19/mobility/")
  file_list <- file_list[grepl("\\.pdf", file_list)]
  file_list <- gsub("^.+?(?=https)", "", file_list, perl = TRUE)
  file_list <- gsub("(?<=pdf).*$", "", file_list, perl = TRUE)
  
  files <- lapply(file_list, textreadr::read_pdf)
  
  for(lines in files[1:3]){
    #lines <- files[[2]]
    lines <- lines$text
    headers <- lines[grep("\\+80%", lines)-1]
    values <- lines[grep("^[\\+-]{0,1}\\d+%$", lines)]
    values <- values[!grepl("(80|40)", values)]
    if(!length(headers) == length(values)) stop("Couldn't extract headers/values")
  }
  
}
