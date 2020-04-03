get_file_mobility <- function(){
  file_list <- readLines("https://www.google.com/covid19/mobility/")
  file_list <- file_list[grepl("\\.pdf", file_list)]
  file_list <- gsub("^.+?(?=https)", "", file_list, perl = TRUE)
  file_list <- gsub("(?<=pdf).*$", "", file_list, perl = TRUE)
  
  files <- lapply(file_list, textreadr::read_pdf)
  
  tmp <- sapply(files, function(lines){
    #lines <- files[[2]]
    lines <- lines$text
    headers <- lines[grep("\\+80%", lines)-1]
    values <- lines[grep("(^[\\+-]{0,1}\\d+%|ot enough data)", lines)]
    values1 <- values[!grepl("(80|40)", values)]
    values2 <- values[grep("\\+80%", values)+1]
    if(length(values1) >= length(values2)){
      values <- values1
    } else {
      values <- values2
    }
    if(length(values) > 6){
      if(grepl("ot enough data", values[7])){
        values <- values[1:6]
      } else {
        # Fix a problem
        browser()
      }
    }
    names(values) <- headers
    values
  })
  
  tmp2 <- sapply(tmp, function(x){x[1:18]})
}
