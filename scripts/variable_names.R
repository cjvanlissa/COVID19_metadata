library(quanteda)
library(dplyr)

# read file
data.path <- "data/Variable Justification - Full sheet.csv"
read.csv(data.path, header = T) %>%
  select(var, lab) -> dat

# remove header cols
dat <- dat[as.character(dat$var) != "",]
dat <- dat[-(1:8),]

# preparation
rownames(dat) <- dat$var
dat_orig <- dat
dat <- select(dat, lab)

# cone´tent specific pattern
dat <- data.frame(sub(".*Selected Choice(.+)", "\\1", dat$lab, perl = T))

# quanteda anylysis
tmp <- quanteda::corpus(t(dat))
tmp <- quanteda::dfm(tmp, remove = stopwords("english"), remove_punct = TRUE)
tmp <- quanteda::dfm_trim(tmp, min_termfreq = 0, max_docfreq = 15)
tmp <- quanteda::dfm_tfidf(tmp)
tmp <- as.matrix(tmp)
tmp <- data.frame(tmp)

# words to be ignored
tmp <- tmp[!grepl("X[.0-9].*", names(tmp))]
tmp <- tmp[!grepl("[.]", names(tmp))]
ignore_list <- c("happen", "can", "go", "likely", "get", "per")
tmp <- within(tmp, rm(list = ignore_list)) 

# find most relevant words: word 1
top_word <- function(i)
{
  ifelse(max(i)>0, colnames(tmp)[which.max(i)], "XXX")
}
top1 <- apply(tmp, 1, top_word) 

# find most relevant words: word 2
# function for exclusion of found words
reset_max_col_1 <- function(i)
{
  reg_exp <- top1[tail(i, n = 1)]
  if (grepl("[a-zA-Z0-9.]", reg_exp))
  {
    i[grepl(reg_exp, names(i))] <- -1
  }
  i
}

tmp <- data.frame(tmp)
tmp$rownumber <- 1:nrow(tmp)
tmp <- data.frame(t(apply(tmp, 1, reset_max_col_1)))
tmp <- within(tmp, rm("rownumber")) 
top2 <- apply(tmp, 1, top_word)

# find most relevant words: word 3
# function for exclusion of found words
reset_max_col_2 <- function(i)
{
  reg_exp <- top2[tail(i, n = 1)]
  if (grepl("[a-zA-Z0-9.]", reg_exp))
  {
    i[grepl(reg_exp, names(i))] <- -1
  }
  i
}

tmp <- data.frame(tmp)
tmp$rownumber <- 1:nrow(tmp)
tmp <- data.frame(t(apply(tmp, 1, reset_max_col_2)))
tmp <- within(tmp, rm("rownumber")) 
top3 <- apply(tmp, 1, top_word)

# find most relevant words: word 4
# function for exclusion of found words
reset_max_col_3 <- function(i)
{
  reg_exp <- top3[tail(i, n = 1)]
  if (grepl("[a-zA-Z0-9.]", reg_exp))
  {
    i[grepl(reg_exp, names(i))] <- -1
  }
  i
}

tmp <- data.frame(tmp)
tmp$rownumber <- 1:nrow(tmp)
tmp <- data.frame(t(apply(tmp, 1, reset_max_col_3)))
tmp <- within(tmp, rm("rownumber")) 
top4 <- apply(tmp, 1, top_word)

# create variable name from most relevant words
result <- cbind(top1, top2, top3, top4, top5)

# concatenate relevant words
paste_keywords <- function(i)
{
  ret <- ""
  w <- i[1]
  if (w != "XXX") { ret <- w}
  w <- i[2]
  if (w != "XXX") { ret <- paste0(ret, ".", w)}
  w <- i[3]
  if (w != "XXX") { ret <- paste0(ret, ".", w)}
  w <- i[4]
  if (w != "XXX") { ret <- paste0(ret, ".", w)}
#  w <- i[5]
#  if (w != "XXX") { ret <- paste0(ret, ".", w)}

  # remove wrong characters
  ret <- gsub("donâ.t", "", ret)
  ret
}

result <- data.frame(apply(result, 1, paste_keywords))
names(result) <- "lab"
result$lab <- as.character(result$lab)

# eliminate duplicates with various strategies
mark_dupl <- function(i)
{
  w <- i[1]
  ret <- length(result[result$lab == w,]) > 1
  return(ret)
}

result$dupl <- apply(result, 1, mark_dupl)

for (r in 1:nrow(result))
{
  w <- result[r,1]
  
  if (result$dupl[r])
  {
    # startegy 1: identify number ranges
    if (grepl(".*([0-9]+-+[0-9]*).*", dat[r,1])) 
    { 
      result[r,1] <- paste(result[r,1], sub(".*([0-9]+-+[0-9]*).*", "\\1", dat[r,1], perl = T), sep = ".")
    }
    # startegy 2: identify numbers 
    else if (grepl(".*[^0-9]+([0-9]+)[^0-9]+.*", dat[r,1]))
    {
      result[r,1] <- paste(result[r,1], sub(".*[^0-9]+([0-9]+)[^0-9]+.*", "\\1", dat[r,1], perl = T), sep = ".")
    }
    # startegy 3: last words of shorter sentences
    else if (sapply(strsplit(as.character(dat[r,]), " "), length) < 8)
    {
      result[r,1] <- paste(gsub("[^a-z]", "", tolower(tail(strsplit(as.character(dat[r,]), " ")[[1]],4))), collapse = ".")
    }
    # startegy 4: last words of medium sentences
    else if (sapply(strsplit(as.character(dat[r,]), " "), length) < 12)
    {
      result[r,1] <- paste(gsub("[^a-z]", "", tolower(tail(strsplit(as.character(dat[r,]), " ")[[1]],8))), collapse = ".")
    }
    # startegy 5: last words of long sentences
    else
    {
      result[r,1] <- paste(gsub("[^a-z]", "", tolower(tail(strsplit(as.character(dat[r,]), " ")[[1]],3))), collapse = ".")
    }
  }
  
  # missing value: Use description
  if (w == "")
  {
    result[r,1] <- paste(gsub("[^a-z]", "", tolower(tail(strsplit(str_trim(as.character(dat[r,1])), " ")[[1]],2))), collapse = ".")
  }
  
  # clear string
  result[r,1] <- gsub("^[.]", "", result[r,1], perl = T)
  result[r,1] <- gsub("[.][.]+", ".", result[r,1], perl = T)
}

# eliminate remaining duplicates with additional strategy
result <- select(result, lab)
result$dupl <- apply(result, 1, mark_dupl)
rownames(dat) <- rownames(dat_orig)

# startegy 6: last word of long sentences
for (r in 1:nrow(result))
{
  w <- result[r,1]
  
  if (result$dupl[r])
  {
    result[r,1] <- paste(w, gsub("[^a-z]", "", tolower(tail(strsplit(as.character(dat[r,]), " ")[[1]],1))), sep=".")
  }

  # clear string
  result[r,1] <- gsub("^[.]", "", result[r,1])
  result[r,1] <- gsub("[.][.]+", ".", result[r,1])
}

# eliminate remaining duplicates with additional strategy
result <- select(result, lab)
result$dupl <- apply(result, 1, mark_dupl)
rownames(dat) <- rownames(dat_orig)

# startegy 7: append original name
for (r in 1:nrow(result))
{
  w <- result[r,1]

  if (result$dupl[r])
  {
    result[r,1] <- paste(w, rownames(dat)[r], sep=".")
  }
}

result <- select(result, lab)
rownames(result) <- rownames(dat_orig)

write.csv(result, "variable_names.csv")
