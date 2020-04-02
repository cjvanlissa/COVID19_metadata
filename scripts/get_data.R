library(gert)
library(rio)
library(readxl)

get_csse <- function(){
  olddir <- getwd()
  git_dir <- file.path(tempdir(), 'CSSE')
  gert::git_clone("https://github.com/CSSEGISandData/COVID-19.git", git_dir)
  setwd(file.path(git_dir, "csse_covid_19_data", "csse_covid_19_daily_reports"))
  olddir <- "C:/Git_Repositories/psycorona/PsyCorona_phase3"
  f <- list.files(pattern = "csv")
  file.copy(from = f, file.path(olddir, "data", "CSSE", f))
  setwd(olddir)
  unlink(git_dir, recursive = TRUE)
}

get_OxCGRT <- function(){
  df <- rio::import("https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx")
  write.csv(df, file.path("data", "OxCGRT_Oxford_regulation_policies.csv"), row.names = FALSE)
}


get_GHS <- function(){
  f <- file.path(tempdir(), 'ghs.zip')
  download.file("https://www.ghsindex.org/wp-content/uploads/2019/10/Global-Health-Security-Index-2019-Final-October-2019.zip", destfile = f, mode = "wb")

  utils::unzip(zipfile = f, files = "Global Health Security Index 2019 Final (October 2019).xlsm", exdir = tempdir())
  # THis doesn't work
  browser()
  tmp <- read_excel(file.path(tempdir(), "Global Health Security Index 2019 Final (October 2019).xlsm"))
  
  #write.csv(df, file.path("data", "OxCGRT_Oxford_regulation_policies.csv"), row.names = FALSE)
  file.remove(f, file.path(tempdir(), "Global Health Security Index 2019 Final (October 2019).xlsm"))
}


zip.file.extract