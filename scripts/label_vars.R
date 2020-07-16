df_vars <- read.csv("scripts/variable_names.csv", stringsAsFactors = FALSE)
var_rename <- tolower(df_vars$X)
names(var_rename) <- df_vars$lab

names(df_training)[names(df_training) %in% var_rename]

head(names(df_training)[!names(df_training) %in% var_rename])


data.frame(X = c("countryiso3", "date", "coronaclose", "employstatus", "disc", 
  "jbinsec"),
  lab = c("Country", "Date", "COVID exposure", "Empl. status", "Soc. discontent", 
          "Job insec.")