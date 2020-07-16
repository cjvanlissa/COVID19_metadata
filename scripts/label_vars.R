df_vars <- read.csv("scripts/variable_names.csv", stringsAsFactors = FALSE)
df_vars$X <- tolower(df_vars$X)
df_vars <- df_vars[df_vars$X %in% names(df_training), ]
df_vars <- rbind(df_vars,
                 data.frame(X = names(df_training)[!names(df_training) %in% df_vars$X],
                            lab = NA))

write.csv(df_vars, "scripts/df_training_labs.csv", row.names = F)

df_vars <- read.csv("scripts/df_training_labs.csv", stringsAsFactors = F)
var_rename <- tolower(df_vars$lab)
names(var_rename) <- df_vars$X
