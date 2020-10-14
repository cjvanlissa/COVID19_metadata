df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
desc <- read.csv("scale_descriptives.csv", stringsAsFactors = FALSE)
desc_item <- read.csv("descriptives_finaldata.csv", stringsAsFactors = FALSE)
desc <- desc[desc$Subscale %in% names(df_training), ]
desc$type <- "numeric"
names(desc)[1] <- "name"
names(desc)[!names(desc) %in% names(desc_item)]

total_n <- desc_item$n[which.min(desc_item$missing)]
# desc$missing <- 1-(desc$n/total_n)
# desc$unique <- NA
# desc$median <- NA
# desc$mode <- NA
# desc$mode_value <- NA
# desc$v <- NA
# desc$range <- desc$max - desc$min

scales <- desc_item[desc_item$name %in% desc$name, ]
scales_desc <- desc[desc$name %in% desc_item$name, ]
scales <- scales[, c("name", names(scales)[!names(scales) %in% names(desc)])]
scales <- merge(scales, scales_desc, by = "name")

desc_item <- desc_item[!desc_item$name %in% desc$name, ]
desc_item <- desc_item[desc_item$name %in% names(df_training), ]
names(desc_item)[!names(desc_item) %in% names(desc)]
desc_item$Items <- 1
desc_item[c("Reliability", "Interpret", "min_load", "max_load")] <- NA
#desc_item[c("missing", "unique", "median", "mode", "mode_value", "v", "range")] <- NULL

#desc_item[c("missing", "unique", "median", "mode", "mode_value", "v", "range")] <- NULL


tab <- rbind(scales, desc_item)
tab <- tab[tab$name %in% names(df_training), ]

labels <- read.csv("scripts/df_training_labs.csv", stringsAsFactors = FALSE)
names(labels) <- c("name", "label")
tab <- merge(tab, labels, by = "name")
tab$id <- tolower(tab$label)
labels <- read.csv("labels.csv", stringsAsFactors = FALSE)
#labels$Variable.name <- tolower(labels$Variable.name)
names(labels)[1] <- "id"
labels$id <- tolower(labels$id)
labels$id[!labels$id %in% tab$id]
tab$id[!tab$id %in% labels$id]
tab <- merge(tab, labels, by = "id", all.x = TRUE)
tmp <- readRDS("results/res_c19perbeh.RData")
res <- tmp$rf$final_model
# 
VI <- sort(res$variable.importance, decreasing = TRUE)
VI <- data.frame(name = names(VI), Importance = VI, rank = 1:length(VI))
tab$id[!tab$id %in% VI$id]
tab <- merge(tab, VI, by = "name", all.x = TRUE)


tab$id <- NULL
tab <- tab[order(tab$rank), ]
write.csv(tab, "table1.csv", row.names = F)

#summary(lm(c19perbeh ~ age, df_training))

#df_training$gender

sapply(unique(df_training$countryiso3), function(x){
  #x = df_training$countryiso3[1]
  c(n = sum(df_training$countryiso3 == x),
    table(df_training$age[df_training$countryiso3 == x]))
})

if(FALSE){
  pontus <- read.csv("Table S3_14-October_PL_cj.csv", stringsAsFactors = F, check.names = F)
  names(pontus)[1] <- "Label"
  order_vars <- names(pontus)
  tab <- read.csv("table1.csv", stringsAsFactors = F, check.names = F)
  names(tab)[match(c("name", "label", "rank", "Items", "Reliability", "Interpret", "n"), names(tab))] <- c("Variable Shortname", "Label", "Rank", "# Items", "Reliability (alpha)", "Interpretation of alpha", "N with data")
  tab[c("Description")] <- NULL
  pontus <- pontus[c("Variable Shortname", names(pontus)[!names(pontus) %in% names(tab)])]
  pontus <- merge(pontus, tab, by = "Variable Shortname", all.x = TRUE)
  pontus <- pontus[order(pontus$Rank, decreasing = FALSE), order_vars]
  write.csv(pontus, "Table S3_14-October_PL_cj_updated.csv", na = "", row.names = FALSE)
}
