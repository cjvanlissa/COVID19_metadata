library(gert)
library(countrycode)
source("c:/git_repositories/psycorona/psycorona-datacleaning/scripts/functions/merge_metadata.R")

your_datafile <- read.csv("c:/git_repositories/psycorona/psycorona-datacleaning/data/raw data/RMS3_vanLissa_Cooperation 2020-04-20 14-53 CEST.csv", stringsAsFactors = F)

your_datafile$countryiso3 <- countrycode::countrycode(your_datafile$coded_country, origin = "country.name", destination = "iso3c")

metadata_location <- file.path(".")

# See which metadata files are available
which_files(metadata_location)



merge_files(your_datafile, "csse", metadata_location) %>%
  merge_files("csse_within_14", metadata_location) %>%
  merge_files("country_descriptives", metadata_location) %>%
  merge_files("OxCGRT_Oxford_regulation_policies", metadata_location) %>%
  merge_files("google_mobility", metadata_location) ->
  merged_data

merged_data[grep("\\.\\w$", names(merged_data))] <- NULL

stringency <- merged_data[, grep("stringe", names(merged_data), value = TRUE)]

merged_data$max_stringency <- apply(stringency, 1, max, na.rm = TRUE)
merged_data$max_stringency[is.infinite(merged_data$max_stringency)] <- NA
rm(stringency)

cases <- merged_data[, grep("^confirmed", names(merged_data), value = TRUE)]
merged_data$max_cases <- apply(cases, 1, max, na.rm = TRUE)
merged_data$max_cases[is.infinite(merged_data$max_cases)] <- NA
rm(cases)

send_data <- merged_data[, -match(names(your_datafile)[-c(1, 19)], names(merged_data))]
saveRDS(send_data, "coda_data.RData")
write.csv(send_data, "coda_data.csv", row.names = FALSE)