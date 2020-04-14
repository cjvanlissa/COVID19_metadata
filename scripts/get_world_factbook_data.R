library(gert)
library(rio)
library(readxl)
library(countrycode)
library(tidyr)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(data.table)
library(glue)
library(forcats)
library(wbstats)
library(lubridate)
library(tidyverse)

# Get selected data
get_world_factbook_data <- function(){
  get_fctb_population()
  get_fctb_median()
  get_fctb_population_growth()
  get_fctb_birth_rate()
  get_fctb_death_rate()
  get_fctb_migration_rate()
  get_fctb_maternal_mortality()
  get_fctb_infant_mortality()
  get_fctb_life_expectancy()
  get_fctb_fertility_rate()
  get_health_expenditures()
  get_fctb_hiv_rate_adult()
  get_fctb_hiv_rate_all()
  get_fctb_hiv_deaths()
  get_fctb_obesity()
  get_fctb_children_underweight()
  get_fctb_education_expenditures()
  get_fctb_unemployment_youth()
  get_fctb_airports()
  get_fctb_railways()
  get_fctb_roadways()
  get_fctb_waterways()
  get_fctb_merchant_marine()
}

# Convert between contry name and iso3
iso3List <- c("Country Name","Country ISO3",
              "Afghanistan","AFG",
              "Albania","ALB",
              "Algeria","DZA",
              "American Samoa","ASM",
              "Andorra","AND",
              "Angola","AGO",
              "Anguilla","AIA",
              "Antarctica","ATA",
              "Antigua and Barbuda","ATG",
              "Argentina","ARG",
              "Armenia","ARM",
              "Aruba","ABW",
              "Australia","AUS",
              "Austria","AUT",
              "Azerbaijan","AZE",
              "Bahamas, The","BHS",
              "Bahrain","BHR",
              "Bangladesh","BGD",
              "Barbados","BRB",
              "Belarus","BLR",
              "Belgium","BEL",
              "Belize","BLZ",
              "Benin","BEN",
              "Bermuda","BMU",
              "Bhutan","BTN",
              "Bolivia","BOL",
              "Bosnia and Herzegovina","BIH",
              "Botswana","BWA",
              "Bouvet Island","BVT",
              "Brazil","BRA",
              "British Indian Ocean Territory","IOT",
              "British Virgin Islands","VGB",
              "Brunei","BRN",
              "Bulgaria","BGR",
              "Burkina Faso","BFA",
              "Burma","MMR",
              "Burundi","BDI",
              "Cabo Verde","CPV",
              "Cambodia","KHM",
              "Cameroon","CMR",
              "Canada","CAN",
              "Cayman Islands","CYM",
              "Central African Republic","CAF",
              "Chad","TCD",
              "Chile","CHL",
              "China","CHN",
              "Christmas Island","CXR",
              "Cocos (Keeling) Islands","CCK",
              "Colombia","COL",
              "Comoros","COM",
              "Congo, Democratic Republic of the","COD",
              "Congo, Republic of the","COG",
              "Cook Islands","COK",
              "Costa Rica","CRI",
              "Cote d'Ivoire","CIV",
              "Croatia","HRV",
              "Cuba","CUB",
              "Curacao","CUW",
              "Cyprus","CYP",
              "Czechia","CZE",
              "Denmark","DNK",
              "Djibouti","DJI",
              "Dominica","DMA",
              "Dominican Republic","DOM",
              "Ecuador","ECU",
              "Egypt","EGY",
              "El Salvador","SLV",
              "Equatorial Guinea","GNQ",
              "Eritrea","ERI",
              "Estonia","EST",
              "Eswatini","SWZ",
              "Ethiopia","ETH",
              "Falkland Islands (Islas Malvinas)","FLK",
              "Faroe Islands","FRO",
              "Fiji","FJI",
              "Finland","FIN",
              "France","FRA",
              "France, Metropolitan","FXX",
              "French Guiana","GUF",
              "French Polynesia","PYF",
              "French Southern and Antarctic Lands","ATF",
              "Gabon","GAB",
              "Gambia, The","GMB",
              "Gaza Strip","PSE",
              "Georgia","GEO",
              "Germany","DEU",
              "Ghana","GHA",
              "Gibraltar","GIB",
              "Greece","GRC",
              "Greenland","GRL",
              "Grenada","GRD",
              "Guadeloupe","GLP",
              "Guam","GUM",
              "Guatemala","GTM",
              "Guernsey","GGY",
              "Guinea","GIN",
              "Guinea-Bissau","GNB",
              "Guyana","GUY",
              "Haiti","HTI",
              "Heard Island and McDonald Islands","HMD",
              "Holy See (Vatican City)","VAT",
              "Honduras","HND",
              "Hong Kong","HKG",
              "Hungary","HUN",
              "Iceland","ISL",
              "India","IND",
              "Indonesia","IDN",
              "Iran","IRN",
              "Iraq","IRQ",
              "Ireland","IRL",
              "Isle of Man","IMN",
              "Israel","ISR",
              "Italy","ITA",
              "Jamaica","JAM",
              "Japan","JPN",
              "Jersey","JEY",
              "Jordan","JOR",
              "Kazakhstan","KAZ",
              "Kenya","KEN",
              "Kiribati","KIR",
              "Korea, North","PRK",
              "Korea, South","KOR",
              "Kosovo","XKS",
              "Kuwait","KWT",
              "Kyrgyzstan","KGZ",
              "Laos","LAO",
              "Latvia","LVA",
              "Lebanon","LBN",
              "Lesotho","LSO",
              "Liberia","LBR",
              "Libya","LBY",
              "Liechtenstein","LIE",
              "Lithuania","LTU",
              "Luxembourg","LUX",
              "Macau","MAC",
              "Madagascar","MDG",
              "Malawi","MWI",
              "Malaysia","MYS",
              "Maldives","MDV",
              "Mali","MLI",
              "Malta","MLT",
              "Marshall Islands","MHL",
              "Martinique","MTQ",
              "Mauritania","MRT",
              "Mauritius","MUS",
              "Mayotte","MYT",
              "Mexico","MEX",
              "Micronesia, Federated States of","FSM",
              "Moldova","MDA",
              "Monaco","MCO",
              "Mongolia","MNG",
              "Montenegro","MNE",
              "Montserrat","MSR",
              "Morocco","MAR",
              "Mozambique","MOZ",
              "Namibia","NAM",
              "Nauru","NRU",
              "Nepal","NPL",
              "Netherlands","NLD",
              "New Caledonia","NCL",
              "New Zealand","NZL",
              "Nicaragua","NIC",
              "Niger","NER",
              "Nigeria","NGA",
              "Niue","NIU",
              "Norfolk Island","NFK",
              "North Macedonia","MKD",
              "Northern Mariana Islands","MNP",
              "Norway","NOR",
              "Oman","OMN",
              "Pakistan","PAK",
              "Palau","PLW",
              "Panama","PAN",
              "Papua New Guinea","PNG",
              "Paraguay","PRY",
              "Peru","PER",
              "Philippines","PHL",
              "Pitcairn Islands","PCN",
              "Poland","POL",
              "Portugal","PRT",
              "Puerto Rico","PRI",
              "Qatar","QAT",
              "Reunion","REU",
              "Romania","ROU",
              "Russia","RUS",
              "Rwanda","RWA",
              "Saint Barthelemy","BLM",
              "Saint Helena, Ascension, and Tristan da Cunha","SHN",
              "Saint Kitts and Nevis","KNA",
              "Saint Lucia","LCA",
              "Saint Martin","MAF",
              "Saint Pierre and Miquelon","SPM",
              "Saint Vincent and the Grenadines","VCT",
              "Samoa","WSM",
              "San Marino","SMR",
              "Sao Tome and Principe","STP",
              "Saudi Arabia","SAU",
              "Senegal","SEN",
              "Serbia","SRB",
              "Seychelles","SYC",
              "Sierra Leone","SLE",
              "Singapore","SGP",
              "Sint Maarten","SXM",
              "Slovakia","SVK",
              "Slovenia","SVN",
              "Solomon Islands","SLB",
              "Somalia","SOM",
              "South Africa","ZAF",
              "South Georgia and the Islands","SGS",
              "South Sudan","SSD",
              "Spain","ESP",
              "Sri Lanka","LKA",
              "Sudan","SDN",
              "Suriname","SUR",
              "Svalbard","SJM",
              "Sweden","SWE",
              "Switzerland","CHE",
              "Syria","SYR",
              "Taiwan","TWN",
              "Tajikistan","TJK",
              "Tanzania","TZA",
              "Thailand","THA",
              "Timor-Leste","TLS",
              "Togo","TGO",
              "Tokelau","TKL",
              "Tonga","TON",
              "Trinidad and Tobago","TTO",
              "Tunisia","TUN",
              "Turkey","TUR",
              "Turkmenistan","TKM",
              "Turks and Caicos Islands","TCA",
              "Tuvalu","TUV",
              "Uganda","UGA",
              "Ukraine","UKR",
              "United Arab Emirates","ARE",
              "United Kingdom","GBR",
              "United States","USA",
              "United States Minor Outlying Islands","UMI",
              "Uruguay","URY",
              "Uzbekistan","UZB",
              "Vanuatu","VUT",
              "Venezuela","VEN",
              "Vietnam","VNM",
              "Virgin Islands","VIR",
              "Wallis and Futuna","WLF",
              "West Bank","PSE",
              "Western Sahara","ESH",
              "Yemen","YEM",
              "Zambia","ZMB",
              "Zimbabwe","ZWE",
              NA, NA)


countryNameToISO3 <- function(name){
 pos <- match(name, iso3List) 
 return(iso3List[pos+1])
}


countryISO3ToName <- function(name){
  pos <- match(name, iso3List)
  if (pos == 0 || pos %% 2 != 0){
    return(NA)
  }
  else{
    return(iso3List[pos-1])
  }
}


# Population compares estimates from the US Bureau of the Census based on statistics from population censuses, vital statistics 
# registration systems, or sample surveys pertaining to the recent past and on assumptions about future trends (July 2020 estd)
get_fctb_population <- function(){
  process_fctb("population", 2119)
}

  
# Median age
get_fctb_median <- function(){
  process_fctb("median", 2177)
}


# Population growth rate
get_fctb_population_growth <- function(){
  process_fctb("population_growth", 2002)
}


# Birth rate compares the average annual number of births during a year per 1,000 persons in the population at midyear; also known
# as crude birth rate.
get_fctb_birth_rate <- function(){
  process_fctb("birth_rate", 2054)
}


# Death rate compares the average annual number of deaths during a year per 1,000 population at midyear; also known as crude death 
# rate
get_fctb_death_rate <- function(){
  process_fctb("death_rate", 2066)
}


# Net migration rate compares the difference between the number of persons entering and leaving a country during the year per 1,000 
# persons (based on midyear population)
get_fctb_migration_rate <- function(){
  process_fctb("migration_rate", 2112)
}


# The Maternal mortality rate (MMR) is the annual number of female deaths per 100,000 live births from any cause related to or 
# aggravated by pregnancy or its management (excluding accidental or incidental causes).
get_fctb_maternal_mortality <- function(){
  process_fctb("maternal_mortality", 2223)
}


# Infant mortality rate compares the number of deaths of infants under one year old in a given year per 1,000 live births in the 
# same year. This rate is often used as an indicator of the level of health in a country
get_fctb_infant_mortality <- function(){
  process_fctb("infant_mortality", 2091)
}


# Life expectancy at birth compares the average number of years to be lived by a group of people born in the same year, if 
# mortality at each age remains constant in the future. Life expectancy at birth is also a measure of overall quality of life 
# in a country and summarizes the mortality at all ages
get_fctb_life_expectancy <- function(){
  process_fctb("life_expectancy", 2102)
}


# Total fertility rate (TFR) compares figures for the average number of children that would be born per woman if all women lived to 
# the end of their childbearing years and bore children according to a given fertility rate at each age. TFR is a more direct measure 
# of the level of fertility than the crude birth rate, since it refers to births per woman
get_fctb_fertility_rate <- function(){
  process_fctb("fertility_rate", 2127)
}


# Health expenditures provides the total expenditure on health as a percentage of GDP. Health expenditures are broadly defined as 
# activities performed either by institutions or individuals through the application of medical, paramedical, and/or nursing 
# knowledge and technology, the primary purpose of which is to promote, restore, or maintain health
get_health_expenditures <- function(){
  process_fctb("health_expenditures", 2225)
}


# HIV/AIDS - adult prevalence rate compares the percentage of adults (aged 15-49) living with HIV/AIDS
get_fctb_hiv_rate_adult <- function(){
  process_fctb("hiv_rate_adult", 2155)
}


# HIV/AIDS - people living with HIV/AIDS compares all people (adults and children) alive at yearend with HIV infection, whether or 
# not they have developed symptoms of AIDS.
get_fctb_hiv_rate_all <- function(){
  process_fctb("hiv_rate_all", 2156)
}


# HIV/AIDS - deaths compares the number of adults and children who died of AIDS during a given calendar year.
get_fctb_hiv_deaths <- function(){
  process_fctb("hiv_deaths", 2157)
}


# Obesity - adult prevalence rate gives the percent of a country's population considered to be obese.
get_fctb_obesity <- function(){
  process_fctb("obesity", 2228)
}


# Children under the age of 5 years underweight gives the percent of children under five considered to be underweight. 
# Underweight means weight-for-age is approximately 2 kg below for standard at age one, 3 kg below standard for ages two and 
# three, and 4 kg below standard for ages four and five.
get_fctb_children_underweight <- function(){
  process_fctb("children_underweight", 2224)
}


# Education expenditures compares the public expenditure on education as a percent of GDP.
get_fctb_education_expenditures <- function(){
  process_fctb("education_expenditures", 2206)
}


# Unemployment, youth ages 15-24 gives the percent of the total labor force ages 15-24 unemployed during a specified year.
get_fctb_unemployment_youth <- function(){
  process_fctb("unemployment_youth", 2229)
}


# Airports compares the total number of airports or airfields recognizable from the air. The runway(s) may be paved or unpaved 
# and may include closed or abandoned installations
get_fctb_airports <- function(){
  process_fctb("airports", 2053)
}


# Railways compares the total route length of the railway network and of its component parts.
get_fctb_railways <- function(){
  process_fctb("railways", 2121)
}


# Roadways compares the total length of the road network and includes the length of the paved and unpaved portions.
get_fctb_roadways <- function(){
  process_fctb("roadways", 2085)
}


# Waterways compares the total length of navigable rivers, canals, and other inland bodies of water
get_fctb_waterways <- function(){
  process_fctb("waterways", 2093)
}


# Merchant marine compares all publicly or privately owned commercial ships; excluded are military ships.
get_fctb_merchant_marine <- function(){
  process_fctb("merchant_marine", 2108)
}


process_fctb <- function(description, indicatorId){
  print(paste("Processing world factbook data", toupper(description), "...", sep=" "))
  year <- 2020
  url <- paste("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_", indicatorId, ".txt", sep="")
  inStr <- read_file(url)
  inStr <- gsub("  +", "\t", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  inStr <- gsub("\t([0-9]+),([0-9]+)", "\t\\1\\2", inStr)
  df <- read.delim(text=inStr, sep = "\t",  header = FALSE)
  df <- df %>% select(-c(1,4))
  df$country <- df[,1] 
  names(df)[1:2] <- c("countryiso3", "value")
  df <- df[c(3, 1, 2)]
  df[2] <- lapply(df[2], countryNameToISO3)
  df <- df[complete.cases(df[2]),]
  
  # Dictionary
  df$variable <- paste(gsub("_", "", description), indicatorId, sep=".");
  df$description <- description
  
  dict <- df[, c("variable", "description")]
  dict <- dict[!duplicated(dict$variable), ]
  df <- df %>% select(-c("description"))
  df <- df[c(1, 2, 4, 3)]
  
  # Add year columns
  names(df)[4] <- year
  
  # Create pivot
  data <- df
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("code", "year"), "_")
  recent_data <- recent_data %>%  
    mutate(countryiso3 = fct_inorder(countryiso3), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, code)
  recent_data <- recent_data %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # Write results
  fileName = paste("fctb", description, sep="_")
  directory <- paste("FCTB", toupper(description), sep="_")
  checkfilewrite(recent_data, directory, paste(paste("recent", fileName, sep="_"), "csv", sep="."))
  checkfilewrite(dict, directory, "data_dictionary.csv")
  checkfilewrite(data, directory, paste(fileName, "csv", sep="."))
}
