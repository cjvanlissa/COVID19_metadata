library(tidyverse)
library(countrycode)

# TODO: MANUALLY ADD HOSPITALS TO LARGE POPULATION COUNTRIES WHERE MISSING? (some countries with large populations 
# are still missing number of hospitals eg. Brazil, China, Russia ...

# Downloading WHO data
#############################################################################################
# Hospitals
WHO_hospitals = read_csv("https://apps.who.int/gho/athena/data/GHO/DEVICES00,DEVICES01,DEVICES02,DEVICES03,DEVICES04,DEVICES05?filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO&profile=crosstable&format=csv")
head(WHO_hospitals)

# Nurses
WHO_nurses = read_csv("https://apps.who.int/gho/athena/data/GHO/HWF_0006,HWF_0007,HWF_0008,HWF_0009?filter=COUNTRY:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO&profile=crosstable&format=csv")
head(WHO_nurses)

# Doctors
WHO_doctors = read_csv("https://apps.who.int/gho/athena/data/GHO/HWF_0001,HWF_0002,HWF_0003,HWF_0004,HWF_0005,HWF_0025?filter=COUNTRY:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO;SEX&profile=crosstable&format=csv")
head(WHO_doctors)

# Ambient air pollution 
# Description: https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4674
WHO_pollution_2016 = read_csv("https://apps.who.int/gho/athena/data/GHO/SDGPM25?filter=COUNTRY:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR;RESIDENCEAREATYPE&profile=crosstable&format=csv")
head(WHO_pollution_2016)

# Deaths attributable to ambient air pollution 
# Description: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/ambient-air-pollution-attributable-deaths
WHO_air_poluttion_deaths = read_csv("https://apps.who.int/gho/athena/data/GHO/AIR_5,AIR_41,AIR_42?filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY;ENVCAUSE&x-topaxis=GHO;YEAR;AGEGROUP;SEX&profile=crosstable&format=csv")
head(WHO_air_poluttion_deaths)

# Ambient air pollution attributable DALYs (Number of disability-adjusted life years), 
# Description: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/ambient-air-pollution-attributable-dalys
WHO_air_poluttion_DALYs = read_csv("https://apps.who.int/gho/athena/data/GHO/AIR_8,AIR_9,AIR_90,AIR_7?filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY;ENVCAUSE&x-topaxis=GHO;YEAR;AGEGROUP;SEX&profile=crosstable&format=csv")
head(WHO_air_poluttion_DALYs)

# Ambient air pollution attributable YLLs (Number of years life lost)
# Description: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/ambient-air-pollution-attributable-ylls
WHO_air_polution_YLLs = read_csv("https://apps.who.int/gho/athena/data/GHO/AIR_45?filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY;ENVCAUSE&x-topaxis=GHO;YEAR;SEX&profile=crosstable&format=csv")
head(WHO_air_polution_YLLs)

# Deaths attributable to household air pollution
# Description: https://www.who.int/data/gho/indicator-metadata-registry/imr-details/2256
WHO_household_air_poluttion_deaths = read_csv("https://apps.who.int/gho/athena/data/GHO/AIR_11,AIR_51,AIR_52?filter=COUNTRY:*&x-sideaxis=COUNTRY;ENVCAUSE;GHECAUSES&x-topaxis=GHO;YEAR;AGEGROUP;SEX&profile=crosstable&format=csv")
head(WHO_household_air_poluttion_deaths)

# DALYs attributable to household air pollution 
# Description: https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4491
WHO_household_air_poluttion_DALYs = read_csv("https://apps.who.int/gho/athena/data/GHO/AIR_15,AIR_17,AIR_39?filter=COUNTRY:*&x-sideaxis=COUNTRY;ENVCAUSE&x-topaxis=GHO;YEAR;AGEGROUP;SEX&profile=crosstable&format=csv")
head(WHO_household_air_poluttion_DALYs)

# EXPENDITURE (Current health expenditure (CHE) as percentage of gross domestic product (GDP) (%))
# Description: https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4950
WHO_health_exp <- read_csv("https://apps.who.int/gho/athena/data/GHO/GHED_CHEGDP_SHA2011?filter=REGION:*;COUNTRY:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR&profile=crosstable&format=csv")
head(WHO_health_exp)
#############################################################################################



# Downloading OECD data 
#############################################################################################
# I wasn't able to open the data directly from the download link so I had to download the CSVs onto my computer first
# HOSPITALS 
# link for the download https://stats.oecd.org/Download.ashx?type=csv&Delimiter=%2c&IncludeTimeSeriesIdentifiers=False&LabelType=CodeAndLabel&LanguageCode=en
OECD_hospitals = read_csv("data/WHO_OECD/oecd_1_raw.csv")
# HOSPITAL BEDS https://stats.oecd.org/Download.ashx?type=csv&Delimiter=%2c&IncludeTimeSeriesIdentifiers=False&LabelType=CodeAndLabel&LanguageCode=en
OECD_hospital_beds <- read_csv("data/WHO_OECD/oecd_2_raw.csv")
#############################################################################################

# Aggregating Health Facilities Data 
#############################################################################################
# Some notes on data similarities/differences between WHO and OECD
# I made a decision to choose mainly one source to collect the data from. I chose WHO as it had usually included much 
# more countries. So I only used OECD when some data category wasn't available in WHO.

# This was the case for two data categories:
#  1.For thenumber of hospitals there was large mismatch in the countries, some were available in OECD but not in WHO
#    and vice versa. So the hospitals ....
#  2.Hospital beds data was only found in OECD, so this data will be used 

# In all other cases data was gathered from the WHO databases. I went with the approach of using the most recent 
# available data for each country in each category 


# Wrangle hospital and hospital beds data
#############################################################################################
# FIRST extract and filter Hospital data from OECD 
# Originaly OECD hospital data contains 35 countries and OECD hospital_beds data contains 44 countries
length(unique(OECD_hospitals$Country))
length(unique(OECD_hospital_beds$Country))

# get latest total number of hospitals and the number of hospitals per 1 mil per country 
OECD_hospitals_filtered <- OECD_hospitals %>% 
  group_by(Country) %>% 
  dplyr::filter(Variable == "Hospitals", 
                Measure %in% c("Number", "Per million population"),
                Year == max(Year)) %>% 
  ungroup() %>% 
  select(Variable,Measure, Country, Value, Year)

# Looks like Norway has dropped out as the result but upon a closer look at the OECD data Norway is barely present 
# with only two rows from year 2000
unique(OECD_hospitals$Country)[!(unique(OECD_hospitals$Country) %in% OECD_hospitals_filtered$Country)]
# the years rang is from 2003 to 2018
range(min(OECD_hospitals_filtered$Year),max(OECD_hospitals_filtered$Year))

# make it into long format
OECD_hospitals_long <- pivot_wider(OECD_hospitals_filtered,names_from=Measure, values_from=Value)
# rename the columns for joining later 
colnames(OECD_hospitals_long)[c(2,3,4,5)] <- c("country",
                                               "year",
                                                 "total_n_of_hospitals",
                                                 "hospitals_per_1mil")
# keep only needed columns 
OECD_hospitals_ready_to_join <- OECD_hospitals_long[c(2,3,4,5)]




# get latest total number of hospital beds and the number of hospitals per 1 mil per country 
OECD_hospital_beds_filtered <- OECD_hospital_beds %>% 
  group_by(Country) %>% 
  dplyr::filter(Variable == "Total hospital beds", 
                Measure %in% c("Number", "Per 1 000 population"),
                Year == max(Year)) %>% 
  ungroup() %>% 
  select(Variable, Measure, Country, Value, Year)

# All countries were kept during the filter
unique(OECD_hospital_beds$Country)[!(unique(OECD_hospital_beds$Country) %in% OECD_hospital_beds_filtered$Country)]
# the years rang is from 2010 to 2018
range(min(OECD_hospital_beds_filtered$Year),max(OECD_hospital_beds_filtered$Year))

# make it into long format
OECD_hospital_beds_long <- pivot_wider(OECD_hospital_beds_filtered,names_from=Measure, values_from=Value)
# rename the columns for joining later 
colnames(OECD_hospital_beds_long)[c(2,4,5)] <- c("country",
                                                  "total_n_of_hospital_beds",
                                                  "hospital_beds_per_1000")
# keep only needed columns 
OECD_hospital_beds_ready_to_join <- OECD_hospital_beds_long[c(2,4,5)]


# Now extract and filter Hospital data from WHO 
# WHO only has data of total hospitals per 100,000 population 
# From WHO there is data on 144 countries
length(unique(WHO_hospitals$Country))

WHO_hospitals_filtered <- WHO_hospitals %>% 
  group_by(Country) %>% 
  dplyr::filter(!is.na(`Total density per 100 000 population: Hospitals`),
                Year == max(Year)) %>% 
  ungroup() %>% 
  select(Country, Year, `Total density per 100 000 population: Hospitals`)

# in WHO the hospital density is per 100,000 thousand and not 1 million. I will multiply the values by 10 to make
# the scale same as in OECD
colnames(WHO_hospitals_filtered)[c(1,2,3)] <- c("country", "year", "hospitals_per_1mil")
WHO_hospitals_filtered$hospitals_per_1mil <- WHO_hospitals_filtered$hospitals_per_1mil * 10


# JOIN all of the hospital DFs
# Now we need to find a way to join the OECD and WHO datas.However they have one overlaping variable, hospitals_per_1mil.
# Thus my approach is as follows:
# 1. If the country is in only one database then we use that hospitals_per_1mil record
# 2. If the country is in both databases then we use the most recent hospitals_per_1mil record

# check what countries overlap in countries 
overlaping_countries_oecd_data <- OECD_hospitals_ready_to_join[OECD_hospitals_ready_to_join$country %in% 
                                                                WHO_hospitals_filtered$country, ]

overlaping_countries_who_data <- WHO_hospitals_filtered %>% 
  dplyr::filter(country %in% overlaping_countries_oecd_data$country)

# check if WHO has any data that is more recent than OECD for overlapping columns 
any(overlaping_countries_who_data$year > overlaping_countries_oecd_data$year)
# OECD always has the most recent columns for hospitals_per_1mil. So we will just use these records

# anti join and bind rows to get the full, most recent hospitals_per1_mil_data
anti_joined_data <- anti_join(WHO_hospitals_filtered,OECD_hospitals_ready_to_join[c(1,2,4)], by = "country") 
hospitals_per1_mil_data <- bind_rows(anti_joined_data, OECD_hospitals_ready_to_join[c(1,2,4)])

# now join all of the hospitals data one by one 
join1 <- full_join(hospitals_per1_mil_data, OECD_hospitals_ready_to_join[c(1,3)], by = "country")
hospital_data_joined <- full_join(join1, OECD_hospital_beds_ready_to_join, by = "country")

colnames(hospital_data_joined)[2] <- "hospital_data_yr_rec"

# now our hospital data is almost ready. Since we know the hospitals_per_1mil for all countries, we can use that 
# to fill up the total_number_of_hospitals column. Since all of NAs have come from the WHO data, we can use 
# population data for 2013 to impute the missing values. 


# Wrangling WHO doctors data 
WHO_doctors_selected <- WHO_doctors %>% dplyr::select(c(X1, 
                                X2,
                                `Medical doctors (per 10 000 population)`, 
                                `Medical doctors (number)`))

colnames(WHO_doctors_selected) <- c("country", 
                                    "doctors_data_yr_rec", 
                                    "doctors_per_10k", 
                                    "total_n_of_doctors")

WHO_doctors_cleaned <- WHO_doctors_selected[2:nrow(WHO_doctors_selected), ]
# get latest number of doctors 
WHO_doctors_ready_for_join <- WHO_doctors_cleaned %>% 
  group_by(country) %>% 
  filter(doctors_data_yr_rec == max(doctors_data_yr_rec))

joined_data_1 <- full_join(hospital_data_joined, WHO_doctors_ready_for_join, by="country")

# Wrangling WHO nurses data 
colnames(WHO_nurses) <- c("country", 
                          "nurses_data_yr_rec", 
                          "nurses_and_midwifery_per_10k", 
                          "total_n_of_nurses_and_midwifery",
                          "total_n_of_nurses",
                          "total_n_of_midwifery")

# get latest number of nurses 
WHO_nurses_ready_for_join <- WHO_nurses %>% 
  group_by(country) %>% 
  filter(nurses_data_yr_rec == max(nurses_data_yr_rec))

joined_data_2 <- full_join(joined_data_1, WHO_nurses_ready_for_join, by="country")


# wrangling WHO current health expenditure (CHE) as a percentage of GDP
colnames(WHO_health_exp)[c(1,2,3)] <- c("country", 
                                        "CHE_perc_of_GDP_2017",
                                        "CHE_perc_of_GDP_2016")



WHO_health_exp_sel <- WHO_health_exp[2:nrow(WHO_health_exp), ]
WHO_health_exp_sel <- WHO_health_exp_sel %>%
  dplyr::select("country", 
                "CHE_perc_of_GDP_2017",
                "CHE_perc_of_GDP_2016")


joined_data_3 <- full_join(joined_data_2, WHO_health_exp_sel, by="country")


# add population data 
# WHO only has data from 2016
world_population_data <- read_csv("https://apps.who.int/gho/athena/data/GHO/WHS9_86,WHS9_88,WHS9_89,WHS9_92,WHS9_96,WHS9_97,WHS9_90?filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO&profile=crosstable&format=csv")
world_population_data_2016 <- world_population_data %>% 
  filter(Year == 2016) %>% 
  dplyr::select(Country, `Population (in thousands) total`)

hospital_data_joined_world_pop <- left_join(joined_data_3, world_population_data_2016, by=c("country"="Country"))

# remove whitespace 
hospital_data_joined_world_pop$`Population (in thousands) total` <- 
  as.numeric(gsub('\\s+', '',hospital_data_joined_world_pop$`Population (in thousands) total`))

# change the name of the population column
colnames(hospital_data_joined_world_pop)[ncol(hospital_data_joined_world_pop)] <- "world_pop_2016_thousands"


# ADDING ISO3 CODES
# Let's first fix country name inconsistencies before adding ISO3 codes
# I will load in othr data from the repo for reference
##############################################################################################################
preparedness_dat <- read_csv("data/GHS/preparedness.csv")
# get a list of countries that dont match 
unique(hospital_data_joined_world_pop$country)[!(unique(hospital_data_joined_world_pop$country) %in% 
                                                 unique((preparedness_dat$country)))]

# start renaming countries to match 
hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Bolivia (Plurinational State of)") , 1] = "Bolivia"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Brunei Darussalam") , 1] = "Brunei"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "China (People's Republic of)") , 1] = "China"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Czechia") , 1] = "Czech Republic"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Democratic People's Republic of Korea") , 1] = "North Korea"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Republic of Korea") , 1] = "South Korea"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Kyrgyzstan") , 1] = "Kyrgyz Republic"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                      "Democratic Republic of the Congo") , 1] = "Congo (Democratic Republic)"

hospital_data_joined_world_pop[which(hospital_data_joined_world_pop$country ==
                                       "Lao People's Democratic Republic" ) , 1] = "Laos"


# add iso3 country codes
hospital_data_joined_world_pop = add_column(hospital_data_joined_world_pop, 
                                            countryiso3 = countrycode(hospital_data_joined_world_pop$country, 
                                                                      "country.name","iso3c"), 
                                            .after = "country")
##############################################################################################################

# RESOLVING DUPLICATE COUNTRIES
# check cases where there are duplicate countries as the result of WHO and OECD data merge and resolve these 
# to keep the most recent records
##############################################################################################################
hospital_data_joined_world_pop %>% group_by(countryiso3) %>%
  filter(n() > 1)

# CHINA
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="CHN", ]
which(hospital_data_joined_world_pop$countryiso3=="CHN")


hospital_data_joined_world_pop[157, 8:ncol(hospital_data_joined_world_pop)] =
  hospital_data_joined_world_pop[167, 8:ncol(hospital_data_joined_world_pop)]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-167, ]

#CZECH
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="CZE", ]
czech_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="CZE"))

hospital_data_joined_world_pop[czech_rows[[1]], 3:7] =
  hospital_data_joined_world_pop[czech_rows[[2]], 3:7]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-czech_rows[[2]], ]

#RUSSIA
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="RUS", ]
rus_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="RUS"))


hospital_data_joined_world_pop[rus_rows[[1]], 8:ncol(hospital_data_joined_world_pop)] =
  hospital_data_joined_world_pop[rus_rows[[2]], 8:ncol(hospital_data_joined_world_pop)]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-rus_rows[[2]], ]

#GREAT BRITAIN
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="GBR", ]
gbr_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="GBR"))


hospital_data_joined_world_pop[gbr_rows[[1]], 8:ncol(hospital_data_joined_world_pop)] =
  hospital_data_joined_world_pop[gbr_rows[[2]], 8:ncol(hospital_data_joined_world_pop)]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-gbr_rows[[2]], ]

# KOREA
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="KOR", ]
kor_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="KOR"))


hospital_data_joined_world_pop[kor_rows[[1]], 2:7] =
  hospital_data_joined_world_pop[kor_rows[[2]], 2:7]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-kor_rows[[2]], ]

# USA
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="USA", ]
usa_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="USA"))

hospital_data_joined_world_pop[usa_rows[[1]], 8:ncol(hospital_data_joined_world_pop)] =
  hospital_data_joined_world_pop[usa_rows[[2]], 8:ncol(hospital_data_joined_world_pop)]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-usa_rows[[2]], ]

# SLOVAKIA
hospital_data_joined_world_pop[hospital_data_joined_world_pop$countryiso3=="SVK", ]
svk_rows <- as.list(which(hospital_data_joined_world_pop$countryiso3=="SVK"))

hospital_data_joined_world_pop[svk_rows[[1]], 2:7] =
  hospital_data_joined_world_pop[svk_rows[[2]], 2:7]

hospital_data_joined_world_pop <- hospital_data_joined_world_pop[-svk_rows[[2]], ]

# final check 
hospital_data_joined_world_pop %>% group_by(countryiso3) %>%
  filter(n() > 1)
# no more country duplicates!!
##############################################################################################################

# CALCULATING TOTAL HOSPITALS WHERE MISSING
##############################################################################################################
# Calculate total hospitals per country using country populations and hospitals per 1mil
# Because the WHO had data for hospitals per 1mil there are much more countries with this record, 
# so we can use this and the country populations to calculate the total hospitals 

# calculate the total hospitals per country if not already recorded
hospital_data_joined_world_pop[is.na(hospital_data_joined_world_pop$total_n_of_hospitals), ] <- 
  hospital_data_joined_world_pop[is.na(hospital_data_joined_world_pop$total_n_of_hospitals), ] %>% 
  mutate(total_n_of_hospitals = floor(hospitals_per_1mil * (world_pop_2016_thousands/1000)))


hospital_data_wrangled <- hospital_data_joined_world_pop %>% dplyr::arrange(countryiso3)

colnames(hospital_data_wrangled) <- tolower(colnames(hospital_data_wrangled))

# write data
if (!file.exists("data/WHO_OECD/WHO_OECD_health_infrastructure.csv")) {
  
write_csv(hospital_data_wrangled, "data/WHO_OECD/WHO_OECD_health_infrastructure.csv")
  
}
##############################################################################################################


# Aggregating WHO pollution data
#############################################################################################
# All of the pollution data comes from WHO. Here I join and wrangle muulltiple data tables.

# JOINING WHO POLLUTION DATA
##############################################################################################################
colnames(WHO_pollution_2016) <- c("country", 
                                  "t.PM2.5.c.2016", 
                                  "u.PM2.5.c.2016",
                                  "r.PM2.5.c.2016")

WHO_pollution_2016_ready_to_join <- WHO_pollution_2016[3:nrow(WHO_pollution_2016), ]


# JOINING WHO pollution deaths data
colnames(WHO_air_poluttion_deaths)[c(1,2,3,6,9)] <- c("country", 
                                                      "cause", 
                                                      "t.ap.d.2016", 
                                                      "ap.d.100k.2016",
                                                      "ap.d.100k.as.2016")


WHO_air_poluttion_deaths_sel <- WHO_air_poluttion_deaths[3:nrow(WHO_air_poluttion_deaths), ]
WHO_air_poluttion_deaths_sel <- WHO_air_poluttion_deaths_sel %>% dplyr::select("country", 
                                           "cause", 
                                           "t.ap.d.2016", 
                                           "ap.d.100k.2016",
                                           "ap.d.100k.as.2016")


WHO_air_poluttion_deaths_sel$cause <- recode(WHO_air_poluttion_deaths_sel$cause, 
                                             "Lower respiratory infections" = "lri", 
                                             "Trachea, bronchus, lung cancers" = "tblc",
                                             "Ischaemic heart disease" = "ihd",
                                             "Chronic obstructive pulmonary disease" = "copd")

WHO_air_poluttion_deaths_wide <- pivot_wider(WHO_air_poluttion_deaths_sel, names_from=cause,names_sep = ".",
                                             values_from=c(  "t.ap.d.2016", 
                                                             "ap.d.100k.2016",
                                                             "ap.d.100k.as.2016"))


joined_data_pollution_1 <- full_join(WHO_pollution_2016_ready_to_join, WHO_air_poluttion_deaths_wide, by="country")

# JOINING WHO pollution DALYs data
colnames(WHO_air_poluttion_DALYs)[c(1,2,3,9,12)] <- c("country", 
                                                      "cause", 
                                                      "t.ap.dal.2016", 
                                                      "ap.dal.100k.2016",
                                                      "ap.dal.100k.as.2016")

WHO_air_poluttion_DALYs_sel <- WHO_air_poluttion_DALYs[3:nrow(WHO_air_poluttion_DALYs), ]
WHO_air_poluttion_DALYs_sel <- WHO_air_poluttion_DALYs_sel %>% 
  dplyr::select("country", 
                "cause", 
                "t.ap.dal.2016", 
                "ap.dal.100k.2016",
                "ap.dal.100k.as.2016")


WHO_air_poluttion_DALYs_filt <- WHO_air_poluttion_DALYs_sel %>% filter(cause != "Cataracts")


WHO_air_poluttion_DALYs_filt$cause <- recode(WHO_air_poluttion_DALYs_filt$cause, 
                                             "Lower respiratory infections" = "lri", 
                                             "Trachea, bronchus, lung cancers" = "tblc",
                                             "Ischaemic heart disease" = "ihd",
                                             "Chronic obstructive pulmonary disease" = "copd")



WHO_air_poluttion_DALYs_wide <- pivot_wider(WHO_air_poluttion_DALYs_filt, names_from=cause, names_sep = ".",
                                             values_from=c("t.ap.dal.2016", 
                                                                           "ap.dal.100k.2016",
                                                                           "ap.dal.100k.as.2016"))
                                            

joined_data_pollution_2 <- full_join(joined_data_pollution_1, WHO_air_poluttion_DALYs_wide, by="country")

# JOINING WHO pollution YLLs data
colnames(WHO_air_polution_YLLs)[c(1,2,3)] <- c("country", 
                                                      "cause", 
                                                      "t.ap.yll.2016")

WHO_air_polution_YLLs <- WHO_air_polution_YLLs[3:nrow(WHO_air_polution_YLLs), ]
WHO_air_polution_YLLs <- WHO_air_polution_YLLs %>% 
  dplyr::select("country", 
                "cause", 
                "t.ap.yll.2016")


WHO_air_polution_YLLs$cause <- recode(WHO_air_polution_YLLs$cause, 
                                      "Lower respiratory infections" = "lri", 
                                      "Trachea, bronchus, lung cancers" = "tblc",
                                      "Ischaemic heart disease" = "ihd",
                                      "Chronic obstructive pulmonary disease" = "copd")

WHO_air_polution_YLLs_wide <- pivot_wider(WHO_air_polution_YLLs, names_from=cause, names_sep = ".",
                                            values_from=c("t.ap.yll.2016"),
                                                          names_prefix="t.ap.yll.2016.")
                                                        

joined_data_pollution_3 <- full_join(joined_data_pollution_2, WHO_air_polution_YLLs_wide, by="country")



# JOINING WHO household pollution deaths data
colnames(WHO_household_air_poluttion_deaths)[c(1,2,3,6,9)] <- c("country", 
                                                      "cause", 
                                                      "t.hap.d.2016", 
                                                      "hap.d.100k.2016",
                                                      "hap.d.100k.as.2016")




WHO_household_air_poluttion_deaths_sel <- WHO_household_air_poluttion_deaths[3:nrow(WHO_household_air_poluttion_deaths), ]
WHO_household_air_poluttion_deaths_sel <- WHO_household_air_poluttion_deaths_sel %>%
  dplyr::select("country", 
                "cause", 
                "t.hap.d.2016", 
                "hap.d.100k.2016",
                "hap.d.100k.as.2016")



WHO_household_air_poluttion_deaths_sel$cause <- recode(WHO_household_air_poluttion_deaths_sel$cause, 
                                                       "Lower respiratory infections" = "lri", 
                                                       "Trachea, bronchus, lung cancers" = "tblc",
                                                       "Ischaemic heart disease" = "ihd",
                                                       "Chronic obstructive pulmonary disease" = "copd")

WHO_household_air_poluttion_deaths_wide <- pivot_wider(WHO_household_air_poluttion_deaths_sel, names_from=cause, 
                                                       names_sep = ".",
                                             values_from=c("t.hap.d.2016", 
                                                           "hap.d.100k.2016",
                                                           "hap.d.100k.as.2016"))



joined_data_pollution_4 <- full_join(joined_data_pollution_3, WHO_household_air_poluttion_deaths_wide, by="country")



# JOINING WHO household pollution DALYs data
colnames(WHO_household_air_poluttion_DALYs)[c(1,2,3,6,9)] <- c("country", 
                                                                "cause", 
                                                               "t.hap.dal.2016", 
                                                               "hap.dal.100k.2016",
                                                               "hap.dal.100k.as.2016")



WHO_household_air_poluttion_DALYs_sel <- WHO_household_air_poluttion_DALYs[3:nrow(WHO_household_air_poluttion_DALYs), ]
WHO_household_air_poluttion_DALYs_sel <- WHO_household_air_poluttion_DALYs_sel %>%
  dplyr::select("country", 
                "cause", 
                "t.hap.dal.2016", 
                "hap.dal.100k.2016",
                "hap.dal.100k.as.2016")


WHO_household_air_poluttion_DALYs_sel$cause <- recode(WHO_household_air_poluttion_DALYs_sel$cause, 
                                                      "Lower respiratory infections" = "lri", 
                                                      "Trachea, bronchus, lung cancers" = "tblc",
                                                      "Ischaemic heart disease" = "ihd",
                                                      "Chronic obstructive pulmonary disease" = "copd")

WHO_household_air_poluttion_DALYs_filt <- WHO_household_air_poluttion_DALYs_sel %>% filter(cause != "Cataracts")


WHO_household_air_poluttion_DALYSs_wide <- pivot_wider(WHO_household_air_poluttion_DALYs_filt, names_from=cause, 
                                                       names_sep = ".",
                                                       values_from=c("t.hap.dal.2016", 
                                                                     "hap.dal.100k.2016",
                                                                     "hap.dal.100k.as.2016"))

joined_data_pollution_5 <- full_join(joined_data_pollution_4, WHO_household_air_poluttion_DALYSs_wide, by="country")
##############################################################################################################

# CLEANING WHO POLLUTION DATA
##############################################################################################################
#make all column names lower case
colnames(joined_data_pollution_5) <- tolower(colnames(joined_data_pollution_5))
colnames(joined_data_pollution_5)

# filter out confidence intervals and white space from data 
white_space_remover <- function(string){
  
  return(as.numeric(gsub('\\s+', '', as.character(string))))
}

ci_remover <- function(string){
  
  return(as.numeric(gsub( " .*$", "", as.character(string))))
}

colnames(joined_data_pollution_5)[41] <- "Tempchange"
WHO_pollution_dat_no_ws <-  joined_data_pollution_5 %>% mutate_at(vars(matches('yll')), white_space_remover)

WHO_pollution_dat_no_ci <-  WHO_pollution_dat_no_ws %>% mutate_at(vars(-matches("yll|countr")), ci_remover)
colnames(WHO_pollution_dat_no_ci)[41] <- "t.ap.yll.2016.Total"

# make all column names lowercase
colnames(WHO_pollution_dat_no_ci) <- tolower(colnames(WHO_pollution_dat_no_ci))
colnames(WHO_pollution_dat_no_ci)


# ADDING ISO3 CODES
##############################################################################################################
# Let's first fix country name inconsistencies before adding ISO3 codes
# start renaming countries to match 
WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Bolivia (Plurinational State of)") , 1] = "Bolivia"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Brunei Darussalam") , 1] = "Brunei"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "China (People's Republic of)") , 1] = "China"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Czechia") , 1] = "Czech Republic"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Democratic People's Republic of Korea") , 1] = "North Korea"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Republic of Korea") , 1] = "South Korea"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Kyrgyzstan") , 1] = "Kyrgyz Republic"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Democratic Republic of the Congo") , 1] = "Congo (Democratic Republic)"

WHO_pollution_dat_no_ci[which(WHO_pollution_dat_no_ci$country ==
                                       "Lao People's Democratic Republic" ) , 1] = "Laos"


# add iso3 country codes
WHO_pollution_dat_wrangled = add_column(WHO_pollution_dat_no_ci, 
                                            countryiso3 = countrycode(WHO_pollution_dat_no_ci$country, 
                                                                      "country.name","iso3c"), 
                                            .after = "country")


WHO_pollution_dat_wrangled <- WHO_pollution_dat_wrangled %>% dplyr::arrange(countryiso3)

# write data
if (!file.exists("data/WHO_OECD/WHO_pollution.csv")) {
  
  write_csv(WHO_pollution_dat_wrangled, "data/WHO_OECD/WHO_pollution.csv")
  
}






