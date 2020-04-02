library(tidyverse)

# TODO: RENAME THE FIRST YEAR COLUMN
# TODO: ADD POPULATION COLUMN
# TODO: TRANSFORM HOSPITALS PER 1MIL TO TOTAL WHERE NEEDED
# TODO: FILTER FOR COUNTRY INCONSITENCIES 
# TODO: STANDARDISE COUNTRY NAMES AND ADD COUNTR CODES
# TODO: ADD MORE COMMENT



# Aggregating Health Facilities Data 

# Some notes on data similarities/differences between WHO and OECD
# I made a decision to choose mainly one source to collect the data from. I chose WHO as it had usually included much 
# more countries. So I only used OECD when some data category wasn't available in WHO.

# This was the case for two data categories:
#  1.For number of hospitals there was large mismatch in the countries, some were available in OECD but not in WHO
#    and vice versa. So the hospitals ....
#  2.Hospital beds data was only found in OECD, so this data will be used 

# In all other cases data was gathered from the WHO databases. I went with the approach of using the most recent 
# available data for each country in each category 

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

# DATA WRANGLING STEP 1: hospital and hospital beds data
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
                                                 "total_number_of_hospitals",
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
                                                  "total_number_of_hospital_beds",
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

# now our hospital data is almost ready. Since we know the hospitals_per_1mil for all countries, we can use that 
# to fill up the total_number_of_hospitals column. Since all of NAs have come from the WHO data, we can use 
# population data for 2013 to impute the missing values. 


# CHANGE LATER WHEN YOU CAN CHANGE COUNTRY NAMES SAFELY 

world_population_data <- read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")
world_population_data_2013 <- world_population_data[ ,c(1, 56)]

hospital_data_joined_world_pop <- left_join(hospital_data_joined, world_population_data_2013, by=c("country"="Country"))

hospital_data_joined_world_pop$Year_2013 <- hospital_data_joined_world_pop$Year_2013/1000000

hospital_data_joined_world_pop[is.na(hospital_data_joined_world_pop$total_number_of_hospitals), ] <- 
  hospital_data_joined_world_pop[is.na(hospital_data_joined_world_pop$total_number_of_hospitals), ] %>% 
  mutate(total_number_of_hospitals = hospitals_per_1mil * Year_2013)
  
hospital_data_joined <- hospital_data_joined %>%  dplyr::arrange(country)
#############################################################################################


# DATA WRANGLING STEP 2: the rest of the data
#############################################################################################
# wrangling WHO doctors data 
WHO_doctors_selected <- WHO_doctors %>% dplyr::select(c(X1, 
                                X2,
                                `Medical doctors (per 10 000 population)`, 
                                `Medical doctors (number)`))

colnames(WHO_doctors_selected) <- c("country", 
                                    "year_of_doctors_records", 
                                    "doctors_per_10k", 
                                    "total_number_of_doctors")

WHO_doctors_cleaned <- WHO_doctors_selected[2:nrow(WHO_doctors_selected), ]
# get latest number of doctors 
WHO_doctors_ready_for_join <- WHO_doctors_cleaned %>% 
  group_by(country) %>% 
  filter(year_of_doctors_records == max(year_of_doctors_records))

joined_data_1 <- full_join(hospital_data_joined, WHO_doctors_ready_for_join, by="country")

# wrangling WHO nurses data 
colnames(WHO_nurses) <- c("country", 
                          "year_of_nurses_records", 
                          "nurses_and_midwifery_per_10k", 
                          "total_nurses_and_midwifery",
                          "total_number_of_nurses",
                          "total_number_of_midwifery")

# get latest number of doctors 
WHO_nurses_ready_for_join <- WHO_nurses %>% 
  group_by(country) %>% 
  filter(year_of_nurses_records == max(year_of_nurses_records))

joined_data_2 <- full_join(joined_data_1, WHO_nurses_ready_for_join, by="country")

# wrangling WHO pollution data
colnames(WHO_pollution_2016) <- c("country", 
                                  "total_PM2.5_concentration_2016", 
                                  "urban_pm2.5_concentration_2016",
                                  "rural_pm2.5_concentration_2016")

WHO_pollution_2016_ready_to_join <- WHO_pollution_2016[3:nrow(WHO_pollution_2016), ]

joined_data_3 <- full_join(joined_data_2, WHO_pollution_2016_ready_to_join, by="country")


# wrangling WHO current health expenditure (CHE) as a percentage of GDP

colnames(WHO_health_exp)[c(1,2,3)] <- c("country", 
                                        "CHE_as_percentage_of_GDP_2017",
                                        "CHE_as_percentage_of_GDP_2016")



WHO_health_exp_sel <- WHO_health_exp[2:nrow(WHO_health_exp), ]
WHO_health_exp_sel <- WHO_health_exp_sel %>%
  dplyr::select("country", 
                "CHE_as_percentage_of_GDP_2017",
                "CHE_as_percentage_of_GDP_2016")


joined_data_4 <- full_join(joined_data_3, WHO_health_exp_sel, by="country")



# wrangling WHO pollution deaths data
colnames(WHO_air_poluttion_deaths)[c(1,2,3,6,9)] <- c("country", 
                                                      "cause", 
                                                      "total_air_pol_deaths_2016", 
                                                      "air_pol_deaths_per_100k_2016",
                                                      "air_pol_deaths_per_100k_age_standardised_2016")


WHO_air_poluttion_deaths_sel <- WHO_air_poluttion_deaths[3:nrow(WHO_air_poluttion_deaths), ]
WHO_air_poluttion_deaths_sel <- WHO_air_poluttion_deaths_sel %>% dplyr::select("country", 
                                           "cause", 
                                           "total_air_pol_deaths_2016", 
                                           "air_pol_deaths_per_100k_2016",
                                           "air_pol_deaths_per_100k_age_standardised_2016")


WHO_air_poluttion_deaths_sel$cause <- recode(WHO_air_poluttion_deaths_sel$cause, 
                                             "Lower respiratory infections" = "lower_respiratory_infections", 
                                             "Trachea, bronchus, lung cancers" = "trachea_bronchus_lung_cancers",
                                             "Ischaemic heart disease" = "ischaemic_heart_disease",
                                             "Chronic obstructive pulmonary disease" = "chronic_obstructive_pulmonary_disease")


WHO_air_poluttion_deaths_wide <- pivot_wider(WHO_air_poluttion_deaths_sel, names_from=cause, 
                                             values_from=c(  "total_air_pol_deaths_2016", 
                                                             "air_pol_deaths_per_100k_2016",
                                                             "air_pol_deaths_per_100k_age_standardised_2016"))

joined_data_5 <- full_join(joined_data_4, WHO_air_poluttion_deaths_wide, by="country")

# wrangling WHO pollution DALYs data
colnames(WHO_air_poluttion_DALYs)[c(1,2,3,9,12)] <- c("country", 
                                                      "cause", 
                                                      "total_air_pol_DALYs_2016", 
                                                      "air_pol_DALYs_per_100k_2016",
                                                      "air_pol_DALYs_per_100k_age_standardised_2016")

WHO_air_poluttion_DALYs_sel <- WHO_air_poluttion_DALYs[3:nrow(WHO_air_poluttion_DALYs), ]
WHO_air_poluttion_DALYs_sel <- WHO_air_poluttion_DALYs_sel %>% 
  dplyr::select("country", 
                "cause", 
                "total_air_pol_DALYs_2016", 
                "air_pol_DALYs_per_100k_2016",
                "air_pol_DALYs_per_100k_age_standardised_2016")


WHO_air_poluttion_DALYs_filt <- WHO_air_poluttion_DALYs_sel %>% filter(cause != "Cataracts")


WHO_air_poluttion_DALYs_filt$cause <- recode(WHO_air_poluttion_DALYs_filt$cause, 
       "Lower respiratory infections" = "lower_respiratory_infections", 
       "Trachea, bronchus, lung cancers" = "trachea_bronchus_lung_cancers",
       "Ischaemic heart disease" = "ischaemic_heart_disease",
       "Chronic obstructive pulmonary disease" = "chronic_obstructive_pulmonary_disease")



WHO_air_poluttion_DALYs_wide <- pivot_wider(WHO_air_poluttion_DALYs_filt, names_from=cause, 
                                             values_from=c("total_air_pol_DALYs_2016", 
                                                           "air_pol_DALYs_per_100k_2016",
                                                           "air_pol_DALYs_per_100k_age_standardised_2016"))

joined_data_6 <- full_join(joined_data_5, WHO_air_poluttion_DALYs_wide, by="country")

# wrangling WHO pollution YLLs data
colnames(WHO_air_polution_YLLs)[c(1,2,3)] <- c("country", 
                                                      "cause", 
                                                      "total_air_pol_YYLss_2016")

WHO_air_polution_YLLs <- WHO_air_polution_YLLs[3:nrow(WHO_air_polution_YLLs), ]
WHO_air_polution_YLLs <- WHO_air_polution_YLLs %>% 
  dplyr::select("country", 
                "cause", 
                "total_air_pol_YYLss_2016")


WHO_air_polution_YLLs$cause <- recode(WHO_air_polution_YLLs$cause, 
                                             "Lower respiratory infections" = "lower_respiratory_infections", 
                                             "Trachea, bronchus, lung cancers" = "trachea_bronchus_lung_cancers",
                                             "Ischaemic heart disease" = "ischaemic_heart_disease",
                                             "Chronic obstructive pulmonary disease" = "chronic_obstructive_pulmonary_disease")


WHO_air_polution_YLLs_wide <- pivot_wider(WHO_air_polution_YLLs, names_from=cause, 
                                            values_from=c("total_air_pol_YYLss_2016"),
                                                          names_prefix="total_air_pol_YYLss_2016_")
                                                        

joined_data_7 <- full_join(joined_data_6, WHO_air_polution_YLLs_wide, by="country")



# wrangling WHO household pollution deaths data
colnames(WHO_household_air_poluttion_deaths)[c(1,2,3,6,9)] <- c("country", 
                                                      "cause", 
                                                      "total_household_air_pol_deaths_2016", 
                                                      "household_air_pol_deaths_per_100k_2016",
                                                      "household_air_pol_deaths_per_100k_age_standardised_2016")



WHO_household_air_poluttion_deaths_sel <- WHO_household_air_poluttion_deaths[3:nrow(WHO_household_air_poluttion_deaths), ]
WHO_household_air_poluttion_deaths_sel <- WHO_household_air_poluttion_deaths_sel %>%
  dplyr::select("country", 
                "cause", 
                "total_household_air_pol_deaths_2016", 
                "household_air_pol_deaths_per_100k_2016",
                "household_air_pol_deaths_per_100k_age_standardised_2016")


WHO_household_air_poluttion_deaths_sel$cause <- recode(WHO_household_air_poluttion_deaths_sel$cause, 
                                             "Lower respiratory infections" = "lower_respiratory_infections", 
                                             "Trachea, bronchus, lung cancers" = "trachea_bronchus_lung_cancers",
                                             "Ischaemic heart disease" = "ischaemic_heart_disease",
                                             "Chronic obstructive pulmonary disease" = "chronic_obstructive_pulmonary_disease")


WHO_household_air_poluttion_deaths_wide <- pivot_wider(WHO_household_air_poluttion_deaths_sel, names_from=cause, 
                                             values_from=c("total_household_air_pol_deaths_2016", 
                                                           "household_air_pol_deaths_per_100k_2016",
                                                           "household_air_pol_deaths_per_100k_age_standardised_2016"))



joined_data_8 <- full_join(joined_data_7, WHO_household_air_poluttion_deaths_wide, by="country")



# wrangling WHO household pollution DALYs data
colnames(WHO_household_air_poluttion_DALYs)[c(1,2,3,6,9)] <- c("country", 
                                                                "cause", 
                                                                "total_household_air_pol_DALYs_2016", 
                                                                "household_air_pol_DALYs_per_100k_2016",
                                                                "household_air_pol_DALYs_per_100k_age_standardised_2016")



WHO_household_air_poluttion_DALYs_sel <- WHO_household_air_poluttion_DALYs[3:nrow(WHO_household_air_poluttion_DALYs), ]
WHO_household_air_poluttion_DALYs_sel <- WHO_household_air_poluttion_DALYs_sel %>%
  dplyr::select("country", 
                "cause", 
                "total_household_air_pol_DALYs_2016", 
                "household_air_pol_DALYs_per_100k_2016",
                "household_air_pol_DALYs_per_100k_age_standardised_2016")


WHO_household_air_poluttion_DALYs_sel$cause <- recode(WHO_household_air_poluttion_DALYs_sel$cause, 
                                                       "Lower respiratory infections" = "lower_respiratory_infections", 
                                                       "Trachea, bronchus, lung cancers" = "trachea_bronchus_lung_cancers",
                                                       "Ischaemic heart disease" = "ischaemic_heart_disease",
                                                       "Chronic obstructive pulmonary disease" = "chronic_obstructive_pulmonary_disease")
WHO_household_air_poluttion_DALYs_filt <- WHO_household_air_poluttion_DALYs_sel %>% filter(cause != "Cataracts")


WHO_household_air_poluttion_DALYSs_wide <- pivot_wider(WHO_household_air_poluttion_DALYs_filt, names_from=cause, 
                                                       values_from=c("total_household_air_pol_DALYs_2016", 
                                                                     "household_air_pol_DALYs_per_100k_2016",
                                                                     "household_air_pol_DALYs_per_100k_age_standardised_2016"))

joined_data_9 <- full_join(joined_data_8, WHO_household_air_poluttion_DALYSs_wide, by="country")

# write the data
write_csv(joined_data_9, "data/WHO_OECD/who_oecd_data_2_april.csv")


