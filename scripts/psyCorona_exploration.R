########## TO DO ##########

# TODO: think about what to do with NAs
# TODO: add ways of measurement (yes/no, Likert, etc.) to comments above variables
# TODO: merge with metadata
# TODO: think about other DVs
# TODO: think about the unbalance in DV

########## PREPARATION ##########
get_testing <- TRUE
library(glmnet)
library(caTools)
library(tidyverse)
library(ggplot2)
if(!require("worcs")){
  library(remotes)
  install_github("cjvanlissa/worcs", upgrade ="never")
  library(worcs)
}
if(!require("tidySEM")){
  install_github("cjvanlissa/tidySEM", upgrade ="never")
  library(tidySEM)
}
source("scripts/psyCorona_EDA_plot_function.R")

########## READ AND PREPARE DATA ##########
df_raw <- read.csv("data/RMD30_Caspar van Lissa_2020-05-21 15-21 CEST.csv", stringsAsFactors = FALSE) # read in raw data
#df_raw <- read_csv("data/RMD30_Caspar van Lissa_2020-05-13 17-24 CEST.csv") # read in raw data
df_raw[df_raw == -99] <- NA
# There's some missing values not coded :(


# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
names(df_raw) <- tolower(names(df_raw))        # all names to lower
df_raw <- df_raw[, !grepl("^w\\d", names(df_raw))] # main data file we will use; only baseline measurements
df_raw <- df_raw[!is.na(df_raw$startdate), ]

# Create training/testing split (training will be split into (cross-) validation samples)
set.seed(953007)
if(file.exists("split_sample.csv")){
  the_split <- read.csv("split_sample.csv")
  not_split <- which(!df_raw$responseid %in% the_split$responseid)
  df_raw <- merge(df_raw, the_split, by = "responseid", all.x = TRUE)
  if(length(not_split) > 0){
    new <- as.logical(rbinom(length(not_split), 1, .7))
    df_raw$train[not_split] <- new
    write.csv(df_raw[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
  }
} else {
  df_raw$train <- as.logical(rbinom(nrow(df_raw), 1, .7))
  write.csv(df_raw[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
}

# Drop testing cases; drop longitudinal waves
if(get_testing){
  df <- df_raw[!df_raw$train, !grepl("^w\\d", names(df_raw))]
} else {
  df <- df_raw[df_raw$train, !grepl("^w\\d", names(df_raw))]
}

# getting percentags of countries' counts
country_percentages <- df %>% group_by(coded_country) %>% 
  summarise(perc_of_resp = 100*(n()/nrow(df))) 

# Select only countries with > 1%?
retain_countries <- country_percentages$coded_country[country_percentages$perc_of_resp > 1]
df <- df[which(df$coded_country %in% retain_countries),] # removing countries that are < 1% of data


# Descriptive stats
table_descriptives <- descriptives(df)

# Some variables might be spread out over several columns; check codebook
vars <- grep("_\\d$", table_descriptives$name[table_descriptives$unique < 4], value = TRUE)
# For example, coronaclose:
df$coronaclose <- apply(df[, grep("^coronaclose_\\d", names(df))], 1, function(i){which(i == 1)[1]})
df[, grep("^coronaclose_\\d", names(df))] <- NULL

# Recode: 1 = unemployed, 2 = 1-23 hours, 3 = 24-39 etc
df$employstatus <- apply(df[, c(grep("^employstatus_[45]$", names(df)), grep("^employstatus_[123]$", names(df)))], 1, function(i){which(i == 1)[1]})
# Who is still missing
still_missing <- is.na(df$employstatus)
# Classify as not working: Homemaker, Retired, Disabled, Student, or Volunteering
is_notworking <- apply(df[still_missing, grep("^employstatus_([6789]|10)$", names(df))], 1, function(i){any(i == 1)})
df$employstatus[still_missing][is_notworking] <- 1
df[, grep("^employstatus_\\d+$", names(df))] <- NULL

# House leave
# Recode to dummies: If people filled out any answers about houseleave, then NAs should really be 0.
not_all_missing <- rowSums(is.na(df[, startsWith(names(df), "houseleave")])) < 6
df[not_all_missing, startsWith(names(df), "houseleavewhy")] <- matrix(as.numeric(!is.na(df[not_all_missing, startsWith(names(df), "houseleavewhy")])), ncol = 5)
# Keep all house leave variables, because they are dummies

# here I remove all of the columns that are not needed for analysis. I also remove the "ranking" features as I am not 
# sure how to use them in analysis. (MAYBE RE-ADD LATER)
df <- df %>% select(-enddate, -polorx, -polory, -polorcat, -language, -contains("rank"))

# Create scales -----------------------------------------------------------

vars <- unique(gsub("\\d+$", "", grep("(?<!_)\\d$", names(df), value = TRUE, perl = TRUE)))
scales_list <- lapply(vars, function(i){ grep(paste0("^", i, "\\d+$"), names(df), value = TRUE)})
names(scales_list) <- vars
scales_list[sapply(scales_list, length) < 2] <- NULL
# Do we need to check other reverse coded variables?
rev <- c("jbinsec02", "disc03", "bor03")
#descriptives(df[, rev])
df$bor03 <- 4-df$bor03
df$jbinsec02 <- -1*df$jbinsec02
df$disc03 <- -1*df$disc03
# Create the scales
scales <- create_scales(df, keys.list = scales_list)
# View(scales$descriptives) # commenting this out so it doesn't open every time we run the modelling script
# Add to df
df <- cbind(df, scales$scores)
# Remove items
df[unlist(scales_list)] <- NULL

########## EXPLORE VARIABLES ##########

# Explore NAs
ggplot(table_descriptives, aes(x = missing)) + geom_density()

# Plot vars
# Descriptive stats again, for analysis df
table_descriptives <- descriptives(df)

if(FALSE){
  plotdat <- df[, table_descriptives$name[table_descriptives$type %in% c("numeric", "integer")]]
  plotdat <- pivot_longer(plotdat, cols = names(plotdat))
  
  # View only full screen. Consider plotting only vars with weird skew/kurtosis values
  ggplot(plotdat, aes(x = value)) + geom_density() + facet_wrap(~name, scales = "free") + theme_bw()
  
  # START/END DATE
  explore_psyCorona(df,"startdate")          # exploring StartDate
  startdate_NAs <- df[is.na(df$startdate), ] # exlude NAs
  explore_psyCorona(df,"enddate")            # exploring EndDate
  
  # AFFECT
  explore_psyCorona(df,"affanx", n_bins = 5)     # How did you feel over the last week? - Anxious
  explore_psyCorona(df,"affbor", n_bins = 5)     # How did you feel over the last week? - Bored
  explore_psyCorona(df,"affcalm", n_bins = 5)    # How did you feel over the last week? - Calm
  explore_psyCorona(df,"affcontent", n_bins = 5) # How did you feel over the last week? - Content
  explore_psyCorona(df,"affdepr", n_bins = 5)    # How did you feel over the last week? - Depressed
  explore_psyCorona(df,"affenerg", n_bins = 5)   # How did you feel over the last week? - Energetic
  explore_psyCorona(df,"affexc", n_bins = 5)     # How did you feel over the last week? - Excited
  explore_psyCorona(df,"affnerv", n_bins = 5)    # How did you feel over the last week? - Nervous
  explore_psyCorona(df,"affinsp", n_bins = 5)    # How did you feel over the last week? - Inspired
  explore_psyCorona(df,"affrel", n_bins = 5)     # How did you feel over the last week? - Relaxed
  
  # LIKELIHOOD (How likely is it that the following will happen to you in the next few months?)
  explore_psyCorona(df,"plrac19", n_bins=8) # You will get infected with coronavirus.
  explore_psyCorona(df,"plraeco", n_bins=8) # Your personal situation will get worse due to economic consequences 
  # of coronavirus.# 205 NAs
  
  # SOCIETAL DISCONTENT (Agree or disagree)
  explore_psyCorona(df,"disc01", n_bins=5) # I fear that things will go wrong in society.
  explore_psyCorona(df,"disc02", n_bins=5) # I feel concerned when I think about the future of society.
  explore_psyCorona(df,"disc03", n_bins=5) # I am satisfied with society.
  
  # JOB INSECURITY (Agree or disagree)
  
  # Chances are, I will soon lose my job.
  # [there is a -99 response (probably signifying that the there is no job to lose), 
  # but I refrained from plotting a histogram for jbInsec]
  unique(df$jbinsec01)
  sum(is.na(df$jbinsec01))
  table(df$jbinsec01)
  
  # I am sure I can keep my job.
  unique(df$jbinsec02)
  sum(is.na(df$jbinsec02))
  table(df$jbinsec02)
  
  # I feel insecure about the future of my job.
  unique(df$jbinsec03)
  sum(is.na(df$jbinsec03))
  table(df$jbinsec03)
  
  # I already lost my job.
  unique(df$jbinsec04)
  sum(is.na(df$jbinsec04))
  table(df$jbinsec04)
  sum(is.na(df$employstatus_1))
  
  # EMPLOYMENT STATUS
  # Which of the following categories best describes your employment status during the last week (multiple may apply)?
  
  # employstatus_1 is working 1-23 hours per week
  # employstatus_2 is working 24-39 hours per week
  # employstatus_3 is working 40 or more hours per week
  # employstatus_4 is Not employed, looking for work
  # employstatus_5 is Not employed, not looking for work
  # employstatus_6 is Homemaker
  # employstatus_7 is Retired
  # employstatus_8 is Disabled, not able to work
  # employstatus_9 is Student
  # employstatus_10 is Volunteering
  
  dat_employee_status <- df %>% dplyr::select(contains("employstatus"))
  # plot the counts of employement statuses
  dat_employee_status %>% pivot_longer(cols=colnames(dat_employee_status)) %>% 
    group_by(name) %>% 
    summarise(status_count = sum(value, na.rm=T)) %>% 
    ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
    geom_bar(stat = "identity") 
  
  # Sometimes more than 1 status is selected (e.g. working 1-23 hours and student)
  # find how many responses are left by each participant (0 signifies that the participant did not indicate any 
  # employement status)
  dat_employee_status$non_NA_number <- apply(dat_employee_status, 1, function(x) ncol(dat_employee_status) - sum(is.na(x)))
  table(dat_employee_status$non_NA_number)
  
  # HOURS WORKED AND WORK CONTACT (In the last week, ...)
  explore_psyCorona(df,"work_home_1", n_bins=100) # ...what percentage of your paid work time did you complete from home?
  explore_psyCorona(df,"socint", n_bins=5)        # ...how often do you interact with others at work as part of your job?
  
  # PERCEIVED FINANCIAL STRAIN (Agree or disagree)
  explore_psyCorona(df,"pfs01", n_bins=5) # I am financially strained.
  explore_psyCorona(df,"pfs02", n_bins=5) # I often think about my current financial situation.
  explore_psyCorona(df,"pfs03", n_bins=5) # Due to my financial situation, I have difficulties paying for my expenses.
  
  # DISEMPOWERMENT (Agree or disagree)
  explore_psyCorona(df,"fail01", n_bins=5) # Not a lot is done for people like me in this country
  explore_psyCorona(df,"fail02", n_bins=5) # If I compare people like me against other people in this country, my group is worse off.
  explore_psyCorona(df,"fail03", n_bins=5) # Recent events in society have increased my struggles in daily life.
  
  # ISOLATION OFFLINE/ONLINE 
  # (In the past 7 days, how much social contact have you had with people who live outside your household? )
  explore_psyCorona(df,"isofriends_inperson", n_bins=8) # in-person (face-to-face) contact with friends or relatives
  explore_psyCorona(df,"isoothppl_inperson", n_bins=8)  # in-person (face-to-face) contact with other people in general
  explore_psyCorona(df,"isoimmi_inperson", n_bins=8)    # in-person (face-to-face) contact with ...immigrants
  explore_psyCorona(df,"isofriends_online", n_bins=8)   # online (video or voice) contact with friends or relatives
  explore_psyCorona(df,"isoothppl_online", n_bins=8)    # online (video or voice) contact with other people in general
  explore_psyCorona(df,"isoimmi_online", n_bins=8)      # online (video or voice) contact with immigrants
  
  # RELATIONSHIPS
  explore_psyCorona(df,"persrelsat", n_bins=10) # In general, how satisfied are you with your personal relationships?
  explore_psyCorona(df,"discpers", n_bins=3)    # Do you have anyone with whom you can discuss very personal matters?
  
  # LONELINESS (During the past week, did you...)
  explore_psyCorona(df,"lone01", n_bins=5) # ...feel lonely?
  explore_psyCorona(df,"lone02", n_bins=5) # ...feel isolated from others?
  explore_psyCorona(df,"lone03", n_bins=5) # ...feel left out?
  
  # LIFE SATISFACTION
  explore_psyCorona(df,"happy", n_bins=10)  # In general, how happy would you say you are?
  explore_psyCorona(df,"lifesat", n_bins=6) # In general, how satisfied are you with your life?
  explore_psyCorona(df,"mlq", n_bins=11)    # My life has a clear sense of purpose.
  
  # TIGHTNESS (To what extent do you think that the country you currently live in should have the following 
  # characteristics right now?)
  explore_psyCorona(df,"tightnorms", n_bins=9) # 1: Have flexible social norms; 9: Have rigid social norms
  explore_psyCorona(df,"tightloose", n_bins=9) # 1: Be loose; 9: Be tight
  explore_psyCorona(df,"tighttreat", n_bins=9) # Treat people who don’t conform to norms... 1: kindly; 9: harshly
  
  # PROBLEM_SOLVING (When dealing with stressful situations, what do you usually do?)
  explore_psyCorona(df,"probsolving01", n_bins=5) # I try to come up with a strategy about what to do.
  explore_psyCorona(df,"probsolving02", n_bins=5) # I make a plan of action.
  explore_psyCorona(df,"probsolving03", n_bins=5) # I think hard about what steps to take.
  explore_psyCorona(df,"posrefocus01", n_bins=5)  # I distract myself to avoid thinking about the subject.
  explore_psyCorona(df,"posrefocus02", n_bins=5)  # I do things to distract myself from my thoughts and feelings.
  explore_psyCorona(df,"posrefocus03", n_bins=5)  # I force myself to think about something else.
  
  # CORONA
  
  # How knowledgeable are you about the recent outbreak of Covid-19 [...] in this country?
  explore_psyCorona(df,"c19know", n_bins=5)
  
  # Agree or disagree: - I have high hopes that the situati on regarding coronavirus will improve.
  explore_psyCorona(df,"c19hope", n_bins=5)
  
  # Agree or disagree: - I think that this country ([QID217-ChoiceTextEntryValue]) is able to fight the Coronavirus.
  explore_psyCorona(df,"c19eff", n_bins=5)
  
  # I am willing to... - ...help others who suffer from coronavirus.
  explore_psyCorona(df,"c19proso01", n_bins=7)
  
  # I am willing to... - ...make donations to help others that suffer from coronavirus.
  explore_psyCorona(df,"c19proso02", n_bins=7)
  
  # I am willing to... - ...protect vulnerable groups from coronavirus even at my own expense.
  explore_psyCorona(df,"c19proso03", n_bins=7)
  
  # I am willing to... - ...make personal sacrifices to prevent the spread of coronavirus.
  explore_psyCorona(df,"c19proso04", n_bins=7)
  
  # CORONA SELF-SERVING BEHAVIOUR (To minimize my chances of getting coronavirus,...)
  explore_psyCorona(df,"c19perbeh01", n_bins=7) # ...wash my hands more often.
  explore_psyCorona(df,"c19perbeh02", n_bins=7) # ...avoid crowded spaces.
  explore_psyCorona(df,"c19perbeh03", n_bins=7) # ...put myself in quarantine.
  
  # CORONA RADICAL ACTION (I would sign a petition that supports...)
  
  # ...mandatory vaccination once a vaccine has been developed for coronavirus.
  explore_psyCorona(df,"c19rca01", n_bins=7)
  
  # ...mandatory quarantine for those that have coronavirus and those that have been exposed to the virus.
  explore_psyCorona(df,"c19rca02", n_bins=7)
  
  # ...reporting people who are suspected to have coronavirus.
  explore_psyCorona(df,"c19rca03", n_bins=7)
  
  # CORONA PROXIMITY
  # Do you personally know anyone who currently has coronavirus? (click all that apply)
  # coronaClose_1 Yes, myself
  # coronaClose_2 Yes, a member of my family
  # coronaClose_3 Yes, a close friend
  # coronaClose_4 Yes, someone I know
  # coronaClose_5 Yes, someone else
  # coronaClose 6 Nom I dont know anyone
  dat_corona_close <- df %>% dplyr::select(contains("coronaclose"))
  
  # plot the counts of employement statuses
  dat_corona_close %>% pivot_longer(cols=colnames(dat_corona_close)) %>% 
    group_by(name) %>% 
    summarise(status_count = sum(value, na.rm=T)) %>% 
    ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
    geom_bar(stat = "identity") 
  
  # Sometimes more than 1 answer is selected TK SHOULD BE CLEANED BEFORE MODELLING
  # find how many responses are left by each participant (0 signifies that the participant did not indicate any 
  # employement status)
  dat_corona_close$non_NA_number <- apply(dat_corona_close, 1, function(x) ncol(dat_corona_close) - sum(is.na(x)))
  table(dat_corona_close$non_NA_number)
  
  # CORONA COMMUNITY NORMS/RESPONSE
  
  # Right now, people in my area...
  explore_psyCorona(df,"c19normshould", n_bins=7) # ...should self-isolate and engage in social distancing.
  explore_psyCorona(df,"c19normdo", n_bins=7)     # ...do self-isolate and engage in social distancing.
  
  # To what extent is your community...
  # ...developing strict rules in response to the Coronavirus?
  explore_psyCorona(df,"c19isstrict", n_bins=6)
  # ...punishing people who deviate from the rules that have been put in place in response to the Coronavirus?
  explore_psyCorona(df,"c19ispunish", n_bins=6)
  # ...well organized in responding to the Coronavirus?
  explore_psyCorona(df,"c19isorg", n_bins=6)
  # ...are you getting clear, unambiguous messages about what to do about the Coronavirus?
  explore_psyCorona(df,"extc19msg", n_bins=6)
  
  # CORONA CONSEQUENCES
  # (How personally disturbing would you find the following possible consequences of the coronavirus?)
  explore_psyCorona(df,"csqc19contract", n_bins=5) # Me contracting the virus
  explore_psyCorona(df,"csqecosuffer", n_bins=5)   # Me suffering negative economic consequences
  explore_psyCorona(df,"csqcancpln", n_bins=5)     # Cancellation of my plans
  explore_psyCorona(df,"csqlife", n_bins=5)        # Changing my life's routines
  
  # ECONOMY
  
  # How knowledgeable are you about the potential economic and financial consequences of coronavirus in this country?
  explore_psyCorona(df,"ecoknow", n_bins=5)
  
  # (Agree or disagree)
  # I have high hopes that the situation regarding the economic and financial consequences of coronavirus will improve.
  explore_psyCorona(df,"ecohope", n_bins=7)
  
  # (Agree or disagree)
  # I think that this country is able to fight the economic and financial consequences of coronavirus.
  explore_psyCorona(df,"ecoeff", n_bins=7)
  
  # ECONOMY PROSOCIAL BEHAVIOUR (To help with the economic and financial consequences of coronavirus, I am willing to...)
  explore_psyCorona(df,"ecoproso01", n_bins=7) # help others who suffer from such consequences.
  explore_psyCorona(df,"ecoproso02", n_bins=7) # make donations to help others that suffer from such consequences.
  explore_psyCorona(df,"ecoproso03", n_bins=7) # protect vulnerable groups from such consequences, even at my own expense.
  explore_psyCorona(df,"ecoproso04", n_bins=7) # make personal sacrifices.
  
  # ECONOMY RADICAL ACTION
  # If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports..
  explore_psyCorona(df,"ecorca01", n_bins=7) # ...higher taxes.
  explore_psyCorona(df,"ecorca02", n_bins=7) # ...giving the government more authority over people.
  explore_psyCorona(df,"ecorca03", n_bins=7) # ...increased government spending.
  
  # LEAVING THE HOUSE
  explore_psyCorona(df,"houseleave", n_bins=4) # In the past week, how often did you leave your home?
  
  # houseLeaveWhy_1 Selected Choice I had to go to work.
  # houseLeaveWhy_2 Selected Choice I had errands to run.
  # houseLeaveWhy_4 Selected Choice For leisure purposes with others (e.g., meeting up with friends, seeing family, going to the cinema, etc.)
  # houseLeaveWhy_6 Other, please specify: - Text
  # houseLeaveWhy_7 Selected Choice For leisure purposes alone (e.g., running, going for a walk, etc.)
  dat_house_leave <- df %>% dplyr::select(contains("houseleavewhy"))
  # plot the counts of employement statuses
  dat_house_leave %>% pivot_longer(cols=colnames(dat_house_leave)) %>% 
    group_by(name) %>% 
    summarise(status_count = sum(value, na.rm=T)) %>% 
    ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
    geom_bar(stat = "identity") 
  
  # Sometimes more than 1 answer is selected TK SHOULD BE CLEANED BEFORE MODELLING
  # find how many responses are left by each participant (0 signifies that the participant did not indicate any 
  # employement status)
  dat_house_leave$non_NA_number <- apply(dat_house_leave, 1, function(x) ncol(dat_house_leave) - sum(is.na(x)))
  table(dat_house_leave$non_NA_number)
  
  # BOREDOM (Indicate your agreement or disagreement with the following statements.)
  explore_psyCorona(df,"bor01", n_bins=7) # I wish time would go by faster.
  explore_psyCorona(df,"bor02", n_bins=7) # Time is moving very slowly.
  explore_psyCorona(df,"bor03", n_bins=7) # I feel in control of my time.
  
  # TEMPORAL FOCUS (Agree or disagree)
  explore_psyCorona(df,"tempfocpast", n_bins=7) # I replay memories of the past in my mind.
  explore_psyCorona(df,"tempfocpres", n_bins=7) # I focus on what is currently happening in my life.
  explore_psyCorona(df,"tempfocfut", n_bins=7)  # I think about what my future has in store.
  
  # INTERGROUP CONTACT (When you think about your contacts with immigrants last week,...)
  explore_psyCorona(df,"contactfulfill", n_bins=11) # do these interactions prove to be helpful or hindering?
  explore_psyCorona(df,"contactqual", n_bins=11)    # how positive or negative did you experience them to be?
  
  # MIGRANT THREAT
  # Migrants who come to live here, generally...
  explore_psyCorona(df,"migrantthreat01", n_bins=10) # take jobs away:create new jobs
  explore_psyCorona(df,"migrantthreat02", n_bins=10) # undermine the cultural life:enrich the cultural life
  explore_psyCorona(df,"migrantthreat03", n_bins=10) # make crime problems worse:make crime problems better
  explore_psyCorona(df,"migrantthreat04", n_bins=10) # harm this country's culture:benefit this country's culture
  explore_psyCorona(df,"migrantthreat05", n_bins=10) # bring diseases:help prevent diseases
  
  # How would you rate: Migrants who come to live here
  explore_psyCorona(df,"feelingtherm", n_bins=9)
  
  # CORONA REFLECTION TASK
  
  # You are presented with two policy options to combat the spread of coronavirus. 
  # Your public health agency will enact one of the policies. Policy A has a 95% chance to prevent 1000 new infections. 
  # Policy B has a 90% chance to prevent 5000 new infections.
  explore_psyCorona(df,"crt1", n_bins=2)
  
  # Does the conclusion follow logically from the premises?
  # Premises: 
  #  - All infected people cough and have a fever
  #  - Your neighbor coughs and has a fever
  # Conclusion: Your neighbor is infected
  explore_psyCorona(df,"crt2", n_bins=2)
  
  # Imagine that every time you leave your home you have a 50/50 chance of getting infected with coronavirus. 
  # You have left your home 5 times recently and nothing has happened. The next time you leave your home, 
  # do you think that:
  explore_psyCorona(df,"crt3", n_bins=3)
  
  # Choose the picture which best represents the closeness between the people of the country you currently live in and 
  # yourself.
  explore_psyCorona(df,"idoverlap", n_bins=5)
  
  # NEUROTICISM (I see myself as someone who...)
  explore_psyCorona(df,"neuro01", n_bins=7) # ...is very concerned.
  explore_psyCorona(df,"neuro02", n_bins=7) # ...easily gets nervous.
  explore_psyCorona(df,"neuro03", n_bins=7) # ...is relaxed, can easily deal with stress.
  
  # PARANOIA
  # I need to be on my guard against others
  explore_psyCorona(df,"para01", n_bins=11) # I need to be on my guard against others
  explore_psyCorona(df,"para02", n_bins=11) # People are trying to make me upset
  explore_psyCorona(df,"para03", n_bins=11) # Strangers and friends look at me critically
  
  # CONSPIRACY (I think that...)
  # ...many very important things happen in the world, which the public is never informed about.
  explore_psyCorona(df,"consp01", n_bins=11)
  # ...politicians usually do not tell us the true motives for their decisions.
  explore_psyCorona(df,"consp02", n_bins=11)
  # ...government agencies closely monitor all citizens.
  explore_psyCorona(df,"consp03", n_bins=11)
  
  # TIME VS MONEY
  explore_psyCorona(df,"timemoney", n_bins=7) # To what extent do you want more money than time?
  explore_psyCorona(df,"moneytime", n_bins=7) # To what extent do you want more time than money?
  
  # RELIGION
  explore_psyCorona(df,"relyesno") # Are you religious?
  table(df$godyesno)               # Do you believe in a God or Gods?
  
  # (For each of the following traits, please indicate how characteristic you think each trait is of the god or gods 
  # you believe in. If you believe in more than one god or deity, please respond based on one that is particularly 
  # important to you.)
  explore_psyCorona(df,"godpunish", 6)  # Punishing
  explore_psyCorona(df,"godforgive", 6) # Forgiving
  
  # SOLIDARITY (In general, how much do you trust each of the following to take the right measures to deal with the 
  # coronavirus pandemic? 
  explore_psyCorona(df,"trustgovctry",6)  # The government of your country
  explore_psyCorona(df,"trustgovstate",6) # Your community
  
  # CITIZENSHIP
  explore_psyCorona(df,"countrycitizen", n_bins=2) # Are you a citizen of this country ([QID217-ChoiceTextEntryValue])?
  explore_psyCorona(df,"citizen", n_bins=2) # Have you been a citizen of this country ([country see above]) since birth?
  explore_psyCorona(df,"immigrant", n_bins=2) # Do you consider yourself to be an immigrant?
  
  # DEMOGRAPHICS
  explore_psyCorona(df,"gender", n_bins=3) # What is your gender?
  explore_psyCorona(df,"age", n_bins=8)    # What is your age?
  explore_psyCorona(df,"edu", n_bins=7)    # What is your level of education?
  
  # Exploring coded_country
  sum(is.na(df$coded_country))
  
  ggplot(data = df, aes(x=coded_country)) +
    geom_histogram(stat="count") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}


if(FALSE) {
  
  # plotting all perentages of countries' counts in the data
  ggplot(data=country_percentages,aes(x=coded_country, y=perc_of_resp)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # plotting all perentages for countries with over 1% of total responses
  country_percentages %>% filter(perc_of_resp >1) %>% 
    ggplot(aes(x=coded_country, y=perc_of_resp)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ########## EXPLORING NAs ##########
  
  ### Exploring NAs per variable ###
  tab_desc <- descriptives(df_analyse)
  tab_desc <- tab_desc[order(tab_desc$name), ]
  missings <- is.na(df_analyse)
  by_var <- colSums(missings)
  
  
  # getting NAs for categorical variables (employstatus_xx, coronaClose_xx, houseLeaveWhy_xx)
  df_onlycat <- df_analyse %>% select(x1, contains("employstatus"), contains("coronaClose"), contains("houseLeaveWhy"))
  
  df_onlycat_long <- pivot_longer(data=df_onlycat, cols = colnames(df_onlycat[-1])) %>% 
    mutate(var_name = case_when(
      startsWith(name, "employstatus") ~ "employstatus",
      startsWith(name, "coronaclose") ~ "coronaclose",
      startsWith(name, "houseleavewhy") ~ "houseleavewhy"))
  
  # sanity check, indeed only the needed variables are kept
  unique(df_onlycat_long$var_name)
  
  # should only count as an NA if a participant did not pick any of the options in a given category group (across 
  # employstatus for example)
  df_onlycat_NAs <- df_onlycat_long %>% group_by(x1, var_name) %>% 
    summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
    group_by(var_name) %>% 
    summarise(percentage_NAs= 100*sum(responses_per_cat==0)/nrow(df))
  
  # houseLeaveWhy accounts for by far the largest amount of NAs amongs the 3 cat vars
  df_onlycat_NAs
  
  # getting NAs for non categorical vars
  df_non_cat <- df %>% 
    select(-contains("employstatus"), -contains("coronaClose"), -contains("houseLeaveWhy"))
  
  df_non_cat_NAs <- tibble(var_name = colnames(df_non_cat), 
                           percentage_NAs = 100*colSums(is.na(df_non_cat))/nrow(df_non_cat))
  
  # combining the non_cat and cat NA data
  df_NAs <- bind_rows(df_non_cat_NAs, df_onlycat_NAs)
  
  # histogram of NAs per variable 
  ggplot(df_NAs, aes(x=percentage_NAs)) +
    geom_histogram(bins=100)
  
  # we see that there are some variables that quite high percentage of NAs 
  ggplot(df_NAs, aes(x=var_name, y=percentage_NAs)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # plotting variables with missing percentage over 10% (remove them?)
  df_NAs %>% filter(percentage_NAs > 10) %>% 
    ggplot(aes(x=var_name, y=percentage_NAs)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### Exploring NAs per participant ###
  
  # for categorical vars
  df_onlycat_NAs_participants <- df_onlycat_long %>% group_by(x1, var_name) %>% 
    summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
    group_by(x1) %>% 
    summarise(percentage_NAs_cat= 100*sum(responses_per_cat==0)/ncol(df))
  
  # for non categorical vars
  df_non_cat_NAs_participants <- tibble(x1 = df_non_cat$x1, 
                                        percentage_NAs_non_cat = 100*rowSums(is.na(df_non_cat))/ncol(df_non_cat))
  
  # combining the categorical and non-categorical NAs percentages
  NAs_per_participant_df <- left_join(df_non_cat_NAs_participants, df_onlycat_NAs_participants, by = "x1") %>% 
    transmute(x1 = x1, total_NAs_perc = percentage_NAs_non_cat+percentage_NAs_cat)
  
  # histogram of NAs percentages on participant level
  ggplot(NAs_per_participant_df,aes(total_NAs_perc)) + geom_histogram(bins=100)
  
  # 1,526 participants with percentage of missing responses above 10%
  NAs_per_participant_df %>% dplyr::filter(total_NAs_perc>10) %>% nrow()
  100*(1526 / nrow(NAs_per_participant_df)) # that is only 2.7% of participants
  
  ### Preparing data for modelling and modelling itself moved to: psycorona_modelling.R
  
}

# Put startDate into bins (using 5 here because range is 5 weeks, may have to update this later)
df$startdate <- as.POSIXct(df$startdate)
df$time_ago <- as.numeric(head(df$startdate)-Sys.time())
df$startdate <- NULL
#df$startdate <- cut(df$startdate, breaks = 5, labels = c("w1", "w2", "w3", "w4", "w5"))

# Change the type to integer where appropriate
# first get the list of columns that already contain integer values 
int_columns_list = c()
for (col_n in 1:dim(df)[2]) {
  
  current_column_values <-  df[col_n]
  current_column_name <- colnames(df[col_n])
  
  if (is.numeric(current_column_values[1, ])) {
    if (all((current_column_values%%1 == 0) == TRUE, na.rm=T)) {
      int_columns_list = c(int_columns_list, current_column_name)}
  }
}
# change the type to integer for the appropriate columns 
df <- df %>% 
  mutate_at(int_columns_list, as.integer) %>% 
  dplyr::select(-x)

########## DATAWIDE EXPLORATION ##########


if(get_testing & all(df$train == FALSE)){
  write.csv(df, "testing.csv", row.names = FALSE)
}
if(!get_testing & all(df$train == TRUE)){
  write.csv(df, "training.csv", row.names = FALSE)
}
