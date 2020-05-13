# TODO: think about what to do with NAs
# TODO: merge with metadata
# TODO: optimise GLMNET furhter
# TODO: Try random forests
# TODO: think about other DVs
# TODO: think about the unbalance in DV
library(glmnet)
library(caTools)
library(tidyverse)
library(ggplot2)
if(!require("worcs")){
  library(remotes)
  install_github("cjvanlissa/worcs", upgrade ="never")
  library(worcs)
}
source("scripts/psyCorona_EDA_plot_function.R")
# read the data; Use relative paths so we can all reproduce
df_raw <- read_csv("data/RMD30_Caspar van Lissa_2020-05-08 11-50 CEST.csv") 
# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
names(df_raw) <- tolower(names(df_raw)) 

# Create training/testing split (training will be split into (cross-) validation samples)
set.seed(953007)
if(file.exists("split_sample.csv")){
  the_split <- read.csv("split_sample.csv")
  not_split <- which(!df_raw$x1 %in% the_split$x1)
  df_raw <- merge(df_raw, the_split, by = "x1", all.x = TRUE)
  if(length(not_split) > 0){
    new <- as.logical(rbinom(length(not_split), 1, .7))
    df_raw$train[not_split] <- new
    write.csv(df_raw[, c("x1", "train")], "split_sample.csv", row.names = FALSE)
  }
} else {
  df_raw$train <- as.logical(rbinom(nrow(df_raw), 1, .7))
  write.csv(df_raw[, c("x1", "train")], "split_sample.csv", row.names = FALSE)
}

# Drop testing cases; drop longitudinal waves
df <- df_raw[df_raw$train, !grepl("^w\\d", names(df_raw))]

# Descriptive stats
table_descriptives <- descriptives(df)

# Some variables might be spread out over several columns; check codebook
vars <- grep("_\\d$", table_descriptives$name[table_descriptives$unique < 4], value = TRUE)
# For example, coronaclose:
df$coronaclose <- apply(df[, grep("^coronaclose_\\d", names(df))], 1, function(i){which(i == 1)[1]})

# Recode: 1 = unemployed, 2 = 1-23 hours, 3 = 24-39 etc
df$employstatus <- apply(df[, c(grep("^employstatus_[45]$", names(df)), grep("^employstatus_[123]$", names(df)))], 1, function(i){which(i == 1)[1]})
# Who is still missing
still_missing <- is.na(df$employstatus)
# Classify as not working: Homemaker, Retired, Disabled, Student, or Volunteering
is_notworking <- apply(df[still_missing, grep("^employstatus_([6789]|10)$", names(df))], 1, function(i){any(i == 1)})
df$employstatus[still_missing][isTRUE(is_notworking)] <- 1

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

# for(this_var in unique(gsub("_\\d$", vars))){
#   this_var <- "employstatus"
#   df[[this_var]] <- apply(df[, grepl(paste0("^", this_var, "_\\d$"), names(df))], 1, function(x)which(x == 1))
# }

# Explore NAs
ggplot(table_descriptives, aes(x = missing)) + geom_density()

# Plot vars
plotdat <- df[, table_descriptives$name[table_descriptives$type == "numeric"]]
plotdat <- pivot_longer(plotdat, cols = names(plotdat))
# View only full screen. Consider plotting only vars with weird skew/kurtosis values
ggplot(plotdat, aes(x = value)) + geom_density() + facet_wrap(~name, scales = "free") + theme_bw()


# START/END DATE
# Exploring StartDate
explore_psyCorona(df,"StartDate")
startdate_NAs <- df[is.na(df$StartDate), ]
# Exploring EndDate
explore_psyCorona(df,"EndDate")


# AFFECT
# Exploring affAnx
# How did you feel over the last week? - Anxious
explore_psyCorona(df,"affanx", n_bins = 5)
# Exploring affBor
# How did you feel over the last week? - Bored
explore_psyCorona(df,"affBor", n_bins = 5)
# Exploring affCalm
# How did you feel over the last week? - Calm
explore_psyCorona(df,"affCalm", n_bins = 5)
# Exploring affContent
# How did you feel over the last week? - Content
explore_psyCorona(df,"affContent", n_bins = 5)
# Exploring affDepr
# How did you feel over the last week? - Depressed
explore_psyCorona(df,"affDepr", n_bins = 5)
# Exploring affEnerg
# How did you feel over the last week? - Energetic
explore_psyCorona(df,"affEnerg", n_bins = 5)
# Exploring affExc
# How did you feel over the last week? - Excited
explore_psyCorona(df,"affExc", n_bins = 5)
# Exploring affNerv
# How did you feel over the last week? - Nervous
explore_psyCorona(df,"affNerv", n_bins = 5)
# Exploring affInsp
# How did you feel over the last week? - Inspired
explore_psyCorona(df,"affInsp", n_bins = 5)
# Exploring affRel
# How did you feel over the last week? - Relaxed
explore_psyCorona(df,"affRel", n_bins = 5)


# LIKELIHOOD
# Exploring PLRAC19
# How likely is it that the following will happen to you in the next few months? - You will get infected with coronavirus.
explore_psyCorona(df,"PLRAC19", n_bins=8)
# Exploring PLRAEco
# How likely is it that the following will happen to you in the next few months? - Your personal situation will get worse due to economic consequences of coronavirus.# 205 NAs
explore_psyCorona(df,"PLRAEco", n_bins=8)


# SOCIETAL DISCONTENT
# Exploring disc01
# Agree or disagree: - I fear that things will go wrong in society.
explore_psyCorona(df,"disc01", n_bins=5)
# Exploring disc02
# Agree or disagree: - I feel concerned when I think about the future of society.
explore_psyCorona(df,"disc02", n_bins=5)
# Exploring disc03
# Agree or disagree: - I am satisfied with society.
explore_psyCorona(df,"disc03", n_bins=5)

# JOB INSECURITY
# Exploring jbInsec01
# Agree or disagree: - Chances are, I will soon lose my job.
# there is a -99 response (probably signifying that the there is no job to lose), 
# but I refrained from plotting a histogram for jbInsec
unique(df$jbInsec01)
sum(is.na(df$jbInsec01))
table(df$jbInsec01)
# Exploring jbInsec02
# Agree or disagree: - I am sure I can keep my job.
unique(df$jbInsec02)
sum(is.na(df$jbInsec02))
table(df$jbInsec02)
# Exploring jbInsec03
# Agree or disagree: - I feel insecure about the future of my job.
unique(df$jbInsec03)
sum(is.na(df$jbInsec03))
table(df$jbInsec03)
# Exploring jbInsec04
# Agree or disagree: - I already lost my job.
unique(df$jbInsec04)
sum(is.na(df$jbInsec04))
table(df$jbInsec04)
# Exploring jbInsec04
# Agree or disagree: - I already lost my job.
sum(is.na(df$employstatus_1))
table(df$jbInsec04)


# EMPLOEMENT STATUS
# Exploring dat_employee_status
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

# HOURS WORKED AND WORK CONTACT
# Exploring work_home_1
# In the last week, what percentage of your paid work time did you complete 
# from home? - Percentage of work completed from home
explore_psyCorona(df,"work_home_1", n_bins=100)
# Exploring socint
# In the last week, how often do you interact with others at work as part of your job?
explore_psyCorona(df,"socint", n_bins=5)


# PERCEIVED FINANCIAL STRAIN
# Exploring PFS01
# Agree or disagree: - I am financially strained.
explore_psyCorona(df,"PFS01", n_bins=5)
# Exploring PFS02
# Agree or disagree: - I often think about my current financial situation.
explore_psyCorona(df,"PFS02", n_bins=5)
# Exploring PFS03
# Agree or disagree: - Due to my financial situation, I have difficulties paying for my expenses.
explore_psyCorona(df,"PFS03", n_bins=5)


# DISEMPOWERMENT
# Exploring fail01
# Agree or disagree: - Not a lot is done for people like me in this country 
explore_psyCorona(df,"fail01", n_bins=5)
# Exploring fail02
# Agree or disagree: - If I compare people like me against other people in this country, my group is worse off.
explore_psyCorona(df,"fail02", n_bins=5)
# Exploring fail03
# Agree or disagree: - Recent events in society have increased my struggles in daily life.
explore_psyCorona(df,"fail03", n_bins=5)


# ISOLATION OFFLINE/ONLINE
# Exploring isoFriends_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...friends or relatives
explore_psyCorona(df,"isoFriends_inPerson", n_bins=8)
# Exploring isoOthPpl_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... - 
# In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...other people in general
explore_psyCorona(df,"isoOthPpl_inPerson", n_bins=8)
# Exploring isoFriends_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...immigrants
explore_psyCorona(df,"isoImmi_inPerson", n_bins=8)
# Exploring isoFriends_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - friends or relatives
explore_psyCorona(df,"isoFriends_online", n_bins=8)
# Exploring isoOthPpl_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - other people in general
explore_psyCorona(df,"isoOthPpl_online", n_bins=8)
# Exploring isoImmi_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - ...immigrants
explore_psyCorona(df,"isoImmi_online", n_bins=8)


# RELATIONSHIPS
# Exploring persRelSat
# In general, how satisfied are you with your personal relationships?
explore_psyCorona(df,"persRelSat", n_bins=10)
# Exploring discPers
# Do you have anyone with whom you can discuss very personal matters?
explore_psyCorona(df,"discPers", n_bins=3)


# LONELINESS
# Exploring lone01
# During the past week, did you... - ...feel lonely?
explore_psyCorona(df,"lone01", n_bins=5)
# Exploring lone02
# During the past week, did you... - ...feel isolated from others?
explore_psyCorona(df,"lone02", n_bins=5)
# Exploring lone03
# During the past week, did you... - ...feel left out?
explore_psyCorona(df,"lone03", n_bins=5)


# LIFE SATISFACTION
# Exploring happy
# In general, how happy would you say you are?
explore_psyCorona(df,"happy", n_bins=10)
# Exploring lifeSat
# In general, how satisfied are you with your life?
explore_psyCorona(df,"lifeSat", n_bins=6)
# Exploring MLQ
# My life has a clear sense of purpose.
explore_psyCorona(df,"MLQ", n_bins=11)

# TIGHTNESS
# Exploring tightNorms
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Have flexible social norms:9\: Have rigid social norms
explore_psyCorona(df,"tightNorms", n_bins=9)
# Exploring tightLoose 
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Be loose:9\: Be tight
explore_psyCorona(df,"tightLoose", n_bins=9)
# Exploring tightTreat
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Treat people who don’t conform to norms kindly:9\: 
# Treat people who don’t conform to norms harshly
explore_psyCorona(df,"tightTreat", n_bins=9)


# TIGHTNESS
# Exploring tightNorms
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Have flexible social norms:9\: Have rigid social norms
explore_psyCorona(df,"tightNorms", n_bins=9)
# Exploring tightLoose 
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Be loose:9\: Be tight
explore_psyCorona(df,"tightLoose", n_bins=9)
# Exploring tightTreat
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Treat people who don’t conform to norms kindly:9\: 
# Treat people who don’t conform to norms harshly
explore_psyCorona(df,"tightTreat", n_bins=9)

# PROBLEM_SOLVING
# Exploring probSolving01
# When dealing with stressful situations, what do you usually do? - I try to come up with a strategy about what to do.
explore_psyCorona(df,"probSolving01", n_bins=5)
# Exploring probSolving02
# When dealing with stressful situations, what do you usually do? - I make a plan of action.
explore_psyCorona(df,"probSolving02", n_bins=5)
# Exploring probSolving03
# When dealing with stressful situations, what do you usually do? - I think hard about what steps to take.
explore_psyCorona(df,"probSolving03", n_bins=5)
# Exploring posrefocus01
# When dealing with stressful situations, what do you usually do? - I distract myself to avoid thinking about the subject.
explore_psyCorona(df,"posrefocus01", n_bins=5)
# Exploring posrefocus02
# When dealing with stressful situations, what do you usually do? - I do things to distract myself from my thoughts and feelings.
explore_psyCorona(df,"posrefocus02", n_bins=5)
# Exploring posrefocus03
# When dealing with stressful situations, what do you usually do? - I force myself to think about something else.
explore_psyCorona(df,"posrefocus03", n_bins=5)

# CORONA
# Exploring C19Know
# How knowledgeable are you about the recent outbreak of Covid-19, commonly referred to as the Coronavirus, in this country
explore_psyCorona(df,"C19Know", n_bins=5)
# Exploring c19Hope
# Agree or disagree: - I have high hopes that the situation regarding coronavirus will improve.
explore_psyCorona(df,"c19Hope", n_bins=5)
# Exploring c19Eff
# Agree or disagree: - I think that this country ([QID217-ChoiceTextEntryValue]) is able to fight the Coronavirus.
explore_psyCorona(df,"c19Eff", n_bins=5)
# Exploring c19ProSo01
# I am willing to... - ...help others who suffer from coronavirus.
explore_psyCorona(df,"c19ProSo01", n_bins=7)
# Exploring c19ProSo02
# I am willing to... - ...make donations to help others that suffer from coronavirus.
explore_psyCorona(df,"c19ProSo02", n_bins=7)
# Exploring c19ProSo03
# I am willing to... - ...protect vulnerable groups from coronavirus even at my own expense.
explore_psyCorona(df,"c19ProSo03", n_bins=7)
# Exploring c19ProSo04
# I am willing to... - ...make personal sacrifices to prevent the spread of coronavirus.
explore_psyCorona(df,"c19ProSo04", n_bins=7)

# CORONA SELF-SERVING BEHAVIOUR
# Exploring c19perBeh01
# To minimize my chances of getting coronavirus, I... - ...wash my hands more often.
explore_psyCorona(df,"c19perBeh01", n_bins=7)
# Exploring c19perBeh02
# To minimize my chances of getting coronavirus, I... - ...avoid crowded spaces.
explore_psyCorona(df,"c19perBeh02", n_bins=7)
# Exploring c19perBeh03
# To minimize my chances of getting coronavirus, I... - ...put myself in quarantine.
explore_psyCorona(df,"c19perBeh03", n_bins=7)

# CORONA RADICAL ACTION
# Exploring c19RCA01
# I would sign a petition that supports... - ...mandatory vaccination once a vaccine has been developed for coronavirus.
explore_psyCorona(df,"c19RCA01", n_bins=7)
# Exploring c19RCA02
# I would sign a petition that supports... - ...mandatory quarantine for those that have 
# coronavirus and those that have been exposed to the virus.
explore_psyCorona(df,"c19RCA02", n_bins=7)
# Exploring c19RCA03
# I would sign a petition that supports... - ...reporting people who are suspected to have coronavirus.
explore_psyCorona(df,"c19RCA03", n_bins=7)

# CORONA PROXIMITY
# Exploring coronaClose
# Do you personally know anyone who currently has coronavirus? (click all that apply)
# coronaClose_1 Yes, myself
# coronaClose_2 Yes, a member of my family
# coronaClose_3 Yes, a close friend
# coronaClose_4 Yes, someone I know
# coronaClose_5 Yes, someone else
# coronaClose 6 Nom I dont know anyone
dat_corona_close <- df %>% dplyr::select(contains("coronaClose"))


# plot the counts of employement statuses
dat_corona_close %>% pivot_longer(cols=colnames(dat_corona_close)) %>% 
  group_by(name) %>% 
  summarise(status_count = sum(value, na.rm=T)) %>% 
  ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
  geom_bar(stat = "identity") 

# Sometimes more than 1 answer is selected SHOULD BE CLEANED BEFORE MODELLING
# find how many responses are left by each participant (0 signifies that the participant did not indicate any 
# employement status)
dat_corona_close$non_NA_number <- apply(dat_corona_close, 1, function(x) ncol(dat_corona_close) - sum(is.na(x)))
table(dat_corona_close$non_NA_number)

# CORONA COMMUNITY NORMS/RESPONSE
# Exploring c19NormShould
# Right now, people in my area... - ...should self-isolate and engage in social distancing.
explore_psyCorona(df,"c19NormShould", n_bins=7)
# Exploring c19NormDo
# Right now, people in my area... - ...do self-isolate and engage in social distancing.
explore_psyCorona(df,"c19NormDo", n_bins=7)
# Exploring c19IsStrict
# To what extent is your community…. - ...developing strict rules in response to the Coronavirus?
explore_psyCorona(df,"c19IsStrict", n_bins=6)
# Exploring c19IsPunish
# To what extent is your community…. - ...punishing people who deviate from the rules that 
# have been put in place in response to the Coronavirus?
explore_psyCorona(df,"c19IsPunish", n_bins=6)

#Exploring c19IsOrg
# To what extent is your community…. - ...well organized in responding to the Coronavirus?
explore_psyCorona(df,"c19IsOrg", n_bins=6)
# Exploring extC19Msg
# To what extent…. - ...are you getting clear, unambiguous messages about what to do about the Coronavirus?
explore_psyCorona(df,"extC19Msg", n_bins=6)


# CORONA CONSEQUENCES
# Exploring csqC19Contract
# How personally disturbing would you find the following possible consequences of the coronavirus? - Me contracting the virus
explore_psyCorona(df,"csqC19Contract", n_bins=5)
# Exploring csqEcoSuffer
# How personally disturbing would you find the following possible consequences of the coronavirus? - 
# Me suffering negative economic consequences
explore_psyCorona(df,"csqEcoSuffer", n_bins=5)
# Exploring csqCancPln
# How personally disturbing would you find the following possible consequences of the coronavirus? - Cancellation of my plans
explore_psyCorona(df,"csqCancPln", n_bins=5)
# Exploring csqLife
# How personally disturbing would you find the following possible consequences of the coronavirus? - Changing my life's routines
explore_psyCorona(df,"csqLife", n_bins=5)

# ECONOMY
# Exploring ecoKnow
# How knowledgeable are you about the potential economic and financial consequences of coronavirus in this country?
explore_psyCorona(df,"ecoKnow", n_bins=5)
# Exploring ecoHope
# Agree or disagree: - I have high hopes that the situation regarding the economic and financial consequences 
# of coronavirus will improve.
explore_psyCorona(df,"ecoHope", n_bins=7)
# Exploring ecoEff
# Agree or disagree: - I think that this country is able to fight the economic and financial consequences of coronavirus.
explore_psyCorona(df,"ecoEff", n_bins=7)

# ECONOMY PROSOCIAL BEHAVIOUR
# Exploring ecoProSo01
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...help others who suffer from such consequences.
explore_psyCorona(df,"ecoProSo01", n_bins=7)
# Exploring ecoProSo02
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...make donations to help others that suffer from such consequences.
explore_psyCorona(df,"ecoProSo02", n_bins=7)
# Exploring ecoProSo03
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...protect vulnerable groups from such consequences, even at my own expense.
explore_psyCorona(df,"ecoProSo03", n_bins=7)
# Exploring ecoProSo04
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...make personal sacrifices.
explore_psyCorona(df,"ecoProSo04", n_bins=7)


# ECONOMY RADICAL ACTION
# Exploring ecoRCA01
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports... 
# - ...higher taxes.
explore_psyCorona(df,"ecoRCA01", n_bins=7)
# Exploring ecoRCA02
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports...  
#  ...giving the government more authority over people.
explore_psyCorona(df,"ecoRCA02", n_bins=7)
# Exploring ecoRCA03
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports... 
# ...increased government spending.
explore_psyCorona(df,"ecoRCA03", n_bins=7)

# LEAVING THE HOUSE
# Exploring houseLeave
# In the past week, how often did you leave your home?
explore_psyCorona(df,"houseLeave", n_bins=4)

# houseLeaveWhy_1 Selected Choice I had to go to work.
# houseLeaveWhy_2 Selected Choice I had errands to run.
# houseLeaveWhy_4 Selected Choice For leisure purposes with others (e.g., meeting up with friends, seeing family, going to the cinema, etc.)
# houseLeaveWhy_6 Other, please specify: - Text
# houseLeaveWhy_7 Selected Choice For leisure purposes alone (e.g., running, going for a walk, etc.)
dat_house_leave <- df %>% dplyr::select(contains("houseLeaveWhy"))
# plot the counts of employement statuses
dat_house_leave %>% pivot_longer(cols=colnames(dat_house_leave)) %>% 
  group_by(name) %>% 
  summarise(status_count = sum(value, na.rm=T)) %>% 
  ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
  geom_bar(stat = "identity") 

# Sometimes more than 1 answer is selected SHOULD BE CLEANED BEFORE MODELLING
# find how many responses are left by each participant (0 signifies that the participant did not indicate any 
# employement status)
dat_house_leave$non_NA_number <- apply(dat_house_leave, 1, function(x) ncol(dat_house_leave) - sum(is.na(x)))
table(dat_house_leave$non_NA_number)

# BOREDOM
# Exploring bor01
# Indicate your agreement or disagreement with the following statements. - I wish time would go by faster.
explore_psyCorona(df,"bor01", n_bins=7)
# Exploring bor02
# Indicate your agreement or disagreement with the following statements. - Time is moving very slowly.
explore_psyCorona(df,"bor02", n_bins=7)
# Exploring bor03
# Indicate your agreement or disagreement with the following statements. - I feel in control of my time.
explore_psyCorona(df,"bor03", n_bins=7)

# TEMPORAL FOCUS
# Exploring tempFocPast
# Agree or disagree: - I replay memories of the past in my mind.
explore_psyCorona(df,"tempFocPast", n_bins=7)
# Exploring tempFocPres
# Agree or disagree: - I focus on what is currently happening in my life.
explore_psyCorona(df,"tempFocPres", n_bins=7)
# Exploring tempFocFut
# Agree or disagree: - I think about what my future has in store.
explore_psyCorona(df,"tempFocFut", n_bins=7)

# INTERGROUP CONTACT
# Exploring contactFulfill
# When you think about your contacts with immigrants last week, do these interactions prove to be helpful or hindering?
explore_psyCorona(df,"contactFulfill", n_bins=11)
# Exploring contactQual
# Thinking about your contacts with immigrants, how positive or negative did you experience them to be?
explore_psyCorona(df,"contactQual", n_bins=11)


# MIGRANT THREAT
# Exploring migrantThreat01
# Migrants who come to live here, generally... - take jobs away:create new jobs
explore_psyCorona(df,"migrantThreat01", n_bins=10)
# Exploring migrantThreat02
# Migrants who come to live here, generally... - undermine the cultural life:enrich the cultural life
explore_psyCorona(df,"migrantThreat02", n_bins=10)
# Exploring migrantThreat03
# Migrants who come to live here, generally... - make crime problems worse:make crime problems better
explore_psyCorona(df,"migrantThreat03", n_bins=10)
# Exploring migrantThreat04
# Migrants who come to live here, generally... - harm this country's culture:benefit this country's culture
explore_psyCorona(df,"migrantThreat04", n_bins=10)
# Exploring migrantThreat05
# Migrants who come to live here, generally... - bring diseases:help prevent diseases
explore_psyCorona(df,"migrantThreat05", n_bins=10)

# Exploring feelingTherm
# How would you rate: Migrants who come to live here
explore_psyCorona(df,"feelingTherm", n_bins=9)

# CORONA REFLECTION TASK
# Exploring CRT1
# You are presented with two policy options to combat the spread of coronavirus. 
# Your public health agency will enact one of the policies. Policy A has a 95% chance to prevent 1000 new infections. 
# Policy B has a 90% chance to prevent 5000 new infections.
explore_psyCorona(df,"CRT1", n_bins=2)
# Exploring CRT2
# Does the conclusion follow logically from the premises?
# Premises: 
#  - All infected people cough and have a fever
#  - Your neighbor coughs and has a fever
# Conclusion: Your neighbor is infected
explore_psyCorona(df,"CRT2", n_bins=2)
# Exploring CRT3
# Imagine that every time you leave your home you have a 50/50 chance of getting infected with coronavirus. 
# You have left your home 5 times recently and nothing has happened. The next time you leave your home, 
# do you think that:
explore_psyCorona(df,"CRT3", n_bins=3)

# Exploring idOverlap
# Choose the picture which best represents the closeness between the people of the country you currently live in and yourself.
explore_psyCorona(df,"idOverlap", n_bins=5)


# NEUROTICISM
# Exploring neuro01
# I see myself as someone who... - ...is very concerned.
explore_psyCorona(df,"neuro01", n_bins=7)
# Exploring neuro02
# I see myself as someone who... - ...easily gets nervous.
explore_psyCorona(df,"neuro02", n_bins=7)
# Exploring neuro03
# I see myself as someone who... - ...is relaxed, can easily deal with stress.
explore_psyCorona(df,"neuro03", n_bins=7)


# PARANOIA
# Exploring para01
# I need to be on my guard against others
explore_psyCorona(df,"para01", n_bins=11)
# Exploring para02
# People are trying to make me upset
explore_psyCorona(df,"para02", n_bins=11)
# Exploring para03
# Strangers and friends look at me critically
explore_psyCorona(df,"para03", n_bins=11)


# CONSPIRACY
# Exploring consp01
# I think that...... -
# … many very important things happen in the world, which the public is never informed about.
explore_psyCorona(df,"consp01", n_bins=11)
# Exploring consp02
# I think that...... -
# … politicians usually do not tell us the true motives for their decisions.
explore_psyCorona(df,"consp02", n_bins=11)
# Exploring consp03
# I think that...... -
# … government agencies closely monitor all citizens.
explore_psyCorona(df,"consp03", n_bins=11)

# TIME VS MONEY
# Exploring TimeMoney
# To what extent do you want more money than time?
explore_psyCorona(df,"TimeMoney", n_bins=7)
# Exploring MoneyTime
# To what extent do you want more time than money?
explore_psyCorona(df,"MoneyTime", n_bins=7)


# RELIGION
# Exploring relYesNo
# Are you religious?
explore_psyCorona(df,"relYesNo")
# Exploring godyesno
# Do you believe in a God or Gods?
table(df$godyesno)
# Exploring godPunish
# For each of the following traits, please indicate how characteristic you think each trait is of the god or gods 
#you believe in. If you believe in more than one god or deity, please respond based on one that is particularly 
#important to you. - Punishing
explore_psyCorona(df,"godPunish", 6)
# Exploring godForgive
# For each of the following traits, please indicate how characteristic you think each trait is of the god or gods 
#you believe in. If you believe in more than one god or deity, please respond based on one that is particularly 
#important to you. - Forgiving
explore_psyCorona(df,"godForgive", 6)

# SOLIDARITY
# Exploring trustGovCtry
# In general, how much do you trust each of the following to take the right measures to deal with the coronavirus pandemic? 
# - The government of your country
explore_psyCorona(df,"trustGovCtry",6)
# Exploring godyesno
# In general, how much do you trust each of the following to take the right measures to deal with the coronavirus pandemic? 
# - Your community
explore_psyCorona(df,"trustGovState",6)

# CITIZENSHIP
# Exploring countryCitizen
# Are you a citizen of this country ([QID217-ChoiceTextEntryValue])?
explore_psyCorona(df,"countryCitizen", n_bins=2)
# Exploring Citizen
# Have you been a citizen of this country ([QID217-ChoiceTextEntryValue]) since birth?
explore_psyCorona(df,"Citizen", n_bins=2)
# Exploring Immigrant
# Do you consider yourself to be an immigrant?
explore_psyCorona(df,"Immigrant", n_bins=2)


# DEMOGRAPHICS
# Exploring gender
# What is your gender?
explore_psyCorona(df,"gender", n_bins=3)
# Exploring age
# What is your age?
explore_psyCorona(df,"age", n_bins=8)
# Exploring edu
# What is your level of education?
explore_psyCorona(df,"edu", n_bins=7)

# Exploring coded_country
sum(is.na(df$coded_country))

ggplot(data = df, aes(x=coded_country)) +
  geom_histogram(stat="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

# Data Wide Exploration
##########################################################################################
# here I remove all of the columns that are not needed for analysis. I also remove the "ranking" features as I am not 
# sure how to use them in analysis. (MAYBE RE-ADD LATER)
df <- df %>% select(-EndDate, -PolOrX, -PolOrY, -PolOrCat, -language, -contains("rank"))

# getting percentags of countries' counts
country_percenteges <- df %>% group_by(coded_country) %>% 
  summarise(perc_of_resp = 100*(n()/nrow(df))) 

# plotting all perentages of countries' counts in the data
ggplot(data=country_percenteges,aes(x=coded_country, y=perc_of_resp)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# pllotting all perentages for countries with over 1% of total responses
country_percenteges %>% filter(perc_of_resp >1) %>% 
ggplot(aes(x=coded_country, y=perc_of_resp)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
###################################

# Exploring NAs deeper 
###################################
# Exploring NAs per variable 
####################
# getting NAs for categorical variables (employstatus_xx, coronaClose_xx, houseLeaveWhy_xx)
df_onlycat <- df %>% 
  select(X1, contains("employstatus"), contains("coronaClose"), contains("houseLeaveWhy"))

df_onlycat_long <- pivot_longer(data=df_onlycat, cols = colnames(df_onlycat[-1])) %>% 
  mutate(var_name = case_when(
    startsWith(name, "employstatus") ~ "employStatus",
    startsWith(name, "coronaClose") ~ "coronaClose",
    startsWith(name, "houseLeaveWhy") ~ "houseLeaveWhy"))

# sanity check, indeed only the needed variables are kept
unique(df_onlycat_long$var_name)
# should only count as an NA if a participant did not pick any of the options in a given category group (across 
# employstatus for example)
df_onlycat_NAs <- df_onlycat_long %>% group_by(X1, var_name) %>% 
  summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
  group_by(var_name) %>% 
  summarise(percentage_NAs= 100*sum(responses_per_cat==0)/nrow(df))

# houseLeaveWhy accounts for by far the largest amount of NAs amongs the 3 cat vars
df_onlycat_NAs

# getting NAs for non categorical vars
df_non_cat <- df %>% 
  select(-contains("employstatus"), -contains("coronaClose"), -contains("houseLeaveWhy"))

df_non_cat_NAs <- tibble(var_name = colnames(df_non_cat), percentage_NAs = 100*colSums(is.na(df_non_cat))/nrow(df_non_cat))

#combining the non_cat and cat NA data
####################
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
###################################

# Exploring NAs per participant
########################################
# for categorical vars
df_onlycat_NAs_participants <- df_onlycat_long %>% group_by(X1, var_name) %>% 
  summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
  group_by(X1) %>% 
  summarise(percentage_NAs_cat= 100*sum(responses_per_cat==0)/ncol(df))

# for non categorical vars
df_non_cat_NAs_participants <- tibble(X1 = df_non_cat$X1, percentage_NAs_non_cat = 100*rowSums(is.na(df_non_cat))/ncol(df_non_cat))
# combining the categorical and non-categorical NAs percentages
NAs_per_participant_df <- left_join(df_non_cat_NAs_participants, df_onlycat_NAs_participants, by="X1") %>% 
  transmute(X1 = X1, total_NAs_perc = percentage_NAs_non_cat+percentage_NAs_cat)

# histogram of NAs percentages on participant level
ggplot(NAs_per_participant_df,aes(total_NAs_perc)) +
         geom_histogram(bins=100)

# 1,526 participants with percentage of missing responses above 10%
NAs_per_participant_df %>% dplyr::filter(total_NAs_perc>10) %>% nrow()
# that is onlly 2.7% of participants 
100*(1526 / nrow(NAs_per_participant_df))
########################################

# Getting data ready for modelling 
################################################################################
# remove participants above the % treshold of missing values 
partic_NA_tres_perc <- 10 # 10%
partic_above_missing_tres <- NAs_per_participant_df %>% dplyr::filter(total_NAs_perc > partic_NA_tres_perc) 
partic_above_missing_tres

df_clean_partic <- df %>% dplyr::filter(!(X1 %in% partic_above_missing_tres$X1))

# remove features/variabls above the % treshold of missing values 
partic_NA_tres_var <- 15 # 15%
vars_above_missing_tres <- df_NAs %>% filter(percentage_NAs > partic_NA_tres_var) 
vars_above_missing_tres

df_clean_vars <- df_clean_partic[ ,!(colnames(df_clean_partic) %in% 
                                                               c(vars_above_missing_tres$var_name,
                                                                 "houseLeaveWhy_1",
                                                                  "houseLeaveWhy_2",
                                                                  "houseLeaveWhy_4",
                                                                  "houseLeaveWhy_6",
                                                                  "houseLeaveWhy_7"))]

# remove countries with less than a treshold % responses of total data 
###################
country_remove_treshold = .5 #1%
country_percenteges %>% filter(perc_of_resp > country_remove_treshold) %>%  .$coded_country
df_cleaned_countries <- df_clean_vars %>% filter(coded_country %in% (country_percenteges %>% filter(perc_of_resp >country_remove_treshold) %>%  .$coded_country))
# set to factor
df_cleaned_countries$coded_country <- factor(df_cleaned_countries$coded_country)
unique(df_cleaned_countries$coded_country)


# JBINSEC and GODYESNO have a -99 response which I am not sure about. So I removed them too.
# But may want to add them back later
#df_clean2 %>% select(contains("jbinsec"), contains("employstatus"))
df_cleaned <- df_cleaned_countries %>% select(-contains("jbinsec"), -godyesno)

# Getting data in the right format 
############################################
# factorize the rest of the categorical variables 
df_cleaned$gender <- factor(df_cleaned$gender)
df_cleaned$edu <- factor(df_cleaned$gender)
df_cleaned$age <- factor(df_cleaned$age)
df_cleaned$Immigrant <- factor(df_cleaned$Immigrant)
df_cleaned$Citizen <- factor(df_cleaned$Citizen)
df_cleaned$countryCitizen <- factor(df_cleaned$countryCitizen)
df_cleaned$relYesNo <- factor(df_cleaned$relYesNo)
df_cleaned$CRT1 <- factor(df_cleaned$CRT1)
df_cleaned$CRT2 <- factor(df_cleaned$CRT2)
df_cleaned$CRT3 <- factor(df_cleaned$CRT3)

# one hot encode with houseleaveWhy, coronaClose and employstatus, (add houseLeave here if it is being used in the future)
###################
vars_to_encode <- dplyr::select(df_cleaned, contains("employstatus"), contains("coronaClose")) %>% colnames()

df_cleaned_encoded <- df_cleaned %>% 
  mutate_at(vars_to_encode, funs(ifelse(is.na(.), 0,1)))

# mutate employstats_xx into factors (add houseLeave later if needed)
df_cleaned_encoded <- df_cleaned_encoded %>% mutate_at(vars_to_encode[0:10], funs(factor(.)))
#check
df_cleaned_encoded$employstatus_1

# remove participants who did not answer any coronaClose options. As this is will make up the DV, we don't wnat NAs here
df_cleaned_encoded <- df_cleaned_encoded %>% filter_at(vars_to_encode[11:16], any_vars(. != 0))

# sanity check, checking if the NAs were replaced to 0 for correct variables/participants 
temp1 <- df_cleaned[1:10, ]
temp2 <- df_cleaned_encoded[1:10, ]

# sanity check, check that every participant picked at least one option on coronaClose measure
df_cleaned_encoded %>% filter(coronaClose_6==0 &coronaClose_1==0&coronaClose_2==0&coronaClose_3==0&
                                             coronaClose_4==0&coronaClose_5==0) %>% nrow()


# remove participants who answered they dont know anyone with corona but also that they do (unreliable)
df_cleaned_reliable <- df_cleaned_encoded %>% 
  filter(!(coronaClose_6==1 & (coronaClose_1==1|coronaClose_2==1|coronaClose_3==1|coronaClose_4==1|coronaClose_5==1)))

# check that indeed every response coronaClose_6 (dont know anyone with covid) means that there is not a response in one of the 
# rest of coronaClose_xx. 
df_cleaned_reliable %>% filter((coronaClose_6==0 & (coronaClose_1==1|coronaClose_2==1|coronaClose_3==1|
                                                    coronaClose_4==1|coronaClose_5==1))) %>% nrow()

df_cleaned_reliable %>% filter((coronaClose_6==1 & (coronaClose_1==0|coronaClose_2==0|coronaClose_3==0|
                                                    coronaClose_4==0|coronaClose_5==0))) %>% nrow()
# check
(13019 + 36303) == nrow(df_cleaned_reliable)

# plotting coronaClose after cleaning and only keeping reliable responses
temp4 <- df_cleaned_reliable %>% dplyr::select(contains("coronaClose")) %>% 
  pivot_longer(cols=c("coronaClose_6", "coronaClose_1", "coronaClose_2", "coronaClose_3",
                      "coronaClose_4", "coronaClose_5")) 
# distribution of coronaClose after cleaning and only keeping reliable responses
temp4 %>% 
  group_by(name) %>% 
  summarise(status_count = sum(as.numeric(value), na.rm=T)) %>% 
  ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
  geom_bar(stat = "identity") 

# MAKE the dependent variable
###################
# OPTION 1:Predicting whether whether people know anyone who has COVID (including themselves)
###################
# I chose to set a "coronaKnow" variable which is 1 if the participants knows anyone with corona (including theirself) and
# is 0 if they do not. It's arguable if this is best, but gives us the most positive cases to work with.
df_cleaned_reliable <- df_cleaned_reliable %>% mutate(knowCorona = ifelse(coronaClose_6 ==1, 0, 1))
df_cleaned_reliable$knowCorona <- factor(df_cleaned_reliable$knowCorona)

df_cleaned_reliable$knowCorona
table(df_cleaned_reliable$knowCorona)
# remove coronaclose variables as they are not needed for modelling anymore. all the info we need from them is in the
# knowCorona variable
df_ready <- df_cleaned_reliable %>% 
  dplyr::select(-coronaClose_1,-coronaClose_2,  -coronaClose_3,  -coronaClose_4,  -coronaClose_5, -coronaClose_6)

# put startDate into bins. The range is 5 weeks, so I will use 5 bins
###################
df_ready$StartDate <- cut(df_ready$StartDate,breaks=5, 
                                      labels=c("week_1", "week_2", "week_3", "week_4", "week_5"))

###################
# OPTION 2:Predicting whether whether people know anyone who has COVID (including themselves)
###################
#1st isoFriends_online
dat_dv_isolation <- df_cleaned_reliable
dat_dv_isolation$StartDate <- cut(dat_dv_isolation$StartDate,breaks=5, 
                                      labels=c("week_1", "week_2", "week_3", "week_4", "week_5"))

df_ready <- df_ready %>% mutate_at(vars_to_encode[11:16], funs(factor(.)))
df_ready$coronaClose_1

# Modelling
#########################################################
# for glmnet Lasso we cant have any NAs, so I remove all rows with NAs 
df_ready_noNA <- drop_na(df_ready)
# split into train test
set.seed(300)
sampling_split <- caTools::sample.split(df_ready_noNA$X1, SplitRatio = .75)
train_df <- df_ready_noNA[sampling_split, ]
test_df <- df_ready_noNA[!sampling_split, ]

# transform to matrix 
train_matrix_x <- model.matrix(coronaKnow ~ .-1, data=select(train_df, -X1))
# extract DV
y_train = as.factor(train_df$coronaKnow)


lasso_only_questionnaire_knowCorona <- cv.glmnet(x=train_matrix_x,y=y_train, family = "binomial", 
                                                 type.measure = 'auc', alpha = 1, nfolds=5)

plot(lasso_only_questionnaire_knowCorona)


# Evaluating the model
#########################################################
mean(predict(lasso_only_questionnaire_knowCorona, newx=train_matrix_x, type="class", 
             s=lasso_only_questionnaire_knowCorona$lambda.1se) == y_train)

lasso_preds <- predict(lasso_only_questionnaire_knowCorona, newx=train_matrix_x, type="class",
                       s=lasso_only_questionnaire_knowCorona$lambda.1se)

lasso_preds_processed <- as.factor(unname(lasso_preds[,1]))

# calculate classification metrics 
precision <- posPredValue(lasso_preds_processed, y_train, positive="1")
precision
recall <- sensitivity(lasso_preds_processed, y_train, positive="1")
recall
F1 <- (2 * precision * recall) / (precision + recall)
F1

# get the coefficients and make them into a tibble
lasso_coefs <- coef.glmnet(lasso_only_questionnaire_knowCorona, s=lasso_only_questionnaire_knowCorona$lambda.min)
coefs_df <- tibble(var_name = names(lasso_coefs[,1]), coef = unname(lasso_coefs[,1]))
# plot coefficients above .3
coefs_df %>% filter(abs(coef) > 0.3) %>% 
  ggplot(aes(x=var_name, y=coef)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))







