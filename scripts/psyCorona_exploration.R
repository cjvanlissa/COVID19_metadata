# TODO: Calculate NA statistics across the data and maybe by each country
# TODO: Turn NAs to 0 and one hot encode where necessary
# TODO: Prepare data for analsis and consider countries with low resposne counts, remove not needed columns, put 
# statDate into bins.
# TODO: play with some initial modelling using just the data form the survey 



library(tidyverse)
library(ggplot2)
source("scripts/psyCorona_EDA_plot_function.R")
# read the data
psy_data <- read_csv("~/Desktop/PsyCorona_DatavsScience/PsyCorona_Modelling/psycorona_data.csv")
# get a list of all variables wihtout the w_ prefix
all_vars_without_ws <- str_remove(colnames(psy_data),"w1_") %>% 
  str_remove("w2_") %>% 
  str_remove("w3_") %>% 
  str_remove("w4_")
# get unique variables
unique_vars <- unique(all_vars_without_ws)
# get df with only the baseline responses
dat_first_resp <- psy_data[ ,colnames(psy_data) %in% unique_vars]


# START/END DATE
# Exploring StartDate
explore_psyCorona(dat_first_resp,"StartDate")
startdate_NAs <- dat_first_resp[is.na(dat_first_resp$StartDate), ]
# Exploring EndDate
explore_psyCorona(dat_first_resp,"EndDate")


# AFFECT
# Exploring affAnx
# How did you feel over the last week? - Anxious
explore_psyCorona(dat_first_resp,"affAnx", n_bins = 5)
# Exploring affBor
# How did you feel over the last week? - Bored
explore_psyCorona(dat_first_resp,"affBor", n_bins = 5)
# Exploring affCalm
# How did you feel over the last week? - Calm
explore_psyCorona(dat_first_resp,"affCalm", n_bins = 5)
# Exploring affContent
# How did you feel over the last week? - Content
explore_psyCorona(dat_first_resp,"affContent", n_bins = 5)
# Exploring affDepr
# How did you feel over the last week? - Depressed
explore_psyCorona(dat_first_resp,"affDepr", n_bins = 5)
# Exploring affEnerg
# How did you feel over the last week? - Energetic
explore_psyCorona(dat_first_resp,"affEnerg", n_bins = 5)
# Exploring affExc
# How did you feel over the last week? - Excited
explore_psyCorona(dat_first_resp,"affExc", n_bins = 5)
# Exploring affNerv
# How did you feel over the last week? - Nervous
explore_psyCorona(dat_first_resp,"affNerv", n_bins = 5)
# Exploring affInsp
# How did you feel over the last week? - Inspired
explore_psyCorona(dat_first_resp,"affInsp", n_bins = 5)
# Exploring affRel
# How did you feel over the last week? - Relaxed
explore_psyCorona(dat_first_resp,"affRel", n_bins = 5)


# LIKELIHOOD
# Exploring PLRAC19
# How likely is it that the following will happen to you in the next few months? - You will get infected with coronavirus.
explore_psyCorona(dat_first_resp,"PLRAC19", n_bins=8)
# Exploring PLRAEco
# How likely is it that the following will happen to you in the next few months? - Your personal situation will get worse due to economic consequences of coronavirus.# 205 NAs
explore_psyCorona(dat_first_resp,"PLRAEco", n_bins=8)


# SOCIETAL DISCONTENT
# Exploring disc01
# Agree or disagree: - I fear that things will go wrong in society.
explore_psyCorona(dat_first_resp,"disc01", n_bins=5)
# Exploring disc02
# Agree or disagree: - I feel concerned when I think about the future of society.
explore_psyCorona(dat_first_resp,"disc02", n_bins=5)
# Exploring disc03
# Agree or disagree: - I am satisfied with society.
explore_psyCorona(dat_first_resp,"disc03", n_bins=5)

# JOB INSECURITY
# Exploring jbInsec01
# Agree or disagree: - Chances are, I will soon lose my job.
# there is a -99 response (probably signifying that the there is no job to lose), 
# but I refrained from plotting a histogram for jbInsec
unique(dat_first_resp$jbInsec01)
sum(is.na(dat_first_resp$jbInsec01))
table(dat_first_resp$jbInsec01)
# Exploring jbInsec02
# Agree or disagree: - I am sure I can keep my job.
unique(dat_first_resp$jbInsec02)
sum(is.na(dat_first_resp$jbInsec02))
table(dat_first_resp$jbInsec02)
# Exploring jbInsec03
# Agree or disagree: - I feel insecure about the future of my job.
unique(dat_first_resp$jbInsec03)
sum(is.na(dat_first_resp$jbInsec03))
table(dat_first_resp$jbInsec03)
# Exploring jbInsec04
# Agree or disagree: - I already lost my job.
unique(dat_first_resp$jbInsec04)
sum(is.na(dat_first_resp$jbInsec04))
table(dat_first_resp$jbInsec04)
# Exploring jbInsec04
# Agree or disagree: - I already lost my job.
sum(is.na(dat_first_resp$employstatus_1))
table(dat_first_resp$jbInsec04)


# EMPLOEMENT STATUS
# Exploring dat_employee_status
# Which of the following categories best describes your employment status during the last week (multiple may apply)? 
# dat_employee_status_1 is working 1-23 hours per week
# dat_employee_status_2 is working 24-39 hours per week
# dat_employee_status_3 is working 40 or more hours per week
# dat_employee_status_4 is Not employed, looking for work
# dat_employee_status_5 is Not employed, not looking for work
# dat_employee_status_6 is Homemaker
# dat_employee_status_7 is Retired
# dat_employee_status_8 is Disabled, not able to work
# dat_employee_status_9 is Student
# dat_employee_status_10 is Volunteering
dat_employee_status <- dat_first_resp %>% dplyr::select(contains("employstatus"))
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
explore_psyCorona(dat_first_resp,"work_home_1", n_bins=100)
# Exploring socint
# In the last week, how often do you interact with others at work as part of your job?
explore_psyCorona(dat_first_resp,"socint", n_bins=5)


# PERCEIVED FINANCIAL STRAIN
# Exploring PFS01
# Agree or disagree: - I am financially strained.
explore_psyCorona(dat_first_resp,"PFS01", n_bins=5)
# Exploring PFS02
# Agree or disagree: - I often think about my current financial situation.
explore_psyCorona(dat_first_resp,"PFS02", n_bins=5)
# Exploring PFS03
# Agree or disagree: - Due to my financial situation, I have difficulties paying for my expenses.
explore_psyCorona(dat_first_resp,"PFS03", n_bins=5)


# DISEMPOWERMENT
# Exploring fail01
# Agree or disagree: - Not a lot is done for people like me in this country 
explore_psyCorona(dat_first_resp,"fail01", n_bins=5)
# Exploring fail02
# Agree or disagree: - If I compare people like me against other people in this country, my group is worse off.
explore_psyCorona(dat_first_resp,"fail02", n_bins=5)
# Exploring fail03
# Agree or disagree: - Recent events in society have increased my struggles in daily life.
explore_psyCorona(dat_first_resp,"fail03", n_bins=5)


# ISOLATION OFFLINE/ONLINE
# Exploring isoFriends_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...friends or relatives
explore_psyCorona(dat_first_resp,"isoFriends_inPerson", n_bins=8)
# Exploring isoOthPpl_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... - 
# In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...other people in general
explore_psyCorona(dat_first_resp,"isoOthPpl_inPerson", n_bins=8)
# Exploring isoFriends_inPerson
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have in-person (face-to-face) contact with ... - ...immigrants
explore_psyCorona(dat_first_resp,"isoImmi_inPerson", n_bins=8)
# Exploring isoFriends_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - friends or relatives
explore_psyCorona(dat_first_resp,"isoFriends_online", n_bins=8)
# Exploring isoOthPpl_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - other people in general
explore_psyCorona(dat_first_resp,"isoOthPpl_online", n_bins=8)
# Exploring isoImmi_online
# In the past 7 days, how much social contact have you had with people who live outside your househ... 
# - In the past 7 days, how many days did you have online (video or voice)  contact with ... - ...immigrants
explore_psyCorona(dat_first_resp,"isoImmi_online", n_bins=8)


# RELATIONSHIPS
# Exploring persRelSat
# In general, how satisfied are you with your personal relationships?
explore_psyCorona(dat_first_resp,"persRelSat", n_bins=10)
# Exploring discPers
# Do you have anyone with whom you can discuss very personal matters?
explore_psyCorona(dat_first_resp,"discPers", n_bins=3)


# LONELINESS
# Exploring lone01
# During the past week, did you... - ...feel lonely?
explore_psyCorona(dat_first_resp,"lone01", n_bins=5)
# Exploring lone02
# During the past week, did you... - ...feel isolated from others?
explore_psyCorona(dat_first_resp,"lone02", n_bins=5)
# Exploring lone03
# During the past week, did you... - ...feel left out?
explore_psyCorona(dat_first_resp,"lone03", n_bins=5)


# LIFE SATISFACTION
# Exploring happy
# In general, how happy would you say you are?
explore_psyCorona(dat_first_resp,"happy", n_bins=10)
# Exploring lifeSat
# In general, how satisfied are you with your life?
explore_psyCorona(dat_first_resp,"lifeSat", n_bins=6)
# Exploring MLQ
# My life has a clear sense of purpose.
explore_psyCorona(dat_first_resp,"MLQ", n_bins=11)

# TIGHTNESS
# Exploring tightNorms
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Have flexible social norms:9\: Have rigid social norms
explore_psyCorona(dat_first_resp,"tightNorms", n_bins=9)
# Exploring tightLoose 
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Be loose:9\: Be tight
explore_psyCorona(dat_first_resp,"tightLoose", n_bins=9)
# Exploring tightTreat
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Treat people who don’t conform to norms kindly:9\: 
# Treat people who don’t conform to norms harshly
explore_psyCorona(dat_first_resp,"tightTreat", n_bins=9)


# TIGHTNESS
# Exploring tightNorms
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Have flexible social norms:9\: Have rigid social norms
explore_psyCorona(dat_first_resp,"tightNorms", n_bins=9)
# Exploring tightLoose 
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Be loose:9\: Be tight
explore_psyCorona(dat_first_resp,"tightLoose", n_bins=9)
# Exploring tightTreat
# To what extent do you think that the country you currently live in should have the following 
# characteristics right now? - 1\: Treat people who don’t conform to norms kindly:9\: 
# Treat people who don’t conform to norms harshly
explore_psyCorona(dat_first_resp,"tightTreat", n_bins=9)

# PROBLEM_SOLVING
# Exploring probSolving01
# When dealing with stressful situations, what do you usually do? - I try to come up with a strategy about what to do.
explore_psyCorona(dat_first_resp,"probSolving01", n_bins=5)
# Exploring probSolving02
# When dealing with stressful situations, what do you usually do? - I make a plan of action.
explore_psyCorona(dat_first_resp,"probSolving02", n_bins=5)
# Exploring probSolving03
# When dealing with stressful situations, what do you usually do? - I think hard about what steps to take.
explore_psyCorona(dat_first_resp,"probSolving03", n_bins=5)
# Exploring posrefocus01
# When dealing with stressful situations, what do you usually do? - I distract myself to avoid thinking about the subject.
explore_psyCorona(dat_first_resp,"posrefocus01", n_bins=5)
# Exploring posrefocus02
# When dealing with stressful situations, what do you usually do? - I do things to distract myself from my thoughts and feelings.
explore_psyCorona(dat_first_resp,"posrefocus02", n_bins=5)
# Exploring posrefocus03
# When dealing with stressful situations, what do you usually do? - I force myself to think about something else.
explore_psyCorona(dat_first_resp,"posrefocus03", n_bins=5)

# CORONA
# Exploring C19Know
# How knowledgeable are you about the recent outbreak of Covid-19, commonly referred to as the Coronavirus, in this country
explore_psyCorona(dat_first_resp,"C19Know", n_bins=5)
# Exploring c19Hope
# Agree or disagree: - I have high hopes that the situation regarding coronavirus will improve.
explore_psyCorona(dat_first_resp,"c19Hope", n_bins=5)
# Exploring c19Eff
# Agree or disagree: - I think that this country ([QID217-ChoiceTextEntryValue]) is able to fight the Coronavirus.
explore_psyCorona(dat_first_resp,"c19Eff", n_bins=5)
# Exploring c19ProSo01
# I am willing to... - ...help others who suffer from coronavirus.
explore_psyCorona(dat_first_resp,"c19ProSo01", n_bins=7)
# Exploring c19ProSo02
# I am willing to... - ...make donations to help others that suffer from coronavirus.
explore_psyCorona(dat_first_resp,"c19ProSo02", n_bins=7)
# Exploring c19ProSo03
# I am willing to... - ...protect vulnerable groups from coronavirus even at my own expense.
explore_psyCorona(dat_first_resp,"c19ProSo03", n_bins=7)
# Exploring c19ProSo04
# I am willing to... - ...make personal sacrifices to prevent the spread of coronavirus.
explore_psyCorona(dat_first_resp,"c19ProSo04", n_bins=7)

# CORONA SELF-SERVING BEHAVIOUR
# Exploring c19perBeh01
# To minimize my chances of getting coronavirus, I... - ...wash my hands more often.
explore_psyCorona(dat_first_resp,"c19perBeh01", n_bins=7)
# Exploring c19perBeh02
# To minimize my chances of getting coronavirus, I... - ...avoid crowded spaces.
explore_psyCorona(dat_first_resp,"c19perBeh02", n_bins=7)
# Exploring c19perBeh03
# To minimize my chances of getting coronavirus, I... - ...put myself in quarantine.
explore_psyCorona(dat_first_resp,"c19perBeh03", n_bins=7)

# CORONA RADICAL ACTION
# Exploring c19RCA01
# I would sign a petition that supports... - ...mandatory vaccination once a vaccine has been developed for coronavirus.
explore_psyCorona(dat_first_resp,"c19RCA01", n_bins=7)
# Exploring c19RCA02
# I would sign a petition that supports... - ...mandatory quarantine for those that have 
# coronavirus and those that have been exposed to the virus.
explore_psyCorona(dat_first_resp,"c19RCA02", n_bins=7)
# Exploring c19RCA03
# I would sign a petition that supports... - ...reporting people who are suspected to have coronavirus.
explore_psyCorona(dat_first_resp,"c19RCA03", n_bins=7)

# CORONA PROXIMITY
# Exploring coronaClose
# Do you personally know anyone who currently has coronavirus? (click all that apply)
# coronaClose_1 Yes, myself
# coronaClose_2 Yes, a member of my family
# coronaClose_3 Yes, a close friend
# coronaClose_4 Yes, someone I know
# coronaClose_5 Yes, someone else
# coronaClose 6 Nom I dont know anyone
dat_corona_close <- dat_first_resp %>% dplyr::select(contains("coronaClose"))
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
explore_psyCorona(dat_first_resp,"c19NormShould", n_bins=7)
# Exploring c19NormDo
# Right now, people in my area... - ...do self-isolate and engage in social distancing.
explore_psyCorona(dat_first_resp,"c19NormDo", n_bins=7)
# Exploring c19IsStrict
# To what extent is your community…. - ...developing strict rules in response to the Coronavirus?
explore_psyCorona(dat_first_resp,"c19IsStrict", n_bins=6)
# Exploring c19IsPunish
# To what extent is your community…. - ...punishing people who deviate from the rules that 
# have been put in place in response to the Coronavirus?
explore_psyCorona(dat_first_resp,"c19IsPunish", n_bins=6)

#Exploring c19IsOrg
# To what extent is your community…. - ...well organized in responding to the Coronavirus?
explore_psyCorona(dat_first_resp,"c19IsOrg", n_bins=6)
# Exploring extC19Msg
# To what extent…. - ...are you getting clear, unambiguous messages about what to do about the Coronavirus?
explore_psyCorona(dat_first_resp,"extC19Msg", n_bins=6)


# CORONA CONSEQUENCES
# Exploring csqC19Contract
# How personally disturbing would you find the following possible consequences of the coronavirus? - Me contracting the virus
explore_psyCorona(dat_first_resp,"csqC19Contract", n_bins=5)
# Exploring csqEcoSuffer
# How personally disturbing would you find the following possible consequences of the coronavirus? - 
# Me suffering negative economic consequences
explore_psyCorona(dat_first_resp,"csqEcoSuffer", n_bins=5)
# Exploring csqCancPln
# How personally disturbing would you find the following possible consequences of the coronavirus? - Cancellation of my plans
explore_psyCorona(dat_first_resp,"csqCancPln", n_bins=5)
# Exploring csqLife
# How personally disturbing would you find the following possible consequences of the coronavirus? - Changing my life's routines
explore_psyCorona(dat_first_resp,"csqLife", n_bins=5)

# ECONOMY
# Exploring ecoKnow
# How knowledgeable are you about the potential economic and financial consequences of coronavirus in this country?
explore_psyCorona(dat_first_resp,"ecoKnow", n_bins=5)
# Exploring ecoHope
# Agree or disagree: - I have high hopes that the situation regarding the economic and financial consequences 
# of coronavirus will improve.
explore_psyCorona(dat_first_resp,"ecoHope", n_bins=7)
# Exploring ecoEff
# Agree or disagree: - I think that this country is able to fight the economic and financial consequences of coronavirus.
explore_psyCorona(dat_first_resp,"ecoEff", n_bins=7)

# ECONOMY PROSOCIAL BEHAVIOUR
# Exploring ecoProSo01
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...help others who suffer from such consequences.
explore_psyCorona(dat_first_resp,"ecoProSo01", n_bins=7)
# Exploring ecoProSo02
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...make donations to help others that suffer from such consequences.
explore_psyCorona(dat_first_resp,"ecoProSo02", n_bins=7)
# Exploring ecoProSo03
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...protect vulnerable groups from such consequences, even at my own expense.
explore_psyCorona(dat_first_resp,"ecoProSo03", n_bins=7)
# Exploring ecoProSo04
# To help with the economic and financial consequences of coronavirus, I am willing to... - 
# ...make personal sacrifices.
explore_psyCorona(dat_first_resp,"ecoProSo04", n_bins=7)


# ECONOMY RADICAL ACTION
# Exploring ecoRCA01
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports... 
# - ...higher taxes.
explore_psyCorona(dat_first_resp,"ecoRCA01", n_bins=7)
# Exploring ecoRCA02
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports...  
#  ...giving the government more authority over people.
explore_psyCorona(dat_first_resp,"ecoRCA02", n_bins=7)
# Exploring ecoRCA03
# If it would alleviate the economic and financial consequences of coronavirus, I would sign a petition that supports... 
# ...increased government spending.
explore_psyCorona(dat_first_resp,"ecoRCA03", n_bins=7)

# LEAVING THE HOUSE
# Exploring houseLeave
# In the past week, how often did you leave your home?
explore_psyCorona(dat_first_resp,"houseLeave", n_bins=4)

# coronaClose_1 Selected Choice I had to go to work.
# coronaClose_2 Selected Choice I had errands to run.
# coronaClose_4 Selected Choice For leisure purposes with others (e.g., meeting up with friends, seeing family, going to the cinema, etc.)
# coronaClose_6 Other, please specify: - Text
# coronaClose_7 Selected Choice For leisure purposes alone (e.g., running, going for a walk, etc.)
dat_house_leave <- dat_first_resp %>% dplyr::select(contains("houseLeaveWhy"))
# plot the counts of employement statuses
dat_house_leave %>% pivot_longer(cols=colnames(dat_house_leave)) %>% 
  group_by(name) %>% 
  summarise(status_count = sum(value, na.rm=T)) %>% 
  ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
  geom_bar(stat = "identity") 

# Sometimes more than 1 answer is selected SHOULD BE CLEANED BEFORE MODELLING
# find how many responses are left by each participant (0 signifies that the participant did not indicate any 
# employement status)
dat_house_leave$non_NA_number <- apply(dat_corona_close, 1, function(x) ncol(dat_house_leave) - sum(is.na(x)))
table(dat_house_leave$non_NA_number)

# BOREDOM
# Exploring bor01
# Indicate your agreement or disagreement with the following statements. - I wish time would go by faster.
explore_psyCorona(dat_first_resp,"bor01", n_bins=7)
# Exploring bor02
# Indicate your agreement or disagreement with the following statements. - Time is moving very slowly.
explore_psyCorona(dat_first_resp,"bor02", n_bins=7)
# Exploring bor03
# Indicate your agreement or disagreement with the following statements. - I feel in control of my time.
explore_psyCorona(dat_first_resp,"bor03", n_bins=7)

# TEMPORAL FOCUS
# Exploring tempFocPast
# Agree or disagree: - I replay memories of the past in my mind.
explore_psyCorona(dat_first_resp,"tempFocPast", n_bins=7)
# Exploring tempFocPres
# Agree or disagree: - I focus on what is currently happening in my life.
explore_psyCorona(dat_first_resp,"tempFocPres", n_bins=7)
# Exploring tempFocFut
# Agree or disagree: - I think about what my future has in store.
explore_psyCorona(dat_first_resp,"tempFocFut", n_bins=7)

# INTERGROUP CONTACT
# Exploring contactFulfill
# When you think about your contacts with immigrants last week, do these interactions prove to be helpful or hindering?
explore_psyCorona(dat_first_resp,"contactFulfill", n_bins=11)
# Exploring contactQual
# Thinking about your contacts with immigrants, how positive or negative did you experience them to be?
explore_psyCorona(dat_first_resp,"contactQual", n_bins=11)


# MIGRANT THREAT
# Exploring migrantThreat01
# Migrants who come to live here, generally... - take jobs away:create new jobs
explore_psyCorona(dat_first_resp,"migrantThreat01", n_bins=10)
# Exploring migrantThreat02
# Migrants who come to live here, generally... - undermine the cultural life:enrich the cultural life
explore_psyCorona(dat_first_resp,"migrantThreat02", n_bins=10)
# Exploring migrantThreat03
# Migrants who come to live here, generally... - make crime problems worse:make crime problems better
explore_psyCorona(dat_first_resp,"migrantThreat03", n_bins=10)
# Exploring migrantThreat04
# Migrants who come to live here, generally... - harm this country's culture:benefit this country's culture
explore_psyCorona(dat_first_resp,"migrantThreat04", n_bins=10)
# Exploring migrantThreat05
# Migrants who come to live here, generally... - bring diseases:help prevent diseases
explore_psyCorona(dat_first_resp,"migrantThreat05", n_bins=10)

# Exploring feelingTherm
# How would you rate: Migrants who come to live here
explore_psyCorona(dat_first_resp,"feelingTherm", n_bins=9)

# CORONA REFLECTION TASK
# Exploring CRT1
# You are presented with two policy options to combat the spread of coronavirus. 
# Your public health agency will enact one of the policies. Policy A has a 95% chance to prevent 1000 new infections. 
# Policy B has a 90% chance to prevent 5000 new infections.
explore_psyCorona(dat_first_resp,"CRT1", n_bins=2)
# Exploring CRT2
# Does the conclusion follow logically from the premises?
# Premises: 
#  - All infected people cough and have a fever
#  - Your neighbor coughs and has a fever
# Conclusion: Your neighbor is infected
explore_psyCorona(dat_first_resp,"CRT2", n_bins=2)
# Exploring CRT3
# Imagine that every time you leave your home you have a 50/50 chance of getting infected with coronavirus. 
# You have left your home 5 times recently and nothing has happened. The next time you leave your home, 
# do you think that:
explore_psyCorona(dat_first_resp,"CRT3", n_bins=3)

# Exploring idOverlap
# Choose the picture which best represents the closeness between the people of the country you currently live in and yourself.
explore_psyCorona(dat_first_resp,"idOverlap", n_bins=5)


# NEUROTICISM
# Exploring neuro01
# I see myself as someone who... - ...is very concerned.
explore_psyCorona(dat_first_resp,"neuro01", n_bins=7)
# Exploring neuro02
# I see myself as someone who... - ...easily gets nervous.
explore_psyCorona(dat_first_resp,"neuro02", n_bins=7)
# Exploring neuro03
# I see myself as someone who... - ...is relaxed, can easily deal with stress.
explore_psyCorona(dat_first_resp,"neuro03", n_bins=7)


# PARANOIA
# Exploring para01
# I need to be on my guard against others
explore_psyCorona(dat_first_resp,"para01", n_bins=11)
# Exploring para02
# People are trying to make me upset
explore_psyCorona(dat_first_resp,"para02", n_bins=11)
# Exploring para03
# Strangers and friends look at me critically
explore_psyCorona(dat_first_resp,"para03", n_bins=11)


# CONSPIRACY
# Exploring consp01
# I think that...... -
# … many very important things happen in the world, which the public is never informed about.
explore_psyCorona(dat_first_resp,"consp01", n_bins=11)
# Exploring consp02
# I think that...... -
# … politicians usually do not tell us the true motives for their decisions.
explore_psyCorona(dat_first_resp,"consp02", n_bins=11)
# Exploring consp03
# I think that...... -
# … government agencies closely monitor all citizens.
explore_psyCorona(dat_first_resp,"consp03", n_bins=11)

# TIME VS MONEY
# Exploring TimeMoney
# To what extent do you want more money than time?
explore_psyCorona(dat_first_resp,"TimeMoney", n_bins=7)
# Exploring MoneyTime
# To what extent do you want more time than money?
explore_psyCorona(dat_first_resp,"MoneyTime", n_bins=7)


# RELIGION
# Exploring relYesNo
# Are you religious?
explore_psyCorona(dat_first_resp,"relYesNo")
# Exploring godyesno
# Do you believe in a God or Gods?
table(dat_first_resp$godyesno)
# Exploring godPunish
# For each of the following traits, please indicate how characteristic you think each trait is of the god or gods 
#you believe in. If you believe in more than one god or deity, please respond based on one that is particularly 
#important to you. - Punishing
explore_psyCorona(dat_first_resp,"godPunish", 6)
# Exploring godForgive
# For each of the following traits, please indicate how characteristic you think each trait is of the god or gods 
#you believe in. If you believe in more than one god or deity, please respond based on one that is particularly 
#important to you. - Forgiving
explore_psyCorona(dat_first_resp,"godForgive", 6)

# SOLIDARITY
# Exploring trustGovCtry
# In general, how much do you trust each of the following to take the right measures to deal with the coronavirus pandemic? 
# - The government of your country
explore_psyCorona(dat_first_resp,"trustGovCtry",6)
# Exploring godyesno
# In general, how much do you trust each of the following to take the right measures to deal with the coronavirus pandemic? 
# - Your community
explore_psyCorona(dat_first_resp,"trustGovState",6)

# CITIZENSHIP
# Exploring countryCitizen
# Are you a citizen of this country ([QID217-ChoiceTextEntryValue])?
explore_psyCorona(dat_first_resp,"countryCitizen", n_bins=2)
# Exploring Citizen
# Have you been a citizen of this country ([QID217-ChoiceTextEntryValue]) since birth?
explore_psyCorona(dat_first_resp,"Citizen", n_bins=2)
# Exploring Immigrant
# Do you consider yourself to be an immigrant?
explore_psyCorona(dat_first_resp,"Immigrant", n_bins=2)


# DEMOGRAPHICS
# Exploring gender
# What is your gender?
explore_psyCorona(dat_first_resp,"gender", n_bins=3)
# Exploring age
# What is your age?
explore_psyCorona(dat_first_resp,"age", n_bins=8)
# Exploring edu
# What is your level of education?
explore_psyCorona(dat_first_resp,"edu", n_bins=7)

# Exploring coded_country
ggplot(data = dat_first_resp, aes(x=coded_country)) +
  geom_histogram(stat="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  


