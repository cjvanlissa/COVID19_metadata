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





unique(dat_first_resp$tightNorms)
unique(dat_first_resp$tightNorms)


