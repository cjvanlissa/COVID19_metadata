#installs the required package
if(!"readxl" %in% installed.packages()) install.packages("readxl")

## Modify Path ##
path = "Global Health Security Index 2019 Final (October 2019).xlsm"

# Most prepared
# Score 66.7 to => 100
most_prepared<-readxl::read_excel(path = path, sheet = 4, range = "E47:F59",
                   col_names = F,progress = T)

# More prepared
# Score 33.4 to => 66.6
more_prepared<-readxl::read_excel(path = path, sheet = 4, range = "H47:I155",
                                  col_names = F,progress = T)

# Least prepared
# Score 0 to => 33.3
least_prepared<-readxl::read_excel(path = path, sheet = 4, range = "K47:L119",
                                  col_names = F,progress = T)

#adding names of tibbles as variable because I'm going to merge them later
name.as.variable.df<- function (df)
{
  name=deparse(substitute(df)) #outputs the name of whatever df is
  df[,ncol(df)+1]<-name 
  return(df)
}

most_prepared<-name.as.variable.df(most_prepared)
more_prepared<-name.as.variable.df(more_prepared)
least_prepared<-name.as.variable.df(least_prepared)

#merging into one big tibble
preparedness_score<-rbind(most_prepared, more_prepared, least_prepared)

### Tidying up the tibble ###
names(preparedness_score)<-c("country", "score", "category")
preparedness_score$category<-factor(preparedness_score$category)
preparedness_score$country<-factor(preparedness_score$country)

###INDICATORS###
#the indicators were organized hierarchically as such:
  #Categories
    #Indicators
      #Sub-indicators
        #Questions
#So the question 6.4.1a) is from category 6, indicator 4, subindicator 1 question a)
#Seeing as this info is contained within the question code, here I loaded only the questions

per_country_questions<-readxl::read_excel(path = path, sheet = 13, range = "N10:HC276",
                                  col_names = T,progress = T)

#NOTE: varible containing question string is confusingly named Indicators.
per_country_questions$Indicators

#dropping empty rows (those corresponding to higher hieracrchical levels)
per_country_questions<-per_country_questions[!is.na(per_country_questions$Unit),]

#preparedness score is a weighted sum of questions.
#here I obtain the weights for each question

question_weights<-readxl::read_excel(path = path, sheet = 14, range = "C183:H408",
                                           col_names = T,progress = T)

question_weights[,c(2:3,5)]<-NULL
question_weights<-question_weights[!is.na(question_weights$`Weight %`),]
names(question_weights)[1]<-"question_code"

## equal number of rows means I didn't mess something up
nrow(question_weights)==nrow(per_country_questions)

#question_weights column QUESTIONS is the same as per_country_questions column indicators
all(question_weights$QUESTIONS==per_country_questions$Indicators)

#Realised my mistake. Questions are weighted as percents of subindicator.
#for the absolute weighting I have to obtain weighting for all higher levels.

#Have to redo the question weights. Same code as above
question_weights<-readxl::read_excel(path = path, sheet = 14, range = "C183:H408",
                                     col_names = T,progress = T)
question_weights<-question_weights[!is.na(question_weights$`Weight %`),]


#Subindicator weights here. Same deal as with questions.
subind_weights<-readxl::read_excel(path = path, sheet = 14, range = "C62:H181",
                                     col_names = T,progress = T)
subind_weights<-subind_weights[!is.na(subind_weights$`Weight %`),]

#Indicator weights here. Same deal as with questions.
ind_weights<-readxl::read_excel(path = path, sheet = 14, range = "C20:H60",
                                   col_names = T,progress = T)

ind_weights<-ind_weights[!is.na(ind_weights$`Weight %`),]
#Category weights here. Same deal as with questions.
cat_weights<-readxl::read_excel(path = path, sheet = 14, range = "C12:H18",
                                col_names = T,progress = T)
cat_weights<-cat_weights[!is.na(cat_weights$`Weight %`),]
#to get absolute weights for each question I have to multiply the weight
#by all the weights above it

#takes weights from a lower level and multiplies them by weights from the higher level
abs_weights<-function(lvl, lvl_up)
{
nms=(lvl_up[,1])
for (i in 1:nrow(nms))
  {
  lvl_up_w=as.numeric(lvl_up[i,6])
  this_nm=as.character(nms[i,])
  is_lvl=grep(pattern = this_nm, x = lvl$...1)
  lvl[is_lvl,6]=lvl[is_lvl,6]*lvl_up_w
}
  return(lvl)
}

#sanity check. sums to 1, so it's ok!
sum(abs_weights(lvl = ind_weights, lvl_up = cat_weights)[,6])

ind_weights<-abs_weights(lvl = ind_weights, lvl_up = cat_weights)
subind_weights<-abs_weights(subind_weights, ind_weights)
question_weights<-abs_weights(question_weights,subind_weights)

#It worked! The absolute weighting in the question weights now
#represents the percent contribution it has to the sum score.
#These weights can be linearly transformed (intercept and slope) as needed
sum(question_weights$`Weight %`)

#Tidying up the question weights tibble
question_weights[,c(2:3,5)]<-NULL
names(question_weights)[1]<-"question_code"


#removing junk
rm(list=c("cat_weights","ind_weights", "subind_weights",
          "least_prepared", "more_prepared", "most_prepared",
          "path"))
