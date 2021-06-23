### Job test ###
### jmpark@email.arizona.edu ##

# set working directory
setwd("~/Documents/job search")

# read in the data
data <- read.csv("DataforAnalystTask.csv", header = TRUE, na.strings=c("","NA"))
# str(data)
# head(data)
# summary(data)

# turn missing values into 0
# do for specific variables
# data[is.na(data)] <- 0 # will not work for GPA 


# data$CumGPA2191 [data$CumGPA2191 == 0] <- NA

# reading in data with two rows as header #

header <- scan("202106_DataforAnalystTask.csv", nlines = 1, what = character())
data2 <- read.csv("202106_DataforAnalystTask.csv", skip = 2, header = FALSE)

# names(data2) <- header

### change characters into factors
# try to do it in a less dumb way
library(dplyr)
data <- data %>%
  mutate_if(sapply(data, is.character), as.factor) # this works

# check the data
sapply(data, class)

# data$Q11_1 <- as.factor(data$Q11_1)
# data$Q11_4 <- as.factor(data$Q11_4)
# data$Q11_5 <- as.factor(data$Q11_5)
# data$Q11_6 <- as.factor(data$Q11_6)
# data$Q11_7 <- as.factor(data$Q11_7)
# data$Q11_8 <- as.factor(data$Q11_8)
# data$Q11_9 <- as.factor(data$Q11_9)
# data$Q11_10 <- as.factor(data$Q11_10)
# 
# data$Q14_1 <- as.factor(data$Q14_1)
# data$Q14_4 <- as.factor(data$Q14_4)
# data$Q14_5 <- as.factor(data$Q14_5)
# data$Q14_6 <- as.factor(data$Q14_6)
# data$Q14_7 <- as.factor(data$Q14_7)
# data$Q14_8 <- as.factor(data$Q14_8)
# data$Q14_9 <- as.factor(data$Q14_9)
# data$Q14_10 <- as.factor(data$Q14_10)
# data$Q14_12 <- as.factor(data$Q14_12)
# 
# data$Career <- as.factor(data$Career)
# data$Campus <- as.factor(data$Campus)
# data$Ethnicity <- as.factor(data$Ethnicity)
# data$Gender <- as.factor(data$Gender)
# data$Residency <- as.factor(data$Residency)
# data$Honors.Flag <- as.factor(data$Honors.Flag)
# data$College <- as.factor(data$College)
# data$Acad.Class.Standing <- as.factor(data$Acad.Class.Standing)
# 
# data$Q14_3 <- as.factor(data$Q14_3)
### this does not work ##

# Load packages #
# install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidyr)
library(forcats)

# for exporting HTML
# install.packages("XML")
library(XML)
# install.packages("RCurl")
library(RCurl)
# install.packages("rvest")
library(rvest)

# for exporting SQLite
library(RSQLite)
library(DBI)

# View(data)
# transform multiple columns
# cols <- c("Q3", "Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8", 
#           "Q4_9", "Q4_10", "Q5_1", "Q5_2", "Q5_3", "Q5_4", "Q5_5","Q5_6", "Q5_7",
#           "Q5_8", "Q5_9", "Q5_10", "Q8","Q15", "Q16","Q14_11", "Q18_1", "Q18_4", 
#           "Q18_5", "Q18_6", 
#           "Q18_7", "Q18_8", "Q20", "Q21", "Q22", "Q24_7","Q29_1", "Q29_2", "Q29_3", 
#           "Q29_4", 
#           "Q29_5","Q29_6", "Q29_7", "Q29_8", "Q29_9", "Q29_10")
# 
# 
# data %<>%
#   mutate_each_(funs(factor(.)),cols)
# str(data)
# 
# # MORE COLUMNS
# col <- c("Q24_1", "Q24_2", "Q24_3", "Q24_4", "Q24_5","Q24_6", "Q25", 
#          "Q26_1", "Q26_2", "Q26_3", "Q26_4", "Q26_5","Q26_6","Q26_7", "Q26_8", 
#          "Q26_9", "Q26_10", "Q26_11", "Q30")
# 
# data %<>%
#   mutate_each_(funs(factor(.)),col)
# # str(data)
# show that students are satisfied #
# Do different groups of students report different communication preferences?

# Do attitudes about communication seem to be influenced by anything else we know about students?

# review https://towardsdatascience.com/5-ways-to-effectively-visualize-survey-data-using-r-89928bf08cb2

# select useful variables
# relevant_emails <- data %>% select(Q8)
# ethnic <- data %>% select(contains("ethnic"))

# drop the column with dates and number of emails
# df <- data %>% select(-one_of("Q7")) # may need to fix it

# row filtering

# data %>% filter(Acad.Class.Standing == "Graduate") %>% glimpse() #403 rows
# data %>% filter(Career == "GRAD") %>% glimpse() #403 rows

# filter on basis of two conditions

# data %>% filter(Career == "GRAD", Q3 == "Very effective") %>% glimpse() #403 rows

# # add or condition
# data %>% filter(Q3 == "Very effective"|Q3 == "Somewhat effective") %>% glimpse() #Rows: 2,130
# 
# # filter out what does not meet the conditions
# data %>% filter(Q3 != "Very effective") %>% glimpse() # 1,287 rows

# multiple row conditions
# filter(data, Q7 %in% c("5-Jan")) # I should fix this
# filter(data, Q7 %in% c("10-Jun"))
# filter(data, Q7 %in% c("15-Nov"))

# renaming factor levels
# levels(data$Q7) <- c("5-Jan","10-Jun","15-Nov", "16-20", "More than 20")
# levels(data$Q7)
# levels(data$Q7) <- list("1-5" = "5-Jan", "6-10" = "10-Jun", 
#                        "11-15" = "15-Nov", "16-20" = "16-20", 
#                        "More than 20"="More than 20")
data$Q7 <- recode_factor(data$Q7, "5-Jan" = "1-5", "10-Jun" = "6-10", 
                         "15-Nov" = "11-15", "16-20" = "16-20", 
                         "More than 20"="More than 20")


# export the dataset
write.csv(data,"MyDataIII.csv", row.names = FALSE)
# chaining pipe operators

# data %>% select(matches("Q3")) %>%  filter ()

# create new variable and remove the older ones
# data %>%  transmutate(  )

# group by
data %>% group_by(Ethnicity) %>% summarise((n())) # 9 by 2 table

data %>% group_by(Acad.Class.Standing) %>% 
  summarize(mean_age=mean(Age)) %>% head() #6 by 2 table

data %>% group_by(Career) %>% 
  summarize(mean_age=mean(Age)) %>% head() #5 by 2 table

# correlate between quantitative variables
data %>% group_by(Q3) %>% 
  summarize(mean_age=mean(Age)) %>% head() #5 by 2 table

# arrange data 
# data %>% group_by(Career) %>% arrange(desc(Q7)) %>% head() #this does not work
# data %>% group_by(Q3) %>% arrange(desc(Age)) %>% head() #this does not work

# # create levels for survey responses
# levels(data$Q3) <- c("Very ineffective", "Somewhat ineffective", 
#                      "Somewhat effective", "Very effective")
# levels(data$Q3) # this does not change the ordering in the summarize
# 

### select columns
# 
# data %>% select(Ethnicity,Q3) %>% head()
# # data selection useing the pipe
# data %>% select(Career, Q3) %>% head()

# install package for likert
# install.packages("likert")
# install.packages("HH")
library(likert)
library(HH)
library(reshape2)
library(RColorBrewer)
#install.packages("ggthemes")
library(ggthemes)
library(stringr)
library(plyr)

# likert style
# Transform the items into factors and save the data set as a likert object
# install.packages("here")
# install.packages("skimr")
# install.packages("amerika")
library(here) 
library(skimr) 
library(amerika)

# look to handle Q11
data %>% dplyr::select(starts_with('Q11')) %>% head() #for my Q11 #does not work

# look to handle Q29
data %>% dplyr::select(starts_with("Q29")) %>% head() #for my Q29

# this does not work
# r = data %>%rename(overall_systems = "Q3" ) # new name on the left New name = original name

# con2 = data %>% rename_if(is.numeric, str_to_upper) #worked but why do it

# # convert data wide to long
# # w -> l : gather
# 
# # social_media <- data %>% gather(Q11_1, Q11_4,Q11_4, Q11_5, Q11_6,
#                                 Q11_7, Q11_8, Q11_9)
# 
# # head(social_media)

# get info on GPA
summary(data$CumGPA2191)
# table(data$Career, data$Age) # this is not helpful

# having issues loading in sparklyr
# install.packages("sparklyr")

# recode GRAD
# data %>% 
#  pull("Career") %>% 
#  recode_factor("Career", Graduate = "GRAD") # not quite

# this did not work either
# data %>% mutate(Career=recode_factor(Career, 'GRAD'='Graduate', 
#                                     'LAW'='Graduate',
#                                    'MEDS'='Graduate',
#                                     'PHRM'='Graduate'))
#write.csv(data, file = "mydata3.csv") # see if it saved

#char_vec <- data$Career
#recode(char_vec, GRAD = "Graduate") # this does not work

# just look at undergrads
#vir1 = data[which(data$Career=="UGRD"),] # this does not work
#head(vir1)

#vir2 <- subset(data, data$Career=='UGRD') # This does not work

# wide to long
data_long <- melt(data) # using the function melt turn wide into long form
# head(data_long)

# long to widee you have to use dcast

### use the pipe to preprocess 
#data %>% select(Career, Age) %>% head()

#data %>% select(Career, Age) # this works but the others did not

# data selection by column names
#selecn <- select(data, Career, Q3)
#head(selecn)

# add a new column
dm = mutate(data, Graduate = (Career != 'UGRD')) #yay, this works
# View(dm) 
# str(dm$Graduate)

# drop the column PseudoID
# dm2 <- select(dm, -c(PseudoID)) # this did not work later for some reason
dm2 <- subset(dm, select = -c(PseudoID))
# create Students column
levels(dm2$Graduate) <- list("TRUE" = "Graduate", "FALSE" = "Undergraduate")
library(forcats)

# create the variable for students
dm2$Students <- as.factor(dm2$Graduate)
is.factor(dm2$Students)
dm2$Students <- recode_factor(dm2$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this wroked

# summarize and group by data
# summarize(dm)
          
dm %>% dplyr::select(Q3, Graduate) %>% head()
write.csv(data, file = "mydata4.csv") # this has a new column for grad or not

dm %>% filter(Q3 != "Very effective") %>% 
      group_by(Q3)



# levels(dm2$Q7) <- c("5-Jan","10-Jun","15-Nov", "16-20", "More than 20")
# levels(dm2$Q7)
# levels(data$Q7) <- list("1-5" = "5-Jan", "6-10" = "10-Jun", 
#                        "11-15" = "15-Nov", "16-20" = "16-20", 
#                        "More than 20"="More than 20")
# dm2$Q7 <- recode_factor(dm2$Q7, "5-Jan" = "1-5", "10-Jun" = "6-10", 
#                          "15-Nov" = "11-15", "16-20" = "16-20", 
#                          "More than 20"="More than 20")

# data frame with recoded Q7 and a Graduate column
# 0 for other NA except GPA
# View(dm2) 

# to see how many undergraduates are in honors college
dm2 %>% 
  dplyr::select(Honors.Flag, Graduate) %>% 
  filter(Graduate == FALSE) %>% head()

able(dm2$Honors.Flag, dm2$Graduate)

# melt the data
dm2_long <- melt(dm2) # using the function melt turn wide into long form
head(dm2_long)

# nesting data 
dm3 = dm2 %>% nest(-Graduate) #does not do much
# dm3$dm2[dm3$Graduate == FALSE] # does not work
# 
# dm4 = dm2 %>% nest(-Graduate, -Campus) # warning

# for likert
# http://rcompanion.org/handbook/E_02.html

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(plyr)){install.packages("plyr")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}

# order factors 
dm2$Q3 = factor(dm2$Q3, 
                ordered = TRUE,
                levels = c("Very effective", "Somewhat effective",
                   "Somewhat ineffective", "Very ineffective"))
                   
dm2$Q7 = factor(dm2$Q7, 
                ordered = TRUE,
                levels = c("1-5", "6-10",
                "11-15", "16-20","More than 20")) # more warnings with this one

dm2$Q8 = factor(dm2$Q8, 
                ordered = TRUE,
                levels = c("All", "Most","About half", 
                           "A few", "None", "Other")) # more warnings with this one
dm2$Q9 = factor(dm2$Q9, 
                ordered = TRUE,
                levels = c("All", "Most","About half", 
                           "A few", "None", "Other")) # more warnings with this one

# Q15 
# reorder 
# levels(dm2$Q15) <- c("Very difficult", "Somewhat difficult", 
# "Somewhat easy", "Very easy")
dm2$Q15 = factor(dm2$Q15, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))
dm2$Q16 = factor(dm2$Q16, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))

#Q30
dm2$Q30 = factor(dm2$Q30, 
                ordered = TRUE,
                levels = c("Very likely", "Somewhat likely",
                           "Somewhat unlikely", "Very unlikely"))

# Q29
# must recode Q29_1, Q29_4, Q29_8
# how can I make this more efficient
# define levels
levels_Q29 <- c("Very difficult", "Somewhat difficult",
                "Somewhat easy", "Very easy")

dm2$Q29_1 = factor(dm2$Q29_1, 
                 ordered = TRUE,
                 levels = levels_Q29)

# forcats package has a function calls fct_relevel
dm2$Q29_2 = factor(dm2$Q29_2, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))
dm2$Q29_3 = factor(dm2$Q29_3, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))

dm2$Q29_4 = factor(dm2$Q29_4, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult",
                              "Somewhat easy", "Very easy"))

dm2$Q29_5 = factor(dm2$Q29_5, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult", 
                              "Somewhat easy", "Very easy"))
dm2$Q29_6 = factor(dm2$Q29_6, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult", 
                              "Somewhat easy", "Very easy"))

dm2$Q29_7 = factor(dm2$Q29_7, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult", 
                              "Somewhat easy", "Very easy"))

dm2$Q29_8 = factor(dm2$Q29_8, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult",
                              "Somewhat easy", "Very easy"))

dm2$Q29_9 = factor(dm2$Q29_9, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult", 
                              "Somewhat easy", "Very easy"))
dm2$Q29_10 = factor(dm2$Q29_10, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult", 
                              "Somewhat easy", "Very easy"))
# levels(dm2$Q3)
# 
# summary(dm2$Q3) # good summary


# to find the mean age of graduate students who answered question 3
dm2 %>% 
  filter(Career != "UGRD") %>% 
  group_by(Q3) %>% 
  summarize(mean_age = mean(Age)) # mean age of non-UGRD is 30.3

# summary(dm2$Age)

# more visualization 
# pattern for missing data
library(VIM)
mp <- aggr(dm2, col=c('pink', 'yellow'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(dm2), cex.axis=.7,
           gap=3, ylab=c("Missing data", "Pattern"))
           
# imputation method for missing data
library(mice)
im_dm2 <- mice(dm2, m=5, maxit = 50, method = 'pmm', seed = 500)


#### look at UpSet Plot
#install.packages("naniar")
library(naniar)
dm2 %>%
  # Select the survey items
  dplyr::select(starts_with(c("Q11","Q3"))) %>%
  # Create an UpSet plot
  gg_miss_upset(., nsets = 10) # this works for some reason

##################Pivoting longer
# https://scc.ms.unimelb.edu.au/resources-list/simple-r-scripts-for-analysis/r-scripts

longer_data <- dm2 %>%
  pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response")
print(longer_data)

# dm2 %>%
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response") %>%
#   ggplot(aes(x = response)) +
#   geom_bar() +
#   facet_wrap(vars(question), ncol = 3) +
#   labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")


# bargraph <- dm2 %>%
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response") %>%
#   ggplot(aes(x = response, fill = response, colour = response)) +
#   geom_bar() +
#   geom_boxplot() +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")
# 
# bargraph <- bargraph + labs(title = "Which Apps do Students Use",
#               subtitle = "multiple selection allowed")
# print(bargraph)

# # try to filter out the NA
# # bargraph0[ , 1:50][is.na(data[ , 1:50] ) ] = 0
# # dm2 %>% replace_na(list(Q11_1 = NA, y = "0"))
# 
# bargraph0 <- data %>%
#   filter(Q11_1 != "NA") %>% 
#   filter(Q11_4 != "NA") %>% 
#   filter(Q11_5 != "NA") %>% 
#   filter(Q11_6 != "NA") %>% 
#   filter(Q11_7 != "NA") %>% 
#   filter(Q11_8 != "NA") %>% 
#   filter(Q11_9 != "NA") %>% 
#   filter(Q11_10 != "NA") %>% 
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response") %>% 
#   #tidyr::replace_na(list(x = NA, y = 0)) %>% 
#   # dplyr::mutate(Q11_1 = replace_na(Q11_1, 0)) 
#   ggplot(aes(x = response, colour = response, fill = response, na.rm = TRUE)) +
#   geom_bar(stat="bin", na.rm = TRUE) +
#   geom_bar() +
#   geom_boxplot() +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")
# 
# bargraph0 <- bargraph0 + labs(title = "Which Apps do Students Use",
#                             subtitle = "multiple selection allowed")
# print(bargraph0)

# filter out the NA manually
FB <- filter(dm2, Q11_1 == 'Facebook / Messenger') # 1599
IG <- filter(dm2, Q11_4 == 'Instagram') #1779
SC <- filter(dm2, Q11_5 == 'Snapchat') #1612
SMS <- filter(dm2, Q11_6 == 'Text / SMS') #1991
TW <- filter(dm2, Q11_7 == 'Twitter') # 995
WC <- filter(dm2, Q11_8 == 'WeChat') # 100
WA <- filter(dm2, Q11_9 == 'WhatsApp') # 623
OT <- filter(dm2, Q11_10 == 'Other') #106

# # this bar graph includes counts from Q11 
# # use this with no NA
# bargraph0 <- dm2 %>%
#   # dplyr::select(Q11_1 != "NA") %>% 
#   # dplyr::select(Q11_4 != "NA") %>% 
#   # dplyr::select(Q11_5 != "NA") %>% 
#   # dplyr::select(Q11_6 != "NA") %>% 
#   # dplyr::select(Q11_7 != "NA") %>% 
#   # dplyr::select(Q11_8 != "NA") %>% 
#   # dplyr::select(Q11_9 != "NA") %>% 
#   # dplyr::select(Q11_10 != "NA") %>% 
#   # dplyr::select(Q11_1 == "Facebook / Messenger")
#   # dplyr::filter(Q11_1 == 'Facebook / Messenger') %>%
#   # dplyr::filter(Q11_4 == 'Instagram') #1779 %>%
#   # dplyr::filter(Q11_5 == 'Snapchat') #1612 %>%
#   # dplyr::filter(Q11_6 == 'Text / SMS') #1991 %>%
#   # dplyr::filter(Q11_7 == 'Twitter') # 995 %>%
#   # dplyr::filter(Q11_8 == 'WeChat') # 100 %>%
#   # dplyr::filter(Q11_9 == 'WhatsApp') # 623 %>%
#   # dplyr::filter(Q11_10 == 'Other') #106 %>%
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = response, colour = response)) +
#   geom_bar() +
#   geom_text(stat='count', aes(label=..count..), vjust=-1) +
#   geom_boxplot() +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")
# 
# bargraph0 <- bargraph0 + labs(title = "Which Apps do Students Use",
#                             subtitle = "multiple selection allowed")
# print(bargraph0)
# 
# # subset with 
# 
# # more attempts
# 
# # for campus tools
# bargraph2 <- dm2 %>%
#   pivot_longer(Q5_1:Q5_10, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = response, colour = response)) +
#   geom_bar() +
#   geom_boxplot() +
#   labs(x = " ", y = "Total Responses")
# 
# bargraph2 <- bargraph2 + labs(title = "Overall Satisfaction with Campus Tools",
#                             subtitle = " ")
# print(bargraph2)
# 
# # for campus involvement
# bargraph3 <- dm2 %>%
#   pivot_longer(Q14_1:Q14_11, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = response, colour = response)) +
#   geom_bar() +
#   geom_boxplot() +
#   labs(x = " ", y = "Total Responses")
# 
# bargraph3 <- bargraph3 + labs(title = "Student Involvement",
#                               subtitle = "graduate and Undergraduates")
# 
# print(bargraph3)
# 
# # another attempt for campus involvement

# bargraph3a <- dm2 %>%
#   # dplyr::filter(dm2, Q14_1 == 'ASUA') %>% 
#   # filter(dm2, Q14_1:Q14_13 != NA) %>% 
#   dplyr::filter(as.integer(Q14_1) == 'ASUA') %>% 
#   dplyr::filter(as.integer(Q14_4) == 'Classmates') %>% 
#   dplyr::filter(as.integer(Q14_13) == 'Cultural and resource centers') %>% 
#   dplyr::filter(as.integer(Q14_5) == 'Residence hall community') %>% 
#   dplyr::filter(as.integer(Q14_6) == 'Fraternity and sorority programs') %>% 
#   dplyr::filter(as.integer(Q14_7) == 'My college department') %>% 
#   dplyr::filter(as.integer(Q14_8) == 'Roommates') %>% 
#   dplyr::filter(as.integer(Q14_9) == 'Social media') %>% 
#   dplyr::filter(as.integer(Q14_10) == 'Through friends and acquaintances') %>% 
#   dplyr::filter(as.integer(Q14_12) == 'Workplace') %>% 
#   dplyr::filter(as.integer(Q14_11) == 'Other') %>% 
#   pivot_longer(Q14_1:Q14_13, names_to = "question", values_to = "response") %>%
#   ggplot(aes(x = response, colour = response)) +
#   geom_bar() +
#   geom_boxplot() +
#   labs(x = " ", y = "Total Responses")
# 
# bargraph3a <- bargraph3a + labs(title = "Student Involvement",
#                               subtitle = " ")
# print(bargraph3a)




# install.packages("sjPlot")
# install.packages("sjmisc")
library(sjPlot)
library(sjmisc)
library(dplyr)

# filtering
df14 <- dm2 %>% 
  dplyr::select("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
         "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
         "Q14_11", "Q14_12", "Q14_13", Age, Graduate)

# df14 %>% gather("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
#                 "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
#                 "Q14_11", "Q14_12", "Q14_13", -Age, -Graduate)
# View(df14)

df14_longer <- df14 %>% 
  pivot_longer(cols = c("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
                        "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
                        "Q14_11", "Q14_12", "Q14_13"),
               names_to = "community")
write.csv(df14_longer,"df14_longer.csv", row.names = FALSE)

####
# filter by different groups
# df14 %>% group_by(Age, Q14_1) %>% summarize(N - n()) %>% 
#  mutate(pct = round(N/sum(N) *100, 0))

#Error in eval(cols[[col]], .data, parent.frame()) : object 'N' not found
#In addition: Warning message:
# Factor `Q14_1` contains implicit NA, consider using `forcats::fct_explicit_na` 
# df14_longer %>%
#   group_by(Graduate) %>% 
#   summarize(Age) # what is this
# 
# rem <- df14_longer %>% 
#   filter(Graduate != "<NA>") ## retain all not with Graduate
# 
# undergraduates = subset(rem, Graduate==FALSE) #for undergrads about community
# #remove NA
# na_ugrd <- na.omit(undergraduates) 
# 
# glimpse(undergraduates)
# 
# # this graph tells me very little
# ggplot(na_ugrd, aes(x=community, y=value)) +
#   geom_point(shape=1)    +  # Use hollow circles
#   geom_smooth()            # Add a loess smoothed fit curve with confidence region
# #> `geom_smooth()` using method = 'loess'
#  
# # rbind with multiple columns 
# # df14_cols <- cols(c("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
#                             "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
#                             "Q14_11", "Q14_12", "Q14_13"))
# # df14$Q14 <- df14(cbind(c("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
#                   "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
#                   "Q14_11", "Q14_12", "Q14_13"))

# https://statisticsglobe.com/cbind-and-rbind-vectors-with-different-length-in-r
# install.packages("qpcR")                            # Install qpcR package
library("qpcR")      
# # data_cbind <- qpcR:::cbind.na("Q14_1", "Q14_4", "Q14_5", "Q14_6", 
#                               "Q14_7", "Q14_8", "Q14_9", "Q14_10", 
#                               "Q14_11", "Q14_12", "Q14_13")           # Bind as columns
# # data_cbind    

# # Make a stacked bar graph
# # Activate likert and plyr
# library("likert")
# library("plyr")
# 
# # A custom function to recode numerical responses into ordered factors
# 
#   y <- factor(y, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
#   
#   return(y)
# }

table(dm2$Acad.Class.Standing)

# # average age overall
# # Average Age
#   > dm2 %>% group_by(Acad.Class.Standing) %>% 
#   +   summarize(mean_age=mean(Age)) %>% head() #5 by 2 table
# mean_age
# 1 22.83422

# Average Age of undergrad is 20.996
dm2 %>% group_by(Career) %>% 
  dplyr::filter(Career == "UGRD") %>% 
  summarize(mean_age=mean(Age)) %>% head() #5 by 2 table

# average age of graduate students is 30.29
dm2 %>% group_by(Students) %>% 
  dplyr::filter(Students == "Graduate") %>% 
  summarize(mean_age=mean(Age)) %>% head() #5 by 2 table


table(dm2$Q3) # missing variables but total is 2222 out of 2437

summary(dm2$Q7) # 0.9257283 response rate, 40% get 16 or more emails.

# filter by students in Q9 
table(dm2$Q9, dm2$Students)

# recode variables to shorter names
# for Q14
dm2$Q14_13 <- recode_factor(dm2$Q14_13, "Cultural and resource centers" = "Cultural")

dm2$Q14_5 <- recode_factor(dm2$Q14_5, "Residence hall community" = "Res Hall")

dm2$Q14_6 <- recode_factor(dm2$Q14_6, "Fraternity and sorority programs" = "Greek")

dm2$Q14_7 <- recode_factor(dm2$Q14_7, "My college department" = "Department")

dm2$Q14_10 <- recode_factor(dm2$Q14_13, "Through friends and acquaintances" = "Friends")

# for Q18
dm2$Q18_1 <- recode_factor(dm2$Q18_1, "Campus or mall events" = "Campus Events")

dm2$Q18_4 <- recode_factor(dm2$Q18_4, "Cat Cash balance" = "Cat Cash")

dm2$Q18_5 <- recode_factor(dm2$Q18_5, "Food deals / free food" = "Food")

dm2$Q18_6 <- recode_factor(dm2$Q18_6, "Homework & assignment due dates" = "Homework")

dm2$Q18_7 <- recode_factor(dm2$Q18_7, "Course grades" = "Grades")

#### analyze bargraph5 Q29 with proportions
bargraph5 <- bargraph5 %>% select(!is.na("Fin Aid"))
