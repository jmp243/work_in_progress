# Data Visutalizations
# Jung Mee Park
# jmpark@email.arizona.edu
# create one chart with simple breakdown
# ggplot(dm2, aes(x = Graduate)) + 
#   geom_bar() + 
#   coord_flip()

# create another image about number of emails 
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Q9graph2 <- dm2 %>%
#   # mutate(Q9 = fct_infreq(Q9)) %>%
#   # dplyr::filter(Students == "Undergraduate") %>% 
#   drop_na(Q9) %>% 
#   ggplot(aes(x = Q9, fill = Students, colour = Students)) +
#   geom_bar() +
#   coord_flip() +
#   labs(x = "redundant emails", y = "responses")

Q7graph <- dm2 %>%
  # mutate(Q7 = fct_infreq(Q7)) %>%
  drop_na(Q7) %>% 
  ggplot(aes(x = Q7, fill = Students, response = Students)) +
  geom_bar() +
  coord_flip() +
  labs(x = "emails per week", y = "responses")

Q7graph <- Q7graph + labs(title = "How many emails do Students Get?",
                          subtitle = "undergraduate and graduate students")

print(Q7graph) # 16 or more is 907 and that is about 40% 

# pie chart for Q3
# slices <- c(458, 422, 430, 645, 482)
# lbls <- c("Freshmen", "Sophomore",  "Junior", "Senior","Grad&Prof")
# pct <- round(slices/sum(slices)*100)
# lbls <- paste(lbls, pct) # add percents to labels
# lbls <- paste(lbls,"%",sep="") # add % to labels
# pie(slices,labels = lbls, col=terrain.colors(length(lbls))) 
# pie(slices,labels = lbls, col=terrain.colors(length(lbls)), main="Distribution of Survey") 

# Q7pie <- dm2 %>%
#   # mutate(Q7 = fct_infreq(Q7)) %>%
#   drop_na(Q7) %>% 
#   ggplot(aes(x = Q7), y=response, fill = Q7, response = Q7) +
#   geom_bar(fill = "lightblue", color = "grey") +
#   coord_polar("y", start=0) +
#   theme_void()
# 
# print(Q7pie)
# # just for FB
# dm2 %>%
# # %>%   mutate(Q11_1 = fct_infreq(Q11_1)) %>%
#   ggplot(aes(x = Q11_1)) + 
#   geom_bar() + 
#   coord_flip()

# redudant emails Q9
Q9graph <- dm2 %>%
  # mutate(Q9 = fct_infreq(Q9)) %>%
  drop_na(Q9) %>% 
  ggplot(aes(x = Q9)) +
  geom_bar(fill = "lightblue", color = "grey") +
  coord_flip() +
  labs(x = "redundancy", y = "responses")

Q9graph <- Q9graph + labs(title = "How redundant are these emails?",
                          subtitle = "undergraduate and graduate")

print(Q9graph)

# redudant emails Q9 undergraduates only

Q9graph1 <- dm2 %>%
  mutate(Q9 = fct_infreq(Q9)) %>%
  drop_na(Q9) %>% 
  dplyr::filter(Career =="UGRD") %>% 
  ggplot(aes(x = Q9)) +
  geom_bar(fill = "lightblue", color = "grey") +
  coord_flip() +
  labs(x = "redundant emails", y = "responses")

Q9graph1 <- Q9graph1 + labs(title = "How redundant are these emails?",
                          subtitle = "undergraduate only")

print(Q9graph1)


########33
# redudant emails Q9 undergraduates only
# try to find a 
# bargraph0c <- dm2 %>% 
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = Students, colour = Students)) +
#   geom_bar() +
#   # geom_text(stat='count', aes(label=..count..), vjust=0) +
#   geom_boxplot() +
#   # scale_color_brewer(palette = "Blues") +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")

Q9graph2 <- dm2 %>%
  # mutate(Q9 = fct_infreq(Q9)) %>%
  # dplyr::filter(Students == "Undergraduate") %>% 
  drop_na(Q9) %>% 
  ggplot(aes(x = Q9, fill = Students, colour = Students)) +
  geom_bar() +
  coord_flip() +
  labs(x = "redundant emails", y = "responses")

Q9graph2 <- Q9graph2 + labs(title = "How redundant are these emails?",
                            subtitle = "undergraduate and graduate students")

print(Q9graph2)

# Q15 
# reorder 
# levels(dm2$Q15) <- c("Very difficult", "Somewhat difficult", 
# "Somewhat easy", "Very easy")
dm2$Q15 = factor(dm2$Q15, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))

Q15graph <- dm2 %>%
  mutate(Q15 = fct_infreq(Q15)) %>%
  drop_na(Q15) %>% 
  ggplot(aes(x = Q15), stat="identity") +
  geom_bar(fill = "lightblue", color = "grey") +
  coord_flip() +
  labs(x = "", y = "responses")

Q15graph <- Q15graph + labs(title = "How easy is it for students to get involved?",
                            subtitle = "undergraduate and graduate")

print(Q15graph)

### emails that pertain to me
Q9graph3 <- dm2 %>%
  # mutate(Q9 = fct_infreq(Q9)) %>%
  # dplyr::filter(Students == "Undergraduate") %>% 
  drop_na(Q8) %>% 
  ggplot(aes(x = Q8, fill = Students, colour = Students)) +
  geom_bar() +
  coord_flip() +
  labs(x = "Emails Relevant to Me", y = "Responses")

Q9graph3 <- Q9graph3 + labs(title = "Do these emails pertain to me?",
                            subtitle = "undergraduate and graduate students")

print(Q9graph3)
### data visualization
# catagorical or qualitative
# ordinal 
dim(dm2)
summary(dm2)

# pie chart
table(data$Career)
slices <- c(35, 403, 30, 1955, 14)
lbls <- c("law", "grad",  "med", "undergraduate","phrm")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=terrain.colors(length(lbls))) 
pie(slices,labels = lbls, col=terrain.colors(length(lbls)), main="Distribution of Survey") 

# better pie chart

slices <- c(458, 422, 430, 645, 482)
lbls <- c("Freshmen", "Sophomore",  "Junior", "Senior","Graduate & Professional")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=terrain.colors(length(lbls))) 
pie(slices,labels = lbls, col=terrain.colors(length(lbls)), main="Distribution of Survey") 

# correlation plots
# Activate the dplyr package
library("dplyr")

# Correlation matrix of items
# install.packages("ggcorrplot")
# install.packages("lsr")
library(lsr)

# Correlation matrix of items
cormat <- data %>%
  dplyr::select(starts_with(c("Age", "CumGPA2191"))) %>%
  cor(., use = "pairwise.complete.obs")

# Activate the corrplot package
library("corrplot")

# Correlation matrix plot
corrplot(cormat, # correlation matrix
         order = "hclust", # hierarchical clustering of correlations
         addrect = 2) # number of rectangles to draw around clusters
# function to get chi square p value and Cramers V
# https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
# 
# f = function(x,y) {
#   tbl = dm2 %>% select(x,y) %>% table()
#   chisq_pval = round(chisq.test(tbl)$p.value, 4)
#   cramV = round(cramersV(tbl), 4) 
#   data.frame(x, y, chisq_pval, cramV) }
# 
# # create unique combinations of column names
# # sorting will help getting a better plot (upper triangular)
# df_comb = data.frame(t(combn(sort(names(dm2)), 2)), stringsAsFactors = F)
# 
# # apply function to each variable combination
# df_res = map2_df(df_comb$Age, df_comb$Q7, f)
# 
# # plot results
# df_res %>%
#   ggplot(aes(x,y,fill=chisq_pval))+
#   geom_tile()+
#   geom_text(aes(x,y,label=cramV))+
#   scale_fill_gradient(low="red", high="yellow")+
#   theme_classic()
# 
# require(rcompanion)
# # Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# # Adopted from https://stackoverflow.com/a/52557631/590437
# mixed_assoc = function(dm2, cor_method="spearman", adjust_cramersv_bias=TRUE){
#   df_comb = expand.grid(names(dm2), names(dm2),  stringsAsFactors = F) %>% set_names("X1", "X2")
#   
#   is_nominal = function(x) class(x) %in% c("factor", "character")
#   # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
#   # https://github.com/r-lib/rlang/issues/781
#   is_numeric <- function(x) { is.integer(x) || is_double(x)}
#   
#   f = function(xName,yName) {
#     x =  pull(df, xName)
#     y =  pull(df, yName)
#     
#     result = if(is_nominal(x) && is_nominal(y)){
#       # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
#       cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
#       data.frame(xName, yName, assoc=cv, type="cramersV")
#       
#     }else if(is_numeric(x) && is_numeric(y)){
#       correlation = cor(x, y, method=cor_method, use="complete.obs")
#       data.frame(xName, yName, assoc=correlation, type="correlation")
#       
#     }else if(is_numeric(x) && is_nominal(y)){
#       # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
#       r_squared = summary(lm(x ~ y))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
#       
#     }else if(is_nominal(x) && is_numeric(y)){
#       r_squared = summary(lm(y ~x))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
#       
#     }else { #does not work from here
#       warning(paste("unmatched column type combination: ", class(x), class(y))) 
#     }
#     
#     # finally add complete obs number and ratio to table
#     result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
#   }
#   
#   # apply function to each variable combination
#   map2_df(df_comb$X1, df_comb$X2, f)
# }
# 

# this bar graph includes counts from Q11 
# use this with no NA
bargraph0 <- dm2 %>%
  # dplyr::filter(Career !="UGRD") %>% 
  # dplyr::select(Q11_1 != "NA") %>% 
  # dplyr::select(Q11_4 != "NA") %>% 
  # dplyr::select(Q11_5 != "NA") %>% 
  # dplyr::select(Q11_6 != "NA") %>% 
  # dplyr::select(Q11_7 != "NA") %>% 
  # dplyr::select(Q11_8 != "NA") %>% 
  # dplyr::select(Q11_9 != "NA") %>% 
  # dplyr::select(Q11_10 != "NA") %>% 
  # dplyr::select(Q11_1 == "Facebook / Messenger")
  # dplyr::filter(Q11_1 == 'Facebook / Messenger') %>%
  # dplyr::filter(Q11_4 == 'Instagram') #1779 %>%
# dplyr::filter(Q11_5 == 'Snapchat') #1612 %>%
# dplyr::filter(Q11_6 == 'Text / SMS') #1991 %>%
# dplyr::filter(Q11_7 == 'Twitter') # 995 %>%
# dplyr::filter(Q11_8 == 'WeChat') # 100 %>%
# dplyr::filter(Q11_9 == 'WhatsApp') # 623 %>%
# dplyr::filter(Q11_10 == 'Other') #106 %>%
pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
             values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = response, colour = response)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_boxplot() +
  labs(x = "Type of Social Media Apps", y = "Number of Uses")

bargraph0 <- bargraph0 + labs(title = "Which Apps do Students Use",
                              subtitle = "multiple selection allowed, 
                              undergraduates only")
print(bargraph0)

# subset with Graduates only
bargraph0a <- dm2 %>%
  dplyr::filter(Career !="UGRD") %>% 
  pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
             values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = response, colour = response)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_boxplot() +
  labs(x = "Type of Social Media Apps", y = "Number of Uses")

bargraph0a <- bargraph0a + labs(title = "Which Apps do Students Use",
                              subtitle = "multiple selection allowed, graduates only")
print(bargraph0a)

# subset with ugrd only
bargraph0b <- dm2 %>%
  dplyr::filter(Career =="UGRD") %>% 
  pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = response, colour = response)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_boxplot() +
  labs(x = "Type of Social Media Apps", y = "Number of Uses")

bargraph0b <- bargraph0b + labs(title = "Which Apps do Students Use",
                                subtitle = "multiple selection allowed, undergraduates only")
print(bargraph0b)
# more attempts

# stacked bar graph in R
# https://www.statology.org/stacked-barplot-in-r/
# ggplot(df, aes(fill=position, y=points, x=team)) + 
#   geom_bar(position='stack', stat='identity') +
#   theme_minimal() + 
#   labs(x='Team', y='Points', title='Avg. Points Scored by Position & Team') +
#   theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
#   scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink'))

# bargraph0c <- dm2 %>%
# pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
#              values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = Career, colour = Career)) +
#   geom_bar() +
#   geom_text(stat='count', aes(label=..count..), vjust=-.5) +
#   geom_boxplot() +
#   scale_color_brewer(palette = "Blues") +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")
# 
# bargraph0c <- bargraph0c + labs(title = "Which Apps do Students Use",
#                               subtitle = "multiple selection allowed")
# print(bargraph0c)

# stacked bar with other subset
# recode Graduate variable
# x <- factor(c("alpha","beta","gamma","alpha","beta"))
# levels(x) <- list(A="alpha", B="beta", C="gamma")

# levels(data$Graduate)
# dm2$Graduate <- recode_factor(dm2$Graduate, "2" = "Graduate", "1" = "Undergraduate")

# dm2$Graduate = factor(dm2$Graduate, 
#                 ordered = FALSE,
#                 levels = c("1", "2")) # more warnings with this one

                                    # Duplicate data
levels(dm2$Graduate) <- list("TRUE" = "Graduate", "FALSE" = "Undergraduate")
library(forcats)
dm2$Students <- as.factor(dm2$Graduate)
is.factor(dm2$Students)
dm2$Students <- recode_factor(dm2$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this wroked

# dm2$Students <- recode(dm2$Students, "2" == "Graduate", "1" == "Undergraduate")

# fct_recode(dm2$Graduate, "TRUE" = "Graduate", "FALSE" = "Undergraduate")
# dm2$Graduate[dm2$Graduate== "TRUE"] <- "Graduate"
# dm2$Graduate[dm2$Graduate == "1"] <- "Undergraduate"

# Convert to a factor
# dm2$Graduate <- as.factor(dm2$Graduate)
#  dplyr::mutate(Career=recode(Career,'UGRD'='Undergrad', 
# 'GRAD' ='Graduate', 'LAW'='Graduate', 'MEDS'='Graduate', 'PHRM'='Graduate')) %>% 
  
bargraph0c <- dm2 %>% 
  pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..), vjust=0) +
  geom_boxplot() +
  # scale_color_brewer(palette = "Blues") +
  labs(x = "Type of Social Media Apps", y = "Number of Uses")

bargraph0c <- bargraph0c + labs(title = "Which Apps do Students Use",
                                subtitle = "multiple selection allowed") 
print(bargraph0c)

# different variable class standing
bargraph0d <- dm2 %>% 
# dplyr::mutate(Graduate = c("Yes", "No")) %>% 
pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
             values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Acad.Class.Standing, colour = Acad.Class.Standing)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  geom_boxplot() +
  # scale_color_brewer(palette = "Blues") +
  labs(x = "Type of Social Media Apps", y = "Number of Uses")

bargraph0d <- bargraph0d + labs(title = "Which Apps do Students Use",
                                subtitle = "multiple selection allowed") 
print(bargraph0d)
#####
# bargraph0c <- dm2 %>% 
#   pivot_longer(Q11_1:Q11_10, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = Students, colour = Students)) +
#   geom_bar() +
#   # geom_text(stat='count', aes(label=..count..), vjust=0) +
#   geom_boxplot() +
#   # scale_color_brewer(palette = "Blues") +
#   labs(x = "Type of Social Media Apps", y = "Number of Uses")
# 
# bargraph0c <- bargraph0c + labs(title = "Which Apps do Students Use",
#                                 subtitle = "multiple selection allowed") 
# print(bargraph0c)

# for campus tools
bargraph2 <- dm2 %>%
  pivot_longer(Q5_1:Q5_10, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  geom_boxplot() +
  labs(x = " ", y = "Total Responses")

bargraph2 <- bargraph2 + labs(title = "Overall Satisfaction with Campus Tools",
                              subtitle = "undergraduate and graduate students")
print(bargraph2)


# for campus involvement
bargraph3 <- dm2 %>%
  pivot_longer(Q14_1:Q14_11, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  geom_boxplot() +
  labs(x = " ", y = "Total Responses")

bargraph3 <- bargraph3 + labs(title = "Student Involvement",
                              subtitle = "undergraduate and graduate students")

print(bargraph3)

# another attempt for campus involvement

bargraph3a <- dm2 %>%
  # dplyr::filter(dm2, Q14_1 == 'ASUA') %>% 
  # filter(dm2, Q14_1:Q14_13 != NA) %>% 
  dplyr::filter(as.integer(Q14_1) == 'ASUA') %>% 
  dplyr::filter(as.integer(Q14_4) == 'Classmates') %>% 
  dplyr::filter(as.integer(Q14_13) == 'Cultural and resource centers') %>% 
  dplyr::filter(as.integer(Q14_5) == 'Residence hall community') %>% 
  dplyr::filter(as.integer(Q14_6) == 'Fraternity and sorority programs') %>% 
  dplyr::filter(as.integer(Q14_7) == 'My college department') %>% 
  dplyr::filter(as.integer(Q14_8) == 'Roommates') %>% 
  dplyr::filter(as.integer(Q14_9) == 'Social media') %>% 
  dplyr::filter(as.integer(Q14_10) == 'Through friends and acquaintances') %>% 
  dplyr::filter(as.integer(Q14_12) == 'Workplace') %>% 
  dplyr::filter(as.integer(Q14_11) == 'Other') %>% 
  pivot_longer(Q14_1:Q14_13, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response, colour = response)) +
  geom_bar() +
  geom_boxplot() +
  labs(x = " ", y = "Total Responses")

bargraph3a <- bargraph3a + labs(title = "Student Involvement",
                                subtitle = " ")
print(bargraph3a)

# stacked bar graph for Q30
bargraph0e <- dm2 %>% 
  pivot_longer(Q30, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..), vjust=0) +
  geom_boxplot() +
  # scale_color_brewer(palette = "Blues") +
  labs(x = "", y = "Number of Students")

bargraph0e <- bargraph0e + labs(title = "Will you be recommending UArizona to others?",
                                subtitle = "") 
print(bargraph0e)

# for Q 18 
# What is important to you? 
bargraph4 <- dm2 %>%
  pivot_longer(Q18_1:Q18_8, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  geom_boxplot() +
  labs(x = " ", y = "Total Responses")

bargraph4 <- bargraph4 + labs(title = "Issues Important to Students",
                              subtitle = "undergraduate and graduate students")

print(bargraph4)

# for Q29 
# rate these services 
# Rate these administrative services
bargraph5 <- data.frame(dm2$Q29_1, dm2$Q29_2, dm2$Q29_3, dm2$Q29_4, dm2$Q29_5, 
                        dm2$Q29_6, dm2$Q29_7, dm2$Q29_8, dm2$Q29_9, dm2$Q29_10)

# helpful example
# dt1 <- data.frame(A=1, B=2, C=3, D=4)
# 
# dt2 <- data.frame(Col1=c("A","B","C","D"),Col2=c("E","Q","R","Z"))


dt2 <- data.frame(Col1=c("dm2$Q29_1","dm2$Q29_2","dm2$Q29_3","dm2$Q29_4",
                  "dm2$Q29_5","dm2$Q29_6","dm2$Q29_7","dm2$Q29_8",
                  "dm2$Q29_9","dm2$Q29_10"), 
                  Col2=c("Fin Aid","Assignments","Change Majors","Check Grades",
                         "Class Search","Deg Req","Register","Advisor",
                         "Progress","Bill Pay"))

names(bargraph5) <- dt2$Col2


summary(bargraph5)

bargraph_long <- bargraph5 %>% 
  pivot_longer(cols = everything(), names_to = "question", values_to = "response") 

bargraph_counts <- bargraph_long %>% 
  drop_na() %>% 
  group_by(response) %>% 
  summarise(count = n())

bargraph5a <- bargraph5 %>%
  gather(bargraph5, value = response, na.rm = TRUE) %>%
  # gather(bargraph5, value = response, na.rm = TRUE) %>%
  # mutate(response = factor(response)) %>% 
  mutate(response = factor(response, 
                          levels = c("Very difficult", "Somewhat difficult",
                                      "Somewhat easy", "Very easy"))) %>% 
  # ggplot(aes(x = bargraph5, fill = response, colour = response)) +
  # geom_bar(aes(fill = response), position = "fill") +
  # labs(x = " ", y = "Porportion of Responses")
  mutate(response = factor(response)) %>% 
  ggplot(aes(x = bargraph5, fill = response, colour = response)) +
  geom_bar(aes(fill = response), position = "fill") +
  labs(x = " ", y = "Porportion of Responses")

bargraph5a <- bargraph5a + labs(title = "Rate these Administrative Processes",
                              subtitle = "undergraduate and graduate students")

print(bargraph5a) # this is the same as 5b

###### reorder the stack and legend

# o <- d %>% filter(Response == "A1") %>% arrange(Percent) %>% extract2("ValueName")
# 
# d %>% 
#   mutate(ValueName = factor(ValueName, o)) %>% 
#   ggplot() +
#   aes(x = ValueName, y = Percent, fill = reorder(Response, plotOrder)) +
#   geom_bar(position = "fill", stat = "identity") +
#   coord_flip()
# https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2

# #random order
# p2 <- ggplot(df[sample(1:10),],aes(x=x,y=y,fill=fill_var))+
#   geom_bar(stat="identity") + labs(title="Random order")
# #legend checks out, sequence wird
# 
# #reverse order
# p3 <- ggplot(df[order(df$fill_var,decreasing=T),],
#              aes(x=x,y=y,fill=fill_var))+
#   geom_bar(stat="identity") + labs(title="Reverse sort by fill")
# 
# plots <- list(p1,p2,p3)
# 
# do.call(grid.arrange,plots)


### try to produce graph with better legend
# but this does not look as good

bargraph5$dm2.Q29_1 = factor(bargraph5$dm2$Q29_1, 
                   ordered = TRUE,
                   levels = c("Very difficult", "Somewhat difficult",
                              "Somewhat easy", "Very easy"))

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
summary(bargraph5)
bargraph5a <- bargraph5 %>%
  gather(bargraph5, value = response, na.rm = TRUE) %>%
  mutate(response = factor(response)) %>% 
  ggplot(aes(x = bargraph5, fill = response, colour = response)) +
  geom_bar(aes(fill = response), position = "fill") +
  labs(x = " ", y = "Total Responses")

bargraph5a <- bargraph5a + labs(title = "Rate these Administrative Processes",
                                subtitle = "undergraduate and graduate students")

print(bargraph5a)

# plotting likert scale in R
# This works better
summary(bargraph5)

bargraph5b <- bargraph5 %>%
  gather(bargraph5, value = response, na.rm = TRUE) %>%
  mutate(response = factor(response, levels = c("Very difficult", 
                                                "Somewhat difficult","Somewhat easy", "Very easy"))) %>% 
  # ggplot(bargraph5[order(bargraph5$response,decreasing=T),],aes(x = bargraph5, fill = response, colour = response)) +
  ggplot(aes(x = bargraph5, fill = response, colour = response)) +
  geom_bar(aes(fill = response), position = "fill") +
  labs(x = " ", y = "Porportion of Responses")

bargraph5b <- bargraph5b + labs(title = "How Easy are these Administrative Processes?",
                                subtitle = "undergraduate and graduate students")

print(bargraph5b)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

#
bargraph0f <- dm2 %>% 
  pivot_longer(Q30, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar(data=dm2, aes(x = response, y=question, fill=col), position="stack", stat="identity") +
  geom_bar(data=dm2, aes(x = response, y=-question, fill=col), position="stack", stat="identity") 
  # geom_hline(yintercept = 0, color =c("white")) +
  # scale_fill_identity("Percent", labels = mylevels, guide="legend") + 
  # theme_fivethirtyeight() + 
  # coord_flip() +
  # labs(title=mytitle, y="",x="") +
  # theme(plot.title = element_text(size=14, hjust=0.5)) +
  # theme(axis.text.y = element_text(hjust=0)) +
  # theme(legend.position = "bottom") +
  # scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))

print(bargraph0f)

# plotting likert scale in R
# http://rcompanion.org/handbook/E_02.html
XT <- xtabs(~ dm2$Q30, data=dm2)

prop.table(XT) #proportion tables

library(likert)
# Result <- likert(dm2)
# 
# plot(Result,
#      type="heat",
#      low.color = "white",
#      high.color = "blue",
#      text.color = "black",
#      text.size = 4,
#      wrap = 50)


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
df14_longer %>%
  group_by(Graduate) %>% 
  summarize(Age) # what is this

rem <- df14_longer %>% 
  filter(Graduate != "<NA>") ## retain all not with Graduate

undergraduates = subset(rem, Graduate==FALSE) #for undergrads about community
#remove NA
na_ugrd <- na.omit(undergraduates) 

glimpse(undergraduates)

# this graph tells me very little
ggplot(na_ugrd, aes(x=community, y=value)) +
  geom_point(shape=1)    +  # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
#> `geom_smooth()` using method = 'loess'

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

# stacked Bar graph for Q3
bargraph0g <- dm2 %>% 
  pivot_longer(Q3, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students, colour = Students)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..), vjust=0) +
  geom_boxplot() +
  # scale_color_brewer(palette = "Blues") +
  labs(x = "", y = "Number of Students")

bargraph0g <- bargraph0g + labs(title = "How effective are UArizona's Systems?",
                                subtitle = "") 
print(bargraph0g)

