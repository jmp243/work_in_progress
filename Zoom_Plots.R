# Zoom Plots
# Jung Mee Park
# June 19, 2021

#### UCLA tutorial
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
lapply(dm2[, c("Q3", "Q7", "Graduate")], table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ Q3 + Q7 + Graduate, data = dm2))

# remove the NA's
dat <- dm2 %>% mutate_all(na_if,"")

# ggplot
# plot zoom
#
##### variable values on condition
# recode graduate
dm2$Graduate <- as.factor(dm2$Graduate) 
levels(dm2$Graduate)


## remove NA
# this command is useful for filtering
df <- dm2 %>% filter(!is.na(Q3)) %>% filter(!is.na(Q7)) %>% 
  dplyr::mutate(Graduate = recode(Graduate, "2"="Graduate", "1"="Undergraduate"))

zoom_plot <- ggplot(df,  aes(x = Age, y = Q3, colour=Age)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Graduate ~ Q7, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

zoom_plot + labs(y = "Overall University Systems")
zoom_plot + labs(title = "Efficacy of University Systems by Age of Users and Emails Per Week",
                 subtitle = "number of emails per week")



#### UCLA tutorial
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
lapply(dm2[, c("Q15", "Q16", "Students")], table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ Q15 + Q16 + Students, data = dm2))

# remove the NA's
dat <- dm2 %>% mutate_all(na_if,"")

# reorder factor for Q15 and Q 16
dm2$Q15 = factor(dm2$Q15, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))
dm2$Q16 = factor(dm2$Q16, 
                 ordered = TRUE,
                 levels = c("Very difficult", "Somewhat difficult", 
                            "Somewhat easy", "Very easy"))

# ggplot
# plot zoom
#
##### variable values on condition
# recode graduate
dm2$Career <- as.factor(dm2$Career) 
levels(dm2$Career)


## remove NA
# this command is useful for filtering
df <- dm2 %>% filter(!is.na(Q15)) %>% filter(!is.na(Q16)) 
 # %>% 
 # dplyr::mutate(Career = recode(Career, "2"="Graduate", "1"="Undergraduate"))

zoom_plot2 <- ggplot(df,  aes(x = Students, y = Q15, colour=Students)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Students ~ Q16, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

zoom_plot2 + labs(title = "Finding Student Groups or Study Partners",
                 subtitle = "How Easy is to find Study Partners?") +
                 xlab("Career") +
                 ylab("Finding Student Groups") 

summary(dm2$Q15) # 90% response rate
summary(dm2$Q16) # 90% response rate
#### UCLA tutorial
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
lapply(dm2[, c("Q30", "Q3", "Students")], table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ Q30 + Q3 + Students, data = dm2))

# remove the NA's
dat <- dm2 %>% mutate_all(na_if,"")

# ggplot
# plot zoom
#
##### variable values on condition
# recode graduate
# dm2$Career <- as.factor(dm2$Career)
# levels(dm2$Career)
# 

## remove NA
# this command is useful for filtering
df <- dm2 %>% filter(!is.na(Q30)) %>% filter(!is.na(Q3)) 
# %>% 
# dplyr::mutate(Career = recode(Career, "2"="Graduate", "1"="Undergraduate"))

zoom_plot3 <- ggplot(df,  aes(x = Students, y = Q30, colour=Students)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Students ~ Q3, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

zoom_plot3 + labs(title = "Would Students recommend UA?",
                  subtitle = "Efficacy of UA Tools") +
                  xlab("") +
                  ylab("Likeliness of Recommending UA") 

