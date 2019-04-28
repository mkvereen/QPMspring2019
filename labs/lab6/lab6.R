#####################################################
## File: Lab6.R                                    ##
## t-test and chi-squared test                     ##
#####################################################

#chi sq: when u have constitutency table, u are using proprotions comparing all of the values
#not just a certain mean or point estimate 

## Goals:
## 1. Difference in means test in R
## 2. Contingency tables in R - difference in means and proprotions 


##
## 1. Difference in means test
##
help(t.test) 

getwd()
setwd("~/Documents/GitHub/QPMspring2019/labs")
social <- read.csv("lab6/social.csv")
colnames(social)

# Prior to the 2006 primary, randomly selected voters received one of 
# the following "treatment" messages:
#  - Civic Duty (It's your duty to vote)
#  - Hawthorne (You're being studied)
#  - Neighbors (Your neighbors will know if you voted)
# Those who received one of these treatments are coded as "Treatment".

# Q: Do these treatments (social pressures) increase turnout?
#null: turnout was the same
#alt: they are not equal to each other 

# Source: Gerber, Green, and Larimer. 2008. "Social Pressure and Voter 
# Turnout: Evidence from a Large-Scale Field Experiment." randomly selecting people
#to see what social pressures worked to get people to go vote
# American Political Science Review 102 (1).

table(social$messages)
table(social$treatment) #shows us the ppl who recieved treatment v control group who got nada

table(social$treatment, social$primary2006) # 0 not voted, 1 voted


# Pre-treatment difference in 2004 - t.test (outcome interested in ~ thing that diffenrentiates, what dataset it coems from, 2 sided (vs less or greater), almost always false bc ur assuming differences
t.test(primary2004 ~ treatment, data=social,
       alternative="two.sided", var.equal=FALSE) # default options

# p>.05 so we dont have enough info to reject null

# Post-treatment difference in 2006
t.test(primary2006 ~ treatment, data=social,
       alternative="two.sided", var.equal=FALSE) # default options

#true difference in means is not equal to 0 aka we can def reject 

# One-tailed test - just looking at those who recieve treatment 
t.test(primary2006 ~ treatment, data=social,
       alternative="less")

t.test(primary2006 ~ treatment, data=social, #hypothesis not rejected
       alternative="greater")


# If we assume equal variance, use var.equal=TRUE


# Alternatively, we can use two outcome vectors - a diff visualization
treat.turnouot <- social$primary2006[social$treatment=="Treatment"]

control.turnout <- social$primary2006[social$treatment=="Control"]

t.test(treat.turnouot, control.turnout) # two-tailed




##
## 2. Contingency tables and Chi-squared test
##
help(chisq.test)


# Let's MAKE a fake contingency table for gender and party ID
fake.tab <- matrix(c(762, 327, 468, 484, 239, 677), nrow=2, byrow=TRUE)
dimnames(fake.tab) <- list(Gender=c("Female", "Male"),
                           Party=c("Democrat", "Independent", "Republican"))
fake.tab <- as.table(fake.tab)
fake.tab

help(dimnames)
?list
# The null hypothesis we want to test is that people's party identification 
# is independent of their gender.


# Chi-squared test - since it's already in a contigency table u can just do it 
#simply like this. does same things as before, x-squared is higher than critical value 
#find critical value online tbh, but anyway p-value is so low u can reject null 
chisq.test(fake.tab)








#### Group Work ####


# 1. Write the names of all group members.

harris, lizzie, mk 


# 2. Prior to the 2004 election, voters were contacted and randomly 
#    given either a positive or negative message on an issue. The messages 
#    differed only in whether the information critiqued the incumbent 
#    Republican administration (negative frame) or extoled the benefits of 
#    Democratic proposals (positive frame).

# Source: Arceneaux, Kevin and DavidW. Nickerson. 2010. "Comparing Negative 
# and Positive CampaignMessages: Evidence From Two Field Experiments." 
# American Politics Research 38:54-83.

negative <- read.csv("Lab6/negative_ads.csv")
colnames(negative)

# Do negative messages make a difference for voter turnout? Conduct an
# appropriate test and interpret the result.

#null hypothesis: negative messages have no effect on voter turnout, that treatment is equal to control
#alt: true diff in means is not equal to zero
help(t.test)

# treatment: 1 = negative frame (control), 2 = positive frame (treatment)
# voted02p: 0 = did not vote, 1 = voted

#  t.test (outcome interested in ~ thing that diffenrentiates, what dataset it coems from, 2 sided (vs less or greater), almost always false bc ur assuming differences

                                           
t.test(voted02p ~ treatment, data=negative, alternative="two.sided", var.equal=FALSE)

#neg mess have positive effect - u would do one-sided, it would be greater than
t.test(voted02p ~ treatment, data=negative, alternative="greater", var.equal=FALSE)
#alternative hypothesis: true difference in means is greater than 0
#null: true difference in means is less than 0

# 3. In the movie Titanic, the third class passengers were not allowed 
#    to go up to the deck where the life-boats were boarding because the 
#    first-class passengers would board before them. This led to a sequence 
#    of tragic events.

install.packages("titanic")
library(titanic)
my.titanic <- titanic_train
View(my.titanic)

#    (1) Make a contingency table of the observed frequencies for the survival 
#    of passengers with respect to their class.
#    Hint: use table().

my.titanic$Survived
colnames(my.titanic)

questiontab<-table(my.titanic$Survived, my.titanic$Pclass)



#    (2) Conduct a necessary hypothesis test with the null that survival and 
#    passanger class are independent. Interpret the result.

chisq.test(questiontab)

#reject hypothesis that class and survival rate are independent of each other
#p-value is smoll

################ Additional Question ################ 

# 4. Some claimed that the "women and children first" evacuation policy 
#    helped them escape regardless of their class. Using only the women in 
#    the sample, make a contingency table to test this assertion. Do you think 
#    this evacuation policy worked?


my.titanic$Sex
subset(my.titanic$Sex$"female")

womensample<- my.titanic[my.titanic$Sex=="female",]
MyNewTable <- table(womensample$Survived, womensample$Pclass)
chisq.test(MyNewTable)
#if policy was that women adn children get off first, there should be no diff based on
#survival in terms of class, so we shouldnt see such a large test score
#bc it's so large it shows that there are diffs based on class even when just women



