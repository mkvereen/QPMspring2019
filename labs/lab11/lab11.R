###########################################
## File: Lab11.R                         ##
## Chi-squared test                      ##
###########################################



## Goals:
## 1. Contingency tables in R
## 2. Visualizing contingency tables


##
## 1. Contingency tables and Chi-squared test
##
help(chisq.test)


# Let's make a fake contingency table for gender and party ID
fake.tab <- matrix(c(762, 327, 468, 484, 239, 677), nrow=2, byrow=TRUE)
dimnames(fake.tab) <- list(Gender=c("Female", "Male"),
                           Party=c("Democrat", "Independent", "Republican"))
fake.tab <- as.table(fake.tab)
fake.tab

# The null hypothesis we want to test is that people's party identification 
# is independent of their gender.


# Chi-squared test
chisq.test(fake.tab)

# 2. Visualizing contingency tables

# Example: estimate association between aspirin treatment 
# and cancer incidence in a 2x2 contingency table.

# Read and inspect the data
cancer <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e2AspirinCancer.csv"))
head(cancer)

# create table between cancer and treatment
cancerTable <- table(cancer$cancer, cancer$aspirinTreatment)

# open plot
mosaicplot( t(cancerTable), col = c("firebrick", "goldenrod1"), 
            cex.axis = 1, sub = "Aspirin treatment", ylab = "Relative frequency", main = "")


#### Group Work ####


# 1. Write the names of all group members.


# 2. Example: 2008 GSS

#``Please tell me whether or not you think it should be possible for a
# pregnant woman to obtain a legal abortion if the family has a very low
# income and cannot afford any more children?''

# here is the data
abort.tab <- matrix(c(222, 225, 201, 277, 113, 223, 18, 12), nrow=2, byrow=TRUE)
dimnames(fake.tab) <- list(Gender=c("Yes", "No"),
                           Party=c("Democrat", "Independent", "Republican", "Other"))


#    (1) Make a contingency table of the observed frequencies for the preference regarding 
#    abortion with respect to respondents' party
#    Hint: use table().
#    (2) Conduct a necessary hypothesis test with the null that opinion and 
#    party are independent. Interpret the result.


################ Additional Question ################ 

# 4. Some claimed that the "women and children first" evacuation policy 
#    helped them escape regardless of their class. Using only the women in 
#    the sample, make a contingency table to test this assertion. Do you think 
#    this evacuation policy worked?



