###########################################
## File: Lab11.R                         ##
## Chi-squared test                      ##
###########################################



## Goals:
## 1. Contingency tables in R
## 2. Visualizing contingency tables
## 3. Odds ratios


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

##
## 2. Visualizing contingency tables
##

# Example: estimate association between aspirin treatment 
# and cancer incidence in a 2x2 contingency table.

# Read and inspect the data
cancer <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e2AspirinCancer.csv"))
head(cancer)

# create table between cancer and treatment
cancerTable <- table(cancer$cancer, cancer$aspirinTreatment)

# open plot
mosaicplot( t(cancerTable), col = c("blue", "goldenrod1"), 
            cex.axis = 1, sub = "Aspirin treatment", ylab = "Relative frequency", main = "")

##
## 3. Odds ratios
##

# Odds = Probability success/Probability of failure

# if you don't have this library, install it
if (!require("epitools")) {install.packages("epitools", dependencies = TRUE, repos="http://cran.rstudio.com/")}
library(epitools)

# calculate odds ratio
oddsratio(cancerTable, method = "wald")

# let's just look at the odds of not having cancer with 95% C.I
oddsratio(cancerTable, method = "wald")$measure[-1,]

# Now, let go step-by-step to calculate the odds ratio and confidence interval
  
# First, assign the labels a,b,c, and d to the table
# Then plug the values into the formula for odds ratio
x <- as.vector(cancerTable)
names(x) <- c("a","c","b","d")
orHat <- as.numeric((x["a"] * x["d"]) / (x["b"] * x["c"]))
## [1] 1.008744

# Second, create standard error of log odds ratio
seLnOR <- as.numeric(sqrt(1/x["a"] + 1/x["b"]+ 1/x["c"] + 1/x["d"]))
## [1] 0.03878475

# Last, create 95% confidence interval
Z <- qnorm(1 - 0.05/2) 
# when rounded, is 1.96
# create lower and upper bound
ciLnOR <- c(log(orHat) - Z * seLnOR, log(orHat) + Z * seLnOR)
# transform back to ratio scale
ciOR <- exp(ciLnOR)
ciOR <- c(orHat, ciOR)
# rename variables
names(ciOR) <- c("estimate", "lower","upper")
# check to see if we got the same answer
ciOR
oddsratio(cancerTable, method = "wald")$measure[-1,]

#### Group Work ####


# 1. Write the names of all group members


# 2. Example: 2008 GSS

#``Please tell me whether or not you think it should be possible for a
# pregnant woman to obtain a legal abortion if the family has a very low
# income and cannot afford any more children?''

# here is the data
abort.tab <- matrix(c(222, 225, 201, 277, 113, 223, 18, 12), nrow=4, byrow=TRUE)
dimnames(abort.tab) <- list(Party=c("Democrat", "Independent", "Republican", "Other"),
                            Opinion=c("Yes", "No"))


#    (1) Make a contingency table of the observed frequencies for the preference regarding 
#    abortion with respect to respondents' party
#    Hint: use table()

#    (2) Conduct a necessary hypothesis test with the null that opinion and 
#    party are independent. Interpret the result


################ Additional Question ################ 

#    (3) Cross-Classification of Race
#     of Victim and Race of Offender for murders in the United States in 2005 having a single victim and single offender

convict.tab <- matrix(c(3150, 230, 516, 2984), nrow=2, byrow=TRUE)
dimnames(convict.tab) <- list(Offender=c("White", "Black"),
                            Defendant=c("White", "Black"))

# The table aboves provides the cross tabs for the race of the victim by the race of the offender
# We treat race of victim as the response variable

# what are the odds for white offenders to be convicted?
# Odds for black offenders?
# Compare the two groups, i.e. for white offenders, 
# the odds of a white victim were about X times the odds of a white victim for black offenders?