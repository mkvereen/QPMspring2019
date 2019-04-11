############################################
## File: Lab12.R                          ##
## Bivariate Regression in R              ##
############################################


## Goals:
## 1. Run a bivariate regression in R
## 2. Plot the regression line
## 3. Visualize and assess residuals


## Data on 1932 German Election (at precinct level)
## Paper: G. King, O. Rosen, M. Tanner, A.F. Wagner (2008)
## “Ordinary economic voting behavior in the extraordinary
## election of Adolf Hitler.” 
setwd("~/Documents/GitHub/QPMspring2019/labs/lab12")
load("nazis.Rdata")


## Variables
## nvoter: number of eligible voters
## nazivote: number of votes for Nazis
## shareself: proportion of self-employed potential voters
## shareunemployed: proportion of unemployed potential voters
## sharewhite: proportion of white-collar potential voters

## DV for today -- nazi share of the vote in each precinct
nazis$nazishare <- nazis$nazivote / nazis$nvoter 
hist(nazis$nazishare)
#normally distributed 

## lm() function is for regression aka "linear model"
?lm

#what to put into R
#lm(formula, data, subset, weights, na.action,
#method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#singular.ok = TRUE, contrasts = NULL, offset, ...)



#for us Y~X --> % nazis ~ X

## Bivariate regression where outcome variable is share of 
## vote for Nazis and explanatory variable is proportion of
## self-employed voters
## Theory:  those who were hurt by the economy but were at little risk of
## unemployment -- such as self-employed shopkeepers and professionals,
## were the groups that gave the most disproportionate support to the Nazis
mod_self <- lm(nazishare ~ shareself, data = nazis)
# outcome (D) first, explanatory second (I)
#remember u can use $ to call objects within R 

## summary() function displays detailed model output
## Do you see beta, standard error, tvalue, and pvalue?
summary(mod_self)
mod_self$coefficients

 #interperet intercept:if share of self employee voters was 0, % nazi voters 
 #would be 33% (alpha interp)
 #interperet beta: for every unit of self employee voters, .45% increase in n voters
 #estimates tell us magnitude and direction, but not how sure we can be 
 #about those estimates.

#tvalue and how sure we are:
#use null hypo: assumption that beta is 0 - there is no relationship/effect
#given that t value is large in this example, how likely is it that we 
#would observe the sample relationship (beta and alpha) if beta was zero
#it's clear that this relationship is not 0. 
#this test is kinda basic, it should be pretty easy to assume that the
#relationship is not 0 

#heuristic of wanting t value around 2 

## Note that beta/std error = tvalue
## Why? What were the null and alternative hypotheses?
.455/.08175


## What is our conclusion?
## How do we interpret the coefficient on shareself?


## Confidence interval for our slope estimate
tval <- qt(1-(.05/2), df = 681-2, lower.tail = TRUE) ## high n, so approx 1.96
.455 - (tval*0.08175)
.455 + (tval*0.08175)


## Let's plot our line against the data
plot(x = nazis$shareself, y = nazis$nazishare, pch = "+",
     cex = .75, xlab = "Share Self-Employed", ylab = "Nazi Share of Vote",
     main = "1932 German Election Results,\nby Precinct")


## We can add our fitted line in 2 ways
abline(mod_self, col = "red", lwd = 2) ## use the model itself
abline(a = .3310, b = .4554, col = "blue", lwd = 2)  ## use coefficient results


## Let's check out the residuals for the first 10 observations
## Part of the model object
mod_self$residuals[1:10]


## Remember residuals are the observed value - fitted value
nazis$nazishare[1:10] - mod_self$fitted.values[1:10]


## We should look at the residuals because when we use OLS,
## we assume errors are normally distributed!  Let's check.


## Residuals against fitted values
plot(x = mod_self$fitted.values, y = mod_self$residuals)
abline(h = 0)


## Like we saw above, not as many large, positive residuals,
## but this is pretty good
hist(mod_self$residuals)








####### Group Work ####### 


## Write the name of all group members.





## Load the following SAT data.
#install.packages("faraway")
library(faraway)
data(sat)
?sat




## (1) Plot expenditures per pupil as your explanatory variable and
## % of eligible students taking the SAT as our outcome.  Do these data
## look like good candidates for OLS?


plot(sat$expend, sat$total)
#seems to be a negative linear trend, so let's try OLS. also our
#outcome is continuous. there is no such thing as a negative SAT score tho
#when u learn new models, maybe use them

## (2) Run a bivariate linear regression to test the hypothesis that
## expenditures are associated with more students taking the SAT


satmodel<-lm(total ~ expend, data=sat)



## (2) What are the null and alternative hypotheses?

#doesnt tell us if intercept is reliable, null for all is that intercept
#is not 0. this doesnt rly tell us that much. 

summary(satmodel)
#null: it should be 0, there is no relationship. x is unrelated to y


## (3) Interpret the effect of the coefficient on "expend".


#alt: t stat is v large, so our interpretation of the effect of expenditure
#is that if they spent $0 on that student, on avg students would get 1089
#as their SAT score

#beta on expend: for every additional $, the score goes down by 20 points
#this doesnt make sense. tells us that the relationship is weird or maybe
#we arent using the right model -it's not a linear relationship
#if u think abt this that makes sense, there are prob diminishing marginal returns



## (4) Plot your fitted line against the data.  Do you think
## the line fits the data well?


plot(sat$expend, sat$total)
abline(satmodel)


## (5) Visually examine the residuals.  Does your evaluation of the use
## of OLS with these data change?  In other words, is there something
## wrong with your residual plot?


hist(satmodel$residuals)
abline(v=0, col="red")
#in theory, verticle should be at zero, but in reality let's do it at mean
abline(v=mean(satmodel$residuals), col="blue")

#these are rly similar, so it seems our assumption held - the mean for
#our disturbances is 0


