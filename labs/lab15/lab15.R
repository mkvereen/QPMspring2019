###############################
## Lab11.R                   ##
## DID and RD                ##
################################


## Goals
## 1. Visualize DID
## 2. Find the DID estimate of the effect of the treatment
## 3. Visualize RD
## 4. Find the RD estimate of the effect of the treatment


#### Difference-in-Differences ####

## Package to read in .dta files
#install.packages("foreign")
library("foreign")

## EITC (Earned Income Tax Credit) expansion on employment of single women
eitc <- read.dta("eitc.dta")

## Create dummy variable to indicate treatment and control groups
## EITC only affected women with at least 1 child
eitc$anykids <- (eitc$children >= 1)

## Create dummy variable to indicate before and after policy change
## EITC went into effect in 1994
eitc$post <- (eitc$year > 1993)

## Our outcome variable is "work" (is the woman employed or not)
## Now to compute the DID estimate, we need four data points.
## What are they?
pre_cont <- mean(eitc$work[eitc$post == FALSE & eitc$anykids == FALSE])
pre_treat <- mean(eitc$work[eitc$post == FALSE & eitc$anykids == TRUE])
post_cont <- mean(eitc$work[eitc$post == TRUE & eitc$anykids == FALSE])
post_treat <- mean(eitc$work[eitc$post == TRUE & eitc$anykids == TRUE])

## Let's visualize the data
plot(x = c(0, 0, 1, 1), ## 0 is before, 1 is after
     y = c(pre_cont, pre_treat, post_cont, post_treat),
     col = c("red", "blue", "red", "blue"), ## control group is red
     pch = 16,
     axes = FALSE,
     ylab = "Mean of Employment Dummy",
     xlab = "",
     main = "Visual of DID",
     xlim = c(-.5, 1.5),
     ylim = c(.43, .62))
box()
axis(2)
axis(1, at = c(0,1), labels = c("Before", "After"))
abline(v=.5, lty=3) ## indicate policy change
legend("topleft", c("No Kids (control)", "Any kids (treatment)", "Counterfactual"), 
       bty = "n", cex = .75, col = c("red", "blue", "blue"), lty = c(1, 1, 2))

## Don't worry about this code right now... just look at the picture
## We are visualizing the "parallel trends" assumption
control_diff <- (pre_cont-post_cont) ## What is this?
lines(x = c(0,1), y = c(pre_cont, post_cont), col = "red")
lines(x = c(0,1), y = c(pre_treat, pre_treat - control_diff), ## counterfactual
      col = "blue", lty =  2)
points(x = 1, y = pre_treat - control_diff, pch = 16, col = "blue")

## This is what actually happened...
lines(x = c(0,1), y = c(pre_treat, post_treat), col = "blue")

## So what quantity do we want?  What is the effect of the treatment?
lines(x = c(1,1), y = c(pre_treat - control_diff, post_treat), lty = 1, lwd = 2)
text(x = 1.3, y = .47, "Effect of\ntreatment", cex = .7)

## Easy way to get this from our means calculated above
## Difference (between treatment and control groups) in
## differences (between before and after treatment)!
## This calculation follows from the name!  Ask if this doesn't make sense!

# (treatment difference) -  (control difference)
(post_treat - pre_treat) - (post_cont - pre_cont)


## We can also get this quantity using a linear model
## Interact the before/after with the treatment/control groups
eitc_mod <- lm(work ~ post + anykids + post:anykids, data = eitc)
summary(eitc_mod)



#### Regression Discontinuity ####

## Research Question: How much can politicians increase their personal
## wealth due to holding office?
## Dataset of MPs in the UK
## - ln.net is log net wealth at time of death
## - margin is margin of victory (vote share)
## Note there are negative margins!  That means this dataset has info
## on losers of elections!  This, crucially, allows us to get at the
## counterfactual of what happens to wealth when someone loses an election.

## In other words, these data are good for RD because we can look at
## those who barely won and barely lost and see the difference in wealth
mps <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/MPs.csv"))

## subsets based on party
labour <- mps[mps$party == "labour", ]

## now fit two regressions for each party, 
labour_fitpos <- lm(ln.net ~ margin, data = labour[labour$margin < 0, ]) ## losers
labour_fitneg <- lm(ln.net ~ margin, data = labour[labour$margin > 0, ]) ## winners


## now calculate predicted values over the range of the data
neg_range <- c(min(labour$margin), 0)
pos_range <- c(0, max(labour$margin))

## don't worry about this code, just understand we are filling X values
## into our prediction equations (e.g., Y = 12.304 + 1.245X)
neg_preds <- predict(labour_fitpos, newdata = data.frame(margin = neg_range))
pos_preds <- predict(labour_fitneg, newdata = data.frame(margin = pos_range))

## Now plotting
## first, the raw data for Labour party
## Verticle line at 0, as this is where our cutoff was
plot(y = labour$ln.net, x = labour$margin, pch = 16, cex = .5,
     xlab = "Margin of Victory", ylab = "Log net wealth at Death",
     main = "Labour Party Regression Discontinuity")
abline(v = 0, lty = 2) ## where we split data into 2 regressions

## Now a line for the predicted values for each regression
lines(x = neg_range, y = neg_preds, col = "red", lwd = 2)
lines(x = pos_range, y = pos_preds, col = "red", lwd = 2)


## Key point: difference in predicted values at the point of
## discontinuity between the two regressions is the effect of the 
## "treatment" (serving as an MP) on personal wealth

## Picking out the element we want, by hand, from prediction vectors
## Note: could also use y-intercepts from regression since our cutoff is zero!
labour_mp <- exp(pos_preds[1]) ## exp() to "reverse" the log scale
labour_nonmp <- exp(neg_preds[2])
labour_mp - labour_nonmp
## MPs earn 67,900 less pounds


## Or we can do this with one regression
## We have an interaction to vary the intercept and the slope
## of the lines before/above the threshold
labour$above_thresh <- ifelse(labour$margin > 0, 1, 0)
labour_mod <- lm(ln.net ~ margin*above_thresh, data = labour)
summary(labour_mod)


## Use coefficients to get jump at threshold
## Prediction below threshold minus prediction above
## Remember all we care about is coefficient on indicator of threshold!
alpha <- labour_mod$coefficients["(Intercept)"]
beta2 <- labour_mod$coefficients["above_thresh"]
exp(alpha) - exp(alpha + beta2) ## same prediction









#### Group Work ####

## 1. Write the names of all group members



## 2. Consider the following notable example of a DID study (Card and Krueger 1994).
## New Jersey's minimum wage rose from $4.25 to $5.05 in April 1992.  These authors
## compared employment in the fast food sector in New Jersey and Pennsylvania
## in February and November 1992 to identify the effect of the minimum wage increase.
## These are their employment data, measured in full-time equivalent (google it if you want)
## February, New Jersey:    20.44
## February, Pennsylvania:  23.33
## November, New Jersey:    21.03
## November, Pennsylvania:  21.17
## Calculate the difference-in-differences estimate.




## 3. Interpret your DID estimate.

## the 80 cent minimum wage increase in NJ led to a 2.75 increase in FTE


## 4. Using the MPs dataset, perform a similar analysis for the Tory party.
## Specifically, do the following:
## 4a) Fit separate regression lines for winners and losers.
## 4b) Produce predicted values across the range of the data.
## 4c) Plot the raw data with the 2 lines of predicted values.
## 4d) Estimate the treatment effect at the point of discontinuity.
## 4e) Interpret your finding.  Is the effect of being an MP on wealth
##     different depending on party?
## Show your TA your plot before you leave.





## Note:
## DID example taken from Kevin Goulding's blog
## https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/
## RD example taken from Kosuke Imai's book Quantitative Social Science


