# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# Jeff wd
setwd("C:/Users/ziegl/Google Drive/WashU/teaching/UCollegeSpring2019/problemSets/PS3")
#setwd('~/Google Drive/WashU/teaching/UCollegeSpring2019/problemSets/PS3')

############
# Question 1
############

# install packages and load library/data
install.packages("faraway")
library("faraway")
data("newhamp")

# open up .pdf
pdf("Q1.pdf")
# plot of voters for Dean
# by voters for Obama
# this will plot only those where the votes
# were counted by machine
plot(newhamp$Dean[newhamp$votesys=="D"], 
     newhamp$pObama[newhamp$votesys=="D"],
     col="gold4", pch=18,
     xlim=c(min(newhamp$Dean), max(newhamp$Dean)),
     ylim=c(min(newhamp$pObama), max(newhamp$pObama)),
     xlab="Proportion of Voters for Dean in 2004",
     ylab="Proportion of Voters for Obama in 2008",
     main="", cex.lab=1.25)

# now add points for wards with hand counting
points(newhamp$Dean[newhamp$votesys=="H"], 
       newhamp$pObama[newhamp$votesys=="H"],
       col="darkblue", pch=2)

# add legend to bottom 
legend("bottomright", 
       pch=c(18,2), 
       col=c("gold4", "darkblue"),
       legend=c("Machine-Count", "Hand-Count"),
       cex=1.25)
# close .pdf!
dev.off()

############
# Question 2
############

# set.seed() for reproducibility
set.seed(5)
# create vector of data for all values of x
x <- seq(from=-5, to=5, by=.1)
# open up .pdf
pdf("Q2.pdf")
# create line plot for the distribution function 
# of the normal distribution
plot(x, dnorm(x), lwd=3, type="l", col=1, lty=1,
     ylab="Density of probability distrubtion")
# add line for t-distribution w/ DF=10
lines(x, dt(x, df=10), lwd=3, ylim=c(0, .4), col=2, lty=2)
# add line for t-distribution w/ DF=5
lines(x, dt(x, df=5), lwd=3, ylim=c(0, .4), col=3, lty=3)
# add line for t-distribution w/ DF=1
lines(x, dt(x, df=1), lwd=3, ylim=c(0, .4), col=4, lty=4)
# open legend and edit values in legend
legend("topleft", 
       c("n(0,1)", "t(df=10)", "t(df=5)", "t(df=1)"), 
       lty=c(1,2,3,4), col=c(1,2,3,4), bty="n")
dev.off()

############
# Question 3
############

# load data from Zelig
library("Zelig")
data("voteincome")

# how many observations are there w/o NAs
n <- length(na.omit(voteincome$age))
# get mean age (remember to remove NAs)
mean_age <- mean(voteincome$age, na.rm = T)
# get standard dev. for age
sd_age <- sd(voteincome$age, na.rm = T)

# calculate standard error
std_error <- sd_age / sqrt(n)
# view SE
std_error
# [1] 0.4511027

# calculate t statistic
test_stat <- (mean_age - 50)/std_error
# view t statistic
test_stat
#[1] -1.637469

# double the lower tail because sample
# mean is less than hypothesized value
p_val <- 2*pnorm(test_stat, mean = 0, sd = 1, lower.tail = TRUE)
# view p-value
p_val
# [1] 0.1015326

# generate test statistic for CIs
# i.e. the value of the normal distribution's CDF
# for the value of x we're intersted in (95%)
# two tailed, so x=0.025
z_score <- qnorm(.025, lower.tail = FALSE)
# calculate lower bound
lwr_bound <- mean_age - z_score * (sd_age/sqrt(n))
# calculate upper bound
upr_bound <- mean_age + z_score * (sd_age/sqrt(n))
# view CIs
c(lwr_bound, upr_bound)
#[1] 48.37719 50.14548
