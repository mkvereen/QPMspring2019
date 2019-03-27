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
#setwd("C:/Users/ziegl/Google Drive/WashU/teaching/UCollegeSpring2019/problemSets/PS2")
setwd('~/Google Drive/WashU/teaching/UCollegeSpring2019/problemSets/PS2')

############
# Question 2
############

# (d) Find the probability that her sample mean falls within 0.5 of the population mean.
# get lower probability
pnorm((-0.5/(3/sqrt(36))), lower.tail = F)
pnorm((0.5/(3/sqrt(36))), lower.tail = F)

# (e) Suppose she takes a random sample of size 100. 
# Find the probability that the sample mean falls within 0.5 of the true mean,
# and compare the answer to that in (d).
pnorm((-0.5/(3/sqrt(100))), lower.tail = F)
pnorm((5/(3/sqrt(100))), lower.tail = F)

# (f) Refer to (e). If the sample were truly random, 
# would you be surprised if the anthropologist obtained $\bar{y}=4$. Why?
pnorm(((4-5.2)/(3/sqrt(100))), lower.tail = T)

############
# Question 4
############

# (a) (1 point) At or below the value 57.75
pnorm(57.75, mean=50, sd=6, lower.tail=TRUE)

#(b) (1 point) At or above the value of 50.45
pnorm(50.45, mean=50, sd=6, lower.tail=FALSE)

# (c) (2 points) Between the values of 52.4 and 59.4
## create an object "a":
a <- pnorm(52.4, mean=50, sd=6, lower.tail=TRUE)
  
# show the stored probability in object "a"
a

# create a second object "b":
b <- pnorm(59.4, mean=50, sd=6, lower.tail=TRUE) 

# show the stored probability in object "b"
b

# substract "a" from "b":
b-a

############
# Question 5
############

# set seed for reproducibility
set.seed(12345) 
# create a sample of salaries
salaries <- rnorm(n=10000,mean=40000,sd=15000)
# plot distribution by opening up .pdf
pdf (file="densitySalaries.pdf" # file name
     , width = 6  # plot width (in inches)
     , height = 4 # plot height
)
plot(density(salaries), main="")
dev.off()

############
# Question 6
############

# create 3 rows and 2 columns, so that the plots
# fit on one page
par(mfrow=c(3,2))
# set range of values for x-axis
x <- seq(-10, 10, length=100)

# Distribution A: 
plot(x, dnorm(x, mean=0, sd=sqrt(0.4)), xlab="x value",
     type="l", ylim=c(0,0.8), ylab="Density", 
     main=expression(paste(mu, "=0, ", sigma^2, "=0.4")))
# Distribution B: 
plot(x, dnorm(x, mean=0, sd=sqrt(3)), xlab="x value", 
     type="l", ylim=c(0,0.8), ylab="Density",
     main=expression(paste(mu, "=0, ", sigma^2, "=3")))
# Distribution C: 
plot(x, dnorm(x, mean=3, sd=sqrt(3)), xlab="x value",
     type="l", ylim=c(0,0.8), ylab="Density", 
     main=expression(paste(mu, "=3, ", sigma^2, "=3")))
# Distribution D: 
plot(x, dnorm(x, mean=3, sd=sqrt(0.4)), xlab="x value", 
     type="l", ylim=c(0,0.8), ylab="Density", 
     main=expression(paste(mu, "=3, ", sigma^2, "=0.4")))
# Distribution E: 
plot(x, dnorm(x, mean=-2, sd=sqrt(5)), xlab="x value", 
     type="l", ylim=c(0,0.8), ylab="Density", 
     main=expression(paste(mu, "=-2, ", sigma^2, "=5")))
# Distribution F: 
plot(x, dnorm(x, mean=-2, sd=sqrt(1/4)), xlab="x value", 
     type="l", ylim=c(0,0.8), ylab="Density", 
     main=expression(paste(mu, "=-2, ", sigma^2,"=",frac(1,4))))

# Easier, but more advanced:
# create function since you're doing it 6 times
pdfPlotFunction <- function(avg=0, standDiv=sqrt(0.4)){
  x <- seq(-10, 10, length=100)
  mainText <- bquote(bold(mu == .(avg) ~ ", " ~ sigma^2 == .(standDiv)))
  plot(x, dnorm(x, mean=avg, sd=standDiv), xlab="x value", 
       type="l", ylim=c(0,0.8), ylab="Density", 
       main="")
  mtext(mainText, side=3)
}

# open up .pdf and save
pdf("Q6.pdf")
par(mfrow=c(3,2))
pdfPlotFunction(avg=0, standDiv=sqrt(0.4))
pdfPlotFunction(avg=0, standDiv=sqrt(3))
pdfPlotFunction(avg=3, standDiv=sqrt(3))
pdfPlotFunction(avg=3, standDiv=sqrt(0.4))
pdfPlotFunction(avg=-2, standDiv=sqrt(5))
pdfPlotFunction(avg=-2, standDiv=sqrt(1/4))
dev.off()

############
# Question 7
############

# (a) (1 point) Draw a histogram of the monthly count of drug-related stories.
library(date)
drug <- read.csv("drugCoverage.csv")

pdf("drugsMediaHist.pdf")
hist(drug$drugsmedia, main = "", xlab = "Number of drug-related stories per month", ylab="Number of months")
dev.off()


# (b) (3 points) Draw two boxplots: One of drug-related stories and 
# another of presidential approval. How do these figures differ and 
# what does that tell you about the contrast between the variables?

pdf("drugsBoxplot.pdf")
par(mfrow=c(1,2))
boxplot(drug$drugsmedia)
boxplot(drug$approval)
dev.off()

# (c) (3 points) Draw two scatterplots:
# First, represent the number of drug-related
# stories on the vertical axis, and place the unemployment rate on the horizontal axis.
# Second, represent the number of drug-related stories on the vertical axis,
# and place presidential approval on the horizontal axis.
pdf("drugsScatterplot.pdf")
par(mfrow=c(1,2))
plot(drugsmedia~unemploy,data=drug, ylab = "Drug-related stories", xlab = "Unemployment")
plot(drugsmedia~approval,data=drug, ylab = "Drug-related stories", xlab = "Presidential Approval")
dev.off()

# (d) (3 points) Draw two line graphs:
# First, draw the number of drug-related stories by month over time.
# Second, draw presidential approval by month over time.
pdf("drugsLineplot.pdf")
par(mfrow=c(1,2))
plot(drug$drugsmedia, type="l", ylab = "Drug-related stories", xlab = "Time")
plot(drug$approval, type="l", ylab = "Presidential Approval", xlab = "Time")
dev.off()

############
# Question 8
############

# (a) (2 points) Import data on the 88th and 107th Congresses. 
# Then, create four subsets of the data by session and party 
# (Democratic Party in the 88th session, Democratic Party in the 107th session,
# Republican Party in the 88th session, and Republican Party in the 107th session).
wnominate <- read.csv("wnominatehouse.csv")
dem2 <- wnominate[wnominate$congress==88 & wnominate$party==100,]
dem3 <- wnominate[wnominate$congress==107 & wnominate$party==100,]
rep2 <- wnominate[wnominate$congress==88 & wnominate$party==200,]
rep3 <- wnominate[wnominate$congress==107 & wnominate$party==200,]

# (b) (2 points) For the Democratic Party, calculate the median 
# W-NOMINATE scores for two Congresses. 
# How did the median change over time? What does this mean?
median(dem2$wnominate)
median(dem3$wnominate)

#(c) (2 points) For the Republican Party, calculate the median 
# W-NOMINATE scores for the two Congresses. 
# How did the median change over time? What does this mean?
median(rep2$wnominate)
median(rep3$wnominate)

# (d) For the Democratic Party, calculate the standard
# deviation of W-NOMINATE scores for the two Congresses.
# How did the standard deviation change over time? 
# What does this mean?
sd(dem2$wnominate)
sd(dem3$wnominate)

# (e) (3 points) For the Republican Party, calculate the 
# standard deviation of W-NOMINATE scores for
# the two Congresses. How did the standard deviation
# change over time? What does this mean?
sd(rep2$wnominate)
sd(rep3$wnominate)

# (f) (2 points) For the 88th Congress, create a plot that overlays two histograms. 
# One histogram should plot the distribution of W-NOMINATE scores for the Democratic Party. 
# The other histogram should plot the distribution of W-NOMINATE scores for the Republican Party.

pdf("wnominateHist88.pdf")
hist(rep2$wnominate, xlim=c(-1,1), col=rgb(1,0,0,0.7), 
     main="88th Congress", xlab = "W-NOMINATE Score")
hist(dem2$wnominate, xlim=c(-1,1), col=rgb(0,0,1,0.7), add=T)
dev.off()

# (g) (2 points) For the 107th Congress, create a plot that overlays two histograms.
# One histogram should plot the distribution of W-NOMINATE scores for the Democratic Party. 
# The other histogram should plot the distribution of W-NOMINATE scores for the Republican Party.

pdf("wnominateHist107.pdf")
hist(rep3$wnominate, xlim=c(-1,1), col=rgb(1,0,0,0.7), 
     main="88th Congress", xlab = "W-NOMINATE Score")
hist(dem3$wnominate, xlim=c(-1,1), col=rgb(0,0,1,0.7), add=T)
dev.off()

