############################################
## File: Lab4.R                           ##
## Day 4: Sampling Distribution           ##
############################################


setwd("~/Documents/GitHub/QPMspring2019/labs/lab4")

#IDK WHY THIS DIDN'T WORK

#### Goals
#### a) Working with pnorm and qnorm
#### b) Practical example of a sampling distribution


## Go to the help file
help(Normal)


## 1. To generate random numbers from a normal distribution,
##    use rnorm(n=, mean=, sd=)

#set seed tells u what algorithm to use to get random set of numbers
#this way u can have other people look at ur work and get the same numbers bc 
#they're using the same algorithm

set.seed(1)
vec1 <- rnorm(100000, mean=0, sd=1)  # 100000 random numbers with mean=0 and sd=1
vec1

# if u don't want density u can get a scattterplot of the dta by just doing plot (vec1)
#but rly u want the densityor distribution so u should od what's below or do a histogram 
#u want density rather than hist bc it's easier to work w density than frequency distribution table

plot(density(vec1),
	   main="Distribution of vec1",
	   xlab="")

vec2 <- rnorm(5000, mean=50, sd=6)   # 5000 random numbers with mean=50 and sd=6
vec3 <- rnorm(5000, mean=35, sd=10)  # 5000 random numbers with mean=35 and sd=10

plot(density(vec2), 
	   main="Distribution of vec2 and vec3",
	   xlab="",
	   col="red",
	   xlim=c(0,100))
lines(density(vec3), lty=2, col="blue")


## 2: dnorm(x=, mean=, sd=) returns the value of the probability density function, 
##    or the height of a density curve, given x.

dnorm(0, mean=0, sd=1)
#given a mean of 0, what is probability of gettin 0 if i have normal dist
#with mean 0 and standard dev of 1
dnorm(-1, mean=0, sd=1)
#finding out different probabilities of values. what is area underneath curve
#for value of zero if these are the parameters for this distribuiton
dnorm(1, mean=0, sd=1)

# How to plot a normal curve using dnorm()
x.range <- seq(-4, 4, by=0.0001)                       # specify the range of the x-axis
plot(x.range, dnorm(x=x.range, mean=0, sd=1), 
     type="l",                                         # choose type=line
     main="Normal Distribution with mean=0 and sd=1",
     ylab="density",
     lwd=2,
     xaxt="n")
axis(1, at=-3:3, labels=-3:3)

#dnorm is probabilties that are being assigned on our y axis
#type l means line not scatterplot
#lwd is thickness of line
#xaxt is telling it whether or not x axis should exist in terms of like tics on it
#remember to use question mark when making these to simplify things

## 3. pnorm(q=, mean=, sd=) returns a probability of drawing q or smaller from 
##    a normal distribution.

pnorm(0, mean=0, sd=1)
pnorm(-1, mean=0, sd=1)
pnorm(1, mean=0, sd=1)
pnorm(-1.96, mean=0, sd=1)

#if ur working w normal dist, looking for 95% confidence level, u have 2.5% on each tails
#to find 2% of data at or lower than this value, z score -1.96 is the # for this dist
#where aprox 2.5% data falls below it, +1.96 2.5% falls above it 


# What's the probability of drawing a value between -1.96 and 1.96?
pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1)



## 4. qnorm(p=, mean=, sd=) returns a value of the *p*th quantile.
##    This is an inverse of pnorm() - the exact opposite!

qnorm(0.5, mean=0, sd=1)
qnorm(0.15, mean=0, sd=1)
qnorm(0.85, mean=0, sd=1)
qnorm(0.025, mean=0, sd=1)

qnorm(c(0.025, 0.975), mean=0, sd=1)




## 5. Subsetting data

## == means "equal to"
## >, <, >=, <=

movies <- read.csv("movies.csv")

# Subset by movie genre, with just drama
dramas <- movies[movies$genre=="Drama",]

table(dramas$genre)

# Subset by runtime, with everything that is more than 2 hours
long.movies <- movies[movies$runtime >= 120,]
table(long.movies$runtime)     # u can see that alla re at 120 or above 
short.movies = movies[movies$runtime <= 120,]

plot(density(movies$runtime, na.rm=T),    #na aka like nulls or missing values-->na.remove=true
	   lty=2, 
	   main="Distrubtion of Runtime",
	   xlab="Rutime (minute)",
	   col="gray50", 
	   ylim=c(0,0.05))

mean(movies$runtime, na.rm=T) # mean is 105 so that looks kinda about right, rmemeber
#since this isn't normally distributed, we can't point to exactly where mean will be

lines(density(long.movies$runtime, na.rm=T))
#when u just leave lines, it adds to existing plot w/o creating another one
#typing plot creates a new function 

legend("topright", 
       legend=c("All Movies", "Long Movies"),
       lty=c(2,1),
       col=c("gray50", "black"),
       bty="n",
       cex=0.8)
#adding in legends, u can use ? to understand what each thing means

## Combine multiple conditions
## & means "and"
## | means "or"

# Subset by genre "and" release year
old_comedies <- movies[movies$genre=="Comedy" & movies$thtr_rel_year < 1980,]

# Get Drama "or" Walt Disney Pictures
drama_or_disney <- movies[movies$genre=="Drama" | movies$studio=="Walt Disney Pictures",]
#use double equal signs for these lol so this is a conditional, not assigning something
#u need to start using <- instead for assigning i guess u simpleton mind 

table(drama_and_disney$genre)








#### Group Work ####

## Make sure you use the arguments for the title, labels, etc.
## to make plots look nice.



## 1. Write the names of all group members.

#harris klien
#mk vereen

read.csv("TrumpApproval.csv")
x<-read.csv("TrumpApproval.csv")
x

#whenever u read a csv in, u have to assign it to an object

## 2. Read in the Trump Job Approval poll data.
##    Variables are as follows:
##    - Approve = Proportion of the respondents who approve Trump
##    - survey_house = Survey company
##    - end_date = Date the survey ended
##    - sample_subpopulation = Sample type
##    - observations = Number of observations
##    - mode = Survey method



## 3. Plot a histogram of the Trump job approval rates. 

hist(x$Approve)

## 4. Suppose you only have the "Gallup" poll from "2/19/2017". If we know that
##    the population variance is 0.25, what is your estimate of the sampling 
##    distribution? 
##    Hint: Find this poll using two conditions (survey_house, end_date)


###first get subset of data between those time periods and from that survey firm
old_comedies <- movies[movies$genre=="Comedy" & movies$thtr_rel_year < 1980,]
gallip_and_date <- x[x$survey_house=="Gallup" & x$end_date=="2/19/2017",]
table("gallip_and_date")
### p: probabiltiy, q: quantile, d:density, r: random # generator. so we want r
### creating a random sample: # observations 1500, mean aka approval (which is an average) is .42, 
### variance .25 --> st dev = .5

#rnorm - u have no idea what kind of sample, u do know it's normal dist, to create sample
#dist, randomly draw 1500 obsv from normal dist which u know mean, variance (spread)
#recreate, generate sampling dist

randompoints<-rnorm(1500, .42, .5)



## 5. According to your answer in Q4, what are the 20th and 75th quantiles of 
##    the distribution?



## 6. Suppose a new poll suggests that the Trump approval rate is 47%. 
##    According to your answer in Q4, what is the probability of a poll showing 
##    support for Trump higher than this?



##################### OPTIONAL #####################

## We would like to know the long-term trends in Trum approval from
## Gallup, SurveyMonkey, and YouGov/Economist.
## Using "Approve" and "end_date", create a line plot that summarizes overtime 
## changes in Trump approval rates by survey company (draw three separate lines 
## for the three companies). Which one is most supportive of Trump?

# First, you have to run the following line (change the name of the data):
polls$end_date <- as.Date(polls$end_date, "%m/%d/%Y")


