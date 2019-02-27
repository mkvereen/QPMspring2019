setwd("~/Documents/GitHub/QPMspring2019/problemSets/PS2")
#question 4
#4a
pnorm(57.75, mean=50, sd=6)

#4b

pnorm(50.45, mean=50, sd=6, lower.tail = FALSE)

#4c

x<-pnorm(52.4, mean=50, sd=6)
y<-pnorm(59.4, mean=50, sd=6)
y-x


#Question 5
set.seed(12345)
salaries<-rnorm(n=10000, mean=40000, sd=15000)
plot(density(salaries), 
     main="Distribution of Salaries",
     xlab="salaries", ylab="Probability Density")
pdf(file = "PS2Q5")
dev.off

#Question 6

#A
PlotA<-density(rnorm(100000, mean=0, sd=.04^.5))
PlotB<-density(rnorm(100000, mean=0, sd=3^.5))
PlotC<-density(rnorm(100000, mean=3, sd=3^.5))
PlotD<-density(rnorm(100000, mean=3, sd=.4^.5))
PlotE<-density(rnorm(1000,mean=-2, sd=5^.5))
PlotF<-density(rnorm(1000, mean=-2, sd=.25^.5))

plot(PlotA, main="Question 7",col="red", xlab="Mean", ylab="Probability Density", ylim = c(0,2), xlim= c(-5,10))
lines(PlotB, main="B", col="purple")
lines(PlotC, main="C", col="magenta")
lines(PlotD, main="D", col="black")
lines(PlotE, main="E", col="orange")
lines(PlotF, main="F", col="blue")

pdf("PS2Q6")

#Question 7

setwd("~/Documents/GitHub/QPMspring2019/problemSets/PS2")
Qsev<-read.csv("drugCoverage.csv")

DrugperMo<-Qsev$drugsmedia/Qsev$Year
QsevA<-Qsev$drugsmedia/Qsev$Year
hist(QsevA)
Qsev

#A
hist(Qsev$drugsmedia, breaks= 196, main="Drug Media per Month")
pdf("PS2Q7A")

#B
boxplot(Qsev$drugsmedia)

boxplot(Qsev$approval)

#C
 #number of drug-related stories on the vertical axis, 
 #and place the unemployment rate on the horizontal axis. 

plot(Qsev$unemploy, Qsev$drugsmedia, 
     main = "Q 7C plot 1",
     xlab = "Unemployment",
     ylab = "drug media stories",
     pch = 19)    

plot( Qsev$drugsmedia, Qsev$unemploy, 
     main = "Q 7C plot 2",
     xlab = "drug media stories",
     ylab = "Unemploymen",
     pch = 19)  


#D
plot(Qsev$drugsmedia, Qsev$Year, ylab="drug media stories",xlab="Month-Year",type="l")
plot(Qsev$approval, Qsev$Year, ylab="drug media stories",xlab="Month-Year",type="l")

#Question 8

#A

condataset<-read.csv("wnominatehouse.csv")


Congress88<-congress[1:440]
Congress107<-congress[441:882]


D88 <- condataset$wnominate[condataset$congress == 88 & condataset$party== 100]
R88<- condataset$wnominate[condataset$congress == 88 & condataset$party == 200]
D107<-condataset$wnominate[condataset$congress == 107 & condataset$party == 100]
R107<-condataset$wnominate[condataset$congress == 107 & condataset$party == 200]


#B

median(R88)
median(R107)

#C

median(D88)
median(D107)

#D

sd(D88)
sd(D107)

#E

sd(R88)
sd(R107)

#F
hist(R88, xlim= c(-1,1), col="blue")

hist(D88,xlab= "Wnom", add=TRUE,col = "red", main =  "Q7 Part G")
pdf("PS2Q7F1")


#G
hist(R107, xlim = c(-1,1), col = "blue")
hist(D107,xlab= "Wnom", add=TRUE,col = "red",  main =  "Q7 Part G")
pdf("PS2Q7G1")


