setwd("~/Documents/GitHub/QPMspring2019/problemSets/PS3")

install.packages("faraway")
library("faraway")
data("newhamp")
help("newhamp")

#question 1
#A
plot(newhamp$pObama, newhamp$Dean, main= "Dean vs Obama and hand counted vs machine ballots", cex.main=1, xlab="proportion Obama", ylab="proportion Dean", col=my_cols, xlim=c(0,1), ylim=c(0,1))
my_cols <- ifelse(newhamp$votesys=="H", "Blue", "Red")
legend("topleft", legend = c("Hand counted", "Machine counted"), col = c("Blue", "Red"), lty = 1, cex = 1)
dev.off()


#question 2
#B

x<-seq(from=-5, to=5, by=.1)
plot(x, dnorm(x), lwd=3, type="l", col=1, lty=1, xlab="x", ylab="y", main="Question 2")
lines(x, dt(x, df=20), lwd=3, ylim=c(0, .4), col=2, lty=2)
lines(x, dt(x, df=3), lwd=3, ylim=c(0, .4), col=3, lty=3)
lines(x, dt(x, df=1), lwd=3, ylim=c(0, .4), col=4, lty=4)
legend("topleft", 
       c("normal", "t(df=20)", "t(df=3)", "t(df=1)"), 
       lty=c(1,2,3,4), col=c(1,2,3,4), bty="n")
pdf("Question 2")
dev.off()

#question 3

install.packages("Zelig")
library("Zelig")
data("voteincome")
?voteincome

#se=sd/sqrt(n)
x<-voteincome$age

sd(x)/sqrt(1500)

#se=.4511027

#z score=(x-u)/se
z.score<-(50-mean(x))/.4511027
#z= 1.637469

#D
mean(x)

mean(x) - 1.96*.4511027
mean(x)+ 1.96*.4511027

# [ 48.37717, 50.14549]



