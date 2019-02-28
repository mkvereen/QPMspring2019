# set seed so that anyone else can replicate my graph
set.seed(23409)
# create a random sample of men and women
m <- sample(20:60, 20)
f <- sample(20:60, 20)

# open up .pdf to save male boxplot
pdf("maleBoxplot.pdf")
boxplot(m, main = "Male Boxplot")
points(mean(m), col = "blue", pch = "+", cex = 2)
text(labels = "mean", x = 1.3, y = mean(m), col = "blue")
text(labels = c(".25 quantile", ".75 quantile"), 
     x = 1.35, y = quantile(m, c(.25, .75)))
#  25%   75% 
# 32.75 45.75 
dev.off()

# open up .pdf to save
pdf("femaleBoxplot.pdf")
boxplot(f, main = "Female Boxplot")
points(mean(f), col = "red", pch = "+", cex = 2)
text(labels = "mean", x = 1.3, y = mean(f), col = "red")
text(labels = c(".25 quantile", ".75 quantile"), 
     x = 1.35, y = quantile(f, c(.25, .75)))
#  25%   75% 
# 31.75 50.50 
dev.off()
