setwd("~/Documents/GitHub/QPMspring2019/problemSets/PS4")

#Q1
#A
conttable <- matrix(c(14,6,7,7,7,1),ncol = 3, nrow = 2, byrow = TRUE)
rownames(conttable) <- c("upper", "lower")
colnames(conttable) <- c("notstop", "bribereq", "stop_warn")
chisq.test(conttable, correct = FALSE)
#B
