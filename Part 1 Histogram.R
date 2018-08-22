
##Part 1 30 Day mortality rate for heart attacks
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)

outcome[,11] <- as.numeric(outcome[,11])
##Will introduce NAs, will throw back warning
hist(outcome[,11])
