rankhospital <- function(state, outcome, num = "best"){
                
                #read in data below
                data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
                
               
                
                
                if(!any(state == data$State)) {
                  stop('invalid state')
                }
                
                if(outcome == 'heart attack') {
                  i <- 11
                }
                else if(outcome == 'heart failure') {
                  i <- 17
                }
                else if(outcome == 'pneumonia') {
                  i <- 23
                }
                else {
                  stop('invalid outcome')
                }
                
                #create df with State and Outcome,set as numeric
                data.state <- data[data$State == state, ]
                data.state[,i] <- as.numeric(data.state[,i])
                
                #Remove NAs, create df with hospital name (col 2) and outcome's  mortality rate
                part2 <- data.state[!is.na(data.state[,i]),c(2,i)]
                #Rank all hospitals, by mortality rate then by name
                part2 <- part2[order(part2[,2], part2[,1]),]
                
                if (num == "best") {
                  num = 1
                } else if (num == "worst") {
                  num = length(part2[,1])
                }
                part2[num,1]
  
}

rankhospital("AL","heart attack","best")
rankhospital("TX", "heart failure", 4) 
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
