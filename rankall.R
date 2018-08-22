rankall <- function(outcome, num = "best"){
            data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  
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
            
            #Convert to Numeric
            data[,i] <- as.numeric(data[,i])
            
            #Pull out Hospital Name, State, and Ranking, remove NA
            part <- data[!is.na(data[,i]),c(2,i,7)]
            
            #Sort by State, then Ranking, then Name to avoid ties
            part <- part[order(part[,2],part[,1]),]
            
            if (num == "best") {
              num <- 1
            }
            uni_state <- aggregate(part, list(part[,3]), FUN=function(p) {
              n <- if (num == "worst") 
              { length(p) } 
              else 
              { num }
              p[n]
            })
            
            ## Return a data frame with the hospital names and the
            ## (abbreviated) state name
            ex <- uni_state[,c(2,1)]
            names(ex) <- c("hospital", "state")
            rownames(ex) <- ex[,2]
            ex
            
  
  
}
rankall("heart attack","best")
rankall("heart attack",5)

