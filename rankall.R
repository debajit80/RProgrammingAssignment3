rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    ## Check that state and outcome are valid
    tbloutcome <- function(outcome) {if(outcome =="heart attack"){"Heart.Attack"} else if(outcome =="heart failure") {"Heart.Failure"} else if(outcome == "pneumonia") {"Pneumonia"} else{"enter 1 of 3 specified outcomes"}}
    x <- tbloutcome(outcome)
    outcomecol <- function(outcome) {as.vector(paste("Hospital.30.Day.Death.","Mortality.","Rates.from",tbloutcome(outcome),sep=".",collapse=NULL))}
    y <- outcomecol(outcome)
        check2 <- as.vector (table((colnames(data)) == y, exclude="FALSE"),mode="numeric")
        if (length(check2)==0) {stop("invalid outcome")} 
              
                ## For each state, find the hospital of the given rank
                z <- if(outcome == "heart attack") {data[,c(2,7,11)]} else if(outcome == "heart failure"){data[,c(2,7,17)]} else if(outcome =="pneumonia"){data[,c(2,7,23)]} else {data[,1]}
                z1 <- subset(z, z[,3]!= "Not Available")
                NumRank <- sapply(z1[,3],as.numeric)
                TrnData <- cbind(z1,NumRank)
                
                HospitalRank <- TrnData[order(TrnData[,2],TrnData[,4],TrnData[,1]),]
                
                ##HospitalRank$Rank <- 1
  
                ##HospitalRank$Top <- "best"
                
                ##for(i in 1:100) {if(HospitalRank[i,2] != HospitalRank[i+1,2]) {HospitalRank$Rank[i+1] <- 1} else {HospitalRank$Rank[i+1] <- HospitalRank$Rank[i]+1 }}
                
                ##for(i in 1:100) {if(HospitalRank[i,2] != HospitalRank[i+1,2]) {HospitalRank$Top[i] <- "worst"} else {}}  
                
                names(HospitalRank)[1] <- "hospital"
                names(HospitalRank)[2] <-  "state"
  
                    
                  check3 <- if (!is.numeric(num)== "TRUE") {} else if (is.numeric(num)== "TRUE" & num > nrow(z)) {"NA"}
                  
                  s <- split(HospitalRank,HospitalRank$state)
                  
                  
                  if(num == "best"){sa <- as.data.frame(t(sapply(s,function(s) s[1,])))}
                  else if (num == "worst"){sa <- as.data.frame(t(sapply(s,function(s) s[nrow(s),])))}
                  else {sa <- as.data.frame(t(sapply(s,function(s) s[num,])))}
  
                  sa[,1:2]
}
  
 