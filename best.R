best <- function(state,outcome){
  
    ##read outcome data 
    
    data <<- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
      ##check state and outcome are valid                   
      
      unqstate <- as.vector (unique(data$State)) ##create a vector of unique states
             
        tbloutcome <- function(outcome) {if(outcome =="heart attack"){"Heart.Attack"} else if(outcome =="heart failure") {"Heart.Failure"} else if(outcome == "pneumonia") {"Pneumonia"} else{"enter 1 of 3 specified outcomes"}}
    
            x <- tbloutcome(outcome)
      
            outcomecol <- function(outcome) {as.vector(paste("Hospital.30.Day.Death.","Mortality.","Rates.from",tbloutcome(outcome),sep=".",collapse=NULL))}
         
                y <- outcomecol(outcome)
                y1 <- as.name(y)
            

                check1 <- as.vector ((table(unqstate == state, exclude="FALSE")),mode="numeric")
                  check2 <- as.vector (table((colnames(data)) == y, exclude="FALSE"),mode="numeric")
                            
                      if (length(check1)==0) {stop("invalid state")}
                      if (length(check2)==0) {stop("invalid outcome")} 
   
                        subdata1 <<- data[data$State==state,]
                          z <<- if(outcome == "heart attack") {subdata1[,c(2,11)]} else if(outcome == "heart failure"){subdata1[,c(2,17)]} else if(outcome =="pneumonia"){subdata1[,c(2,23)]} else {subdata1[,1]}
      
                              z1 <<- subset(z, z[,2]!= "Not Available")
                              z1[,2] <<- as.numeric((z1[,2]))    
                              z1$rank <<-  rank(z1[,2])                             
    
                                                      
                                HospitalRank <<- z1[order(z1[,3],z1[,1]),]   
           
                        
                                RankUno <- HospitalRank[1,1]
                                RankUno
    
}
