ordrankhospital <- function(state, outcome, num = "best") {
      source("best.R")
      best(state,outcome)
      check3 <- if (!is.numeric(num)== "TRUE") {} else if (is.numeric(num)== "TRUE" & num > nrow(z)) {"NA"}
      
      for(i in seq(nrow(HospitalRank))){HospitalRank$OrdRank[i] <- i}
      
      RnkdHsptl <<- 
        
        if(num == "best"){subset(HospitalRank, HospitalRank$OrdRank==1,select=c("Hospital.Name"))}
        else if (num == "worst"){subset(HospitalRank,HospitalRank$OrdRank==as.numeric(nrow(HospitalRank)),select=c("Hospital.Name"))}
          else {subset(HospitalRank, HospitalRank$OrdRank == num, select=c("Hospital.Name"))}
            
      RnkdHsptl
      RnkdHsptl[1,1]
}