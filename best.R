
library(plyr)
library(dplyr)
library(data.table)
library(testthat)

best <- function (state , outcome){
  

  OutcomeOfCare <- read.csv("outcome-of-care-measures.csv")
  

  ListOfStates <- levels(OutcomeOfCare$State)
  
  ListOfStates <- as.factor(ListOfStates)
  

  OutcomeList <- c("heart attack" , "heart failure" , "pneumonia")
  
  
  if(!state %in% ListOfStates)  
  {
    skip("invalid state")   
    
  }
  
  else if ( !outcome %in% OutcomeList )
  {
    
    skip("Invalid Outcome")
    
  }
    
  else
          {
            
            
            if( outcome=="heart attack")  {
              
              ColNumber <- 11
              
            }  else if ( outcome=="heart failure")  {
                
              ColNumber <- 17
              
            } else {  
                
              ColNumber <- 23 
              
              }
            
            
            FilteredList <- OutcomeOfCare %>% subset(State==state) %>% select(c(2 , ColNumber))
              
            colnames(FilteredList)[2] <- "TargetRate"
            
            
            FilteredList$TargetRate <- as.numeric(as.character(FilteredList$TargetRate))
              
            FilteredList$Hospital.Name <- as.character(FilteredList$Hospital.Name)
            
            FilteredList <- FilteredList[which(complete.cases(FilteredList)) , ]
            
            FilteredList <- FilteredList[order(FilteredList$TargetRate , FilteredList$Hospital.Name) , ]
            
            HospitalName <- FilteredList$Hospital.Name[[1]]
            
            return(HospitalName)

          }
                
    
}