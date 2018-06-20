

library(plyr)
library(dplyr)
library(data.table)
library(testthat)

rankHospital <- function (state , outcome , ranking){
  
  
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
    
    if ( ranking == "best"){
      
      ranking = 1
      ranking <- as.numeric(ranking)
      
    } else if ( ranking == "worst"){
      
      ranking = nrow(FilteredList)
      ranking <- as.numeric(ranking)
      
    } else {
      
      ranking <- as.numeric(ranking)
      ranking <- as.numeric(ranking)
    }
    
    
    if (as.numeric(ranking) > nrow(FilteredList)){
      
      return(NA)
      
    } else {
    
    HospitalName <- FilteredList$Hospital.Name[[ranking]]
    
    return(HospitalName)
    
    }
    
  }
  
  
}