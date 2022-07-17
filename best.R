#Programming Assignment 3

#Read outcome data
#outcome <- read.csv(file = "outcome-of-care-measures.csv")
#head(outcome)

#Make histogram of data
#outcome[,11] <- as.numeric(outcome[,11])
#hist(outcome[,11])

#Write a function
best <- function(state,outcome)
{
    #Read Outcome Data
    outcome_data <- read.csv(file = "outcome-of-care-measures.csv")
    
    #Check that State and Outcome are valid
    states <- unique(outcome_data$State)  #load state data to get list of state abbreviations
    if (!state %in% states)
    {
        stop("invalid state")
    }
    if (!outcome %in% c('heart attack','heart failure', 'pneumonia'))
    {
        stop("invalid outcome")
    }
    
    #Filter the data for each outcome type for invalid data
    outcome_data_filtered <- outcome_data[outcome_data$State == state,]
    ha_data <- outcome_data_filtered[!is.na(outcome_data_filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    ha_data <- ha_data[!ha_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available",]
    
    hf_data <- outcome_data_filtered[!is.na(outcome_data_filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    hf_data <- hf_data[!hf_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available",]
    
    pn_data <- outcome_data_filtered[!is.na(outcome_data_filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    pn_data <- pn_data[!pn_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available",]
    
    #Figure out the best hospital based on outcome
    best_hospital <- switch(
        outcome,
        "heart attack"=ha_data[order(as.numeric(ha_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ha_data$Hospital.Name),]$Hospital.Name[1],
        "heart failure"=hf_data[order(as.numeric(hf_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),hf_data$Hospital.Name),]$Hospital.Name[1],
        "pneumonia"=pn_data[order(as.numeric(pn_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),pn_data$Hospital.Name),]$Hospital.Name[1]
    )
    
    #Return Best Hospital
    return(best_hospital)
    
}