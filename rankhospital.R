#Create Function
rankhospital <- function(state,outcome,num)
{
    #Read in data
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    
    #Check that the state and outcome are valid
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
    hf_data <- hf_data[!hf_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == "Not Available",]
    
    pn_data <- outcome_data_filtered[!is.na(outcome_data_filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    pn_data <- pn_data[!pn_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia== "Not Available",]
    
    #Deal with best/worst
    count_ha <- nrow(ha_data)
    count_hf <- nrow(hf_data)
    count_pn <- nrow(pn_data)
   
    if(!is.numeric(num)){pick_row <- switch(
        num,
        "best"=1,
        "worst"=if(outcome=='heart attack'){count_ha} else if(outcome=='heart failure'){count_hf} else{count_pn}
        )
    }
    else {pick_row <- num}
    
    #Figure out the best hospital based on outcome
    chosen_hospital <- switch(
        outcome,
        "heart attack"=ha_data[order(as.numeric(ha_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ha_data$Hospital.Name),]$Hospital.Name[pick_row],
        "heart failure"=hf_data[order(as.numeric(hf_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),hf_data$Hospital.Name),]$Hospital.Name[pick_row],
        "pneumonia"=pn_data[order(as.numeric(pn_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),pn_data$Hospital.Name),]$Hospital.Name[pick_row]
    )
    
    #Return Best Hospital
    return(chosen_hospital)
}