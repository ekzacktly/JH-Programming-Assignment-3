#Create Function
rankall <- function(outcome,num="best")
{
    #Read in data
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    states <- unique(outcome_data$State)
    
    #Check that outcome is valid

    if (!outcome %in% c('heart attack','heart failure', 'pneumonia'))
    {
        stop("invalid outcome")
    }
    
    #Filter the data for each outcome type for invalid data
    ha_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    ha_data <- ha_data[!ha_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available",]
    
    hf_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    hf_data <- hf_data[!hf_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == "Not Available",]
    
    pn_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    pn_data <- pn_data[!pn_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == "Not Available",]
    

    
    #Setup hospitals dataframe to accept data
    hospitals <- data.frame(hospital=character(), state=character())
    
    
    #loop to figure out Xth best hospital fo x outcome by state
    i <- 1
    while (i <= length(states))
    {
        current_state <- states[i]
        ha_data_temp <- ha_data[ha_data$State==current_state,]
        hf_data_temp <- hf_data[hf_data$State==current_state,]
        pn_data_temp <- pn_data[pn_data$State==current_state,]
        
        #Deal with best/worst
        count_ha <- nrow(ha_data_temp)
        count_hf <- nrow(hf_data_temp)
        count_pn <- nrow(pn_data_temp)
        
        if(!is.numeric(num))
        {pick_row <- switch(
            num,
            "best"=1,
            "worst"=if(outcome=='heart attack'){count_ha} else if(outcome=='heart failure'){count_hf} else{count_pn}
        )
        }
        else {pick_row <- num}
        
        current_hospital <- switch(
            outcome,
            "heart attack"=ha_data_temp[order(as.numeric(ha_data_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ha_data_temp$Hospital.Name),]$Hospital.Name[pick_row],
            "heart failure"=hf_data_temp[order(as.numeric(hf_data_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),hf_data_temp$Hospital.Name),]$Hospital.Name[pick_row],
            "pneumonia"=pn_data_temp[order(as.numeric(pn_data_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),pn_data_temp$Hospital.Name),]$Hospital.Name[pick_row]
        )
        hospitals[i,] <- c(current_hospital,current_state)
        i <- i + 1
    }
    
    #Return Best Hospital
    return(hospitals)
}