## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state


best <-function(state, outcome) {
        ## Read outcome data by the name of outcome
        outcome_data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## Check that state and outcome are valid
        if (outcome == "heart attack") {
                name = 12
        }
        else if (outcome == "heart failure") {
                name = 18
        }
        else if(outcome == "pneumonia"){
                name = 24  
        }
        else {
                ## use stop to print error message when input wrong outcome
                stop("invalid outcome")
        }
        
        ## use stop to print error message when input wrong state
        judge_state <- outcome_data$State[outcome_data$State == state]
        if(length(judge_state) == 0){
                stop("invalid state")
        }
        
        
        
        ## Return hospital name in that state with lowest 30-day death 
        ## rate
        
}