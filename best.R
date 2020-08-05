## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state


best <-function(state, outcome) {
        ## Read outcome data by the name of outcome
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if (outcome = "heart attack") {
                name = 12
        }
        else if (outcome = "“heart failure”") {
                name = 18
        }
        else{
                name = 24  
        }
        ## extract the hospital without NA values
        
        good <- complete.cases(outcome_data[, name])
        no_missing <- outcome_data[good, ]
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death 
        ## rate
        
}