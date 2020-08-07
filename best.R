## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state


best <-function(state, outcome) {
        ## Read outcome data by the name of outcome
        ## use the option na.strings to "tell" reads.csv that with this data 
        ## set there is some other text string (not the default NA) [Not Available]
        ## that means missing data, otherwise numeric columns will get read in as
        ## character columns.
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 na.strings = "Not Available", 
                                 stringsAsFactors = FALSE)
        ## here can't use colClasses = "character" because it will make 
        ## Then numeric data (outcomes) will be sorted numerically, 
        ## not lexicographically (alphabetically, where for example 10.1 is < 8).
        
        ## Check that state and outcome are valid
        if (outcome == "heart attack") {
                name = 11
        }
        else if (outcome == "heart failure") {
                name = 17
        }
        else if(outcome == "pneumonia"){
                name = 23
        }
        else {
                ## use stop to print error message when input wrong outcome
                stop("invalid outcome")
        }
        
        ## use stop to print error message when input wrong state
        judge_state <- outcome_data$State[outcome_data$State == state]
        ## extracting the line by the way of using logical form.
        if(length(judge_state) == 0){
                stop("invalid state")
        }
        
        
        ## extracting the state data
        s <- split(outcome_data, outcome_data$State == state)
        s <- s[["TRUE"]]
        
        ## remove the NA rows, otherwise there will be an error in next step use
        ## logic to judge which hosptial is the min in value
        good <- complete.cases(s[, name])
        s <- s[good, ]
        
        
        judge_hospital <- s[, name] == min(s[[name]])
        hospital <- s[judge_hospital, 2]
        
        ## Return hospital name in that state with lowest 30-day death 
        hospital
        ## rate
        
}