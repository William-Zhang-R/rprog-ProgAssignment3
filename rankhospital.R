rankhospital <- function(state, outcome, num = "best") { 
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 na.strings = "Not Available", 
                                 stringsAsFactors = FALSE)
        ## Check that state are valid
        
        judge_state <- outcome_data$State[outcome_data$State == state]
                ## extracting the line by the way of using logical form.
        if(length(judge_state) == 0){
                stop("invalid state")
        }
        ## Check that outcome are valid
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
        
        ## check number and covert it to num
        if (num == "best") {
                num = 1
        }
        
        
        ## extracting the state data
        s <- split(outcome_data, outcome_data$State == state)
        s <- s[["TRUE"]]
        
        ## remove the NA rows, otherwise there will be an error in next step use
        ## logic to judge which hosptial is the min in value
        good <- complete.cases(s[, name])
        s <- s[good, ]
        
        ## 生成子表格，使之以死亡率为对象排序
        rate_list <- split(s, s[name])
        rate_list[[1]]$Hospital.Name
        ## 如果是最差的，则取列的最后一个元素
        if (num == "worst") {
                num = length(rate_list)
        }
        
        ## 如果请求排名大于总排名，返回NA，否则返回对应排名的医院名称
        if (num > length(rate_list)) {
                print(NA)
        }
        ## Return hospital name in that state with the given rank 
        ## 30-day death rate
        else {
                rate_list[[num]]$Hospital.Name
                }
        
}
