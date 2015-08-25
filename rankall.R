getwd()
setwd("C:/Users/creschke/datasciencecoursera")
getwd()
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states = unique(data[, 7])
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    ## For each state, find the hospital of the given rank
    data[, col] = as.numeric(data[, col])
    data = data[, c(2, 7, col)]
    data = na.omit(data)
    rank_in_state <- function(state) {
        df = data[data[, 2] == state, ]
        nhospital = nrow(df)
        switch(num, best = {
            num = 1
        }, worst = {
            num = nhospital
        })
        if (num > nhospital) {
            result = NA
        }
        o = order(df[, 3], df[, 1])
        result = df[o, ][num, 1]
        c(result, state)
    }
    output = do.call(rbind, lapply(states, rank_in_state))
    output = output[order(output[, 2]), ]
    rownames(output) = output[, 2]
    colnames(output) = c("hospital", "state")
    data.frame(output)
}
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
