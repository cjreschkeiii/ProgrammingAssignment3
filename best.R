getwd()
setwd("C:/Users/creschke/datasciencecoursera")
getwd()
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
best <- function(state, outcome) {
    
    ## Read the outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!state %in% unique(data[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    ## Return hospital name in that state with lowest 30-day death 
    ## rate
    df = data[data$State == state, c(2, col)]
    df[which.min(df[, 2]), 1]
}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
