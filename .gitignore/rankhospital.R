rankhospital <- function(state, outcome, num = "best") {
        ## read outcome data
        outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data_df <- as.data.frame(cbind(outcome_measures[,2],  ## hospital name
                                       outcome_measures[,7],  ## state
                                       outcome_measures[,11], ## heart attack
                                       outcome_measures[,17], ## heart failure 
                                       outcome_measures[,23], ## pneumonia
                                       stringsasFactors = FALSE, na.strings = "Not Available"))
        data_df[,3] <- as.numeric(data_df[,3], na.rm = TRUE)
        data_df[,4] <- as.numeric(data_df[,4], na.rm = TRUE)
        data_df[,5] <- as.numeric(data_df[,5], na.rm = TRUE)
        colnames(data_df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        ## begin process of input validity 
        ## state
        states <- as.character(data_df$state)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if((state %in% states) == FALSE) {
                stop(print("Invalid State"))
                
        }
        else if((outcome %in% outcomes) == FALSE) {
                stop(print("Invalid Outcome"))
        }
        
        else 
                
                state_subset <- data_df[data_df[,2] == state,]
                outcome_subset <- state_subset[, outcome]
                reorder <- sort(outcome_subset)
                if (is.numeric(num) == TRUE) {
                        if (length(outcome_subset) < num){
                                return(NA)
                        }
                        else if(is.character(num) == TRUE) {
                                if (num == "best") {
                                        num = 1
                                }
                                else if (num == "worst") {
                                        num = length(outcome_subset)
                                }
                                else { num }        }
                match <- which(outcome_subset == reorder[num])
                hosp_name <- as.character(state_subset[match, 1])
             
        
        }
        
                return(hosp_name)
        }
