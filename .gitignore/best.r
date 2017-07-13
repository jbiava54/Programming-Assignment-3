best <- function(state, outcome) {
        ## read outcome data
        outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data_df <- as.data.frame(cbind(outcome_measures[,2],  ## hospital
                                          outcome_measures[,7],  ## state
                                          outcome_measures[,11], ## heart attack
                                          outcome_measures[,17], ## heart failure 
                                          outcome_measures[,23], ## pneumonia
                                        stringsasFactors = FALSE, na.strings = "Not available"))
        data_df[,3] <- as.numeric(data_df[,3])
        data_df[,4] <- as.numeric(data_df[,4])
        data_df[,5] <- as.numeric(data_df[,5])
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
       else if ((state %in% states) == TRUE) && ((outcome %in% outcomes) == TRUE) {
                        state_subset <- data_df[data_df[,2] == state,]
                        outcome_subset <- state_subset[,outcome]
                        calcmin <- min(outcome_subset, na.rm = TRUE)
                       
                       
                        
        }
        minrow <- which(outcome_subset == calcmin)
        hosp_name <- state_subset[minrow,1]        
        return(hosp_name)
        ## create a function that finds the min of when you input a state and outcome
        
        
        
        
        ## ties : sorted alphabetically 
        
        ## check that state and outcome are valid 
        
        
        ## return hospital name in that state with lowest 30 day death rat      
     
        }
