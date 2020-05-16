library(dplyr)

# add_missing_groups: add missing groups to data frame
# ts_condense_interval: condense time interval
# ts_lin_interp: Uses linear Interpolation based of of a criteria column

#***********************************************************************
# Add missing groups to data frame
#***********************************************************************
add_missing_groups <- function(df, group_col, all_groups){
    
    # Mark groups that are not missing
    df$missing <- FALSE
    
    # Getting missing group vector
    exist_groups <- unique(df[,group_col])
    # missing_groups <- all_groups[-exist_groups]
    missing_groups <- all_groups[!(all_groups %in% exist_groups)]
    
    if(length(missing_groups) > 0){
        
        # Missing groups dataframe
        missing_df <- data.frame(matrix(ncol = dim(df)[2], nrow = length(missing_groups)))
        colnames(missing_df) <- colnames(df)
        missing_df[,group_col] <- missing_groups
        missing_df$missing <- TRUE
        
        # Combine original data with missing groups
        new_df <- rbind(df, missing_df)
        
        # Sort Data
        new_df <- new_df[order(new_df[,group_col]),]
        
        rownames(new_df) <- 1:nrow(new_df)
        
    } else {
        new_df <- df
    }
    
    return(new_df)
}


#***********************************************************************
# Uses linear Interpolation based of of a criteria column
#***********************************************************************
# ts_missing_linter
# Start and end val are used otherwise the start or end value are used
ts_lin_interp <- function(df,
                          impute_col,
                          impute_criteria,
                          criteria_col,
                          replace_col = T,
                          start_val = NULL,
                          end_val = NULL) {

    
    # Impute Vector
    impute_data <- df[,impute_col]
    
    # Critera vector
    criteria_data <- df[,criteria_col]

    n <- nrow(df)
    
    counter <- 1
    
    while(counter <= nrow(df)){
        
        while(criteria_data[counter] != impute_criteria & counter <= n){
            counter <- counter + 1
        }
        
        start_index <- counter 
        
        while(criteria_data[counter] == impute_criteria & counter <= n){
            counter <- counter + 1
        }
        
        end_index <- counter 
        
        # Fill Impute Sequence
        missing_count <- end_index - start_index
        
        if( missing_count > 0) {
            
            if(start_index == 1){
                
                start_data <- end_val
                
                if(is.null(start_val)){
                    start_data <- impute_data[end_index]
                }
                
                diff <- (impute_data[end_index] - start_data)/(missing_count + 1)
                
            } else if(end_index > nrow(df)){
                
                end_data <- end_val
                
                if(is.null(end_val)){
                    end_data = impute_data[start_index - 1]
                }
                
                start_data <- impute_data[start_index - 1] 
                diff <- (end_data  - start_data)/(missing_count + 1)
                
            } else {
                start_data <- impute_data[start_index - 1]
                diff <- (impute_data[end_index] - start_data)/(missing_count + 1)
            }
            
            for(i in 1:missing_count){
                new_value <- start_data + diff * i 
                impute_data[start_index + i - 1] <- new_value
            }
        }
    }  
    
    if(replace_col == T){
        df[,impute_col] <- impute_data
    } else{
        df[,paste0(impute_col, "_imp")] <- impute_data
    }
    
    return(df)
    
}

#***********************************************************************
# Condense time interval
#***********************************************************************
ts_condense_interval <- function(df, group_col, num_groups, keep_cols){
    
    n <- nrow(df)
    
    group_breaks <- n/num_groups
    
    start_point <- 0
    end_point <- group_breaks
    
    groups <- rep(0, n)
    
    for(i in 1:num_groups){
        groups[c(start_point:end_point)] <- i
        start_point <- end_point + 1
        end_point <- end_point + group_breaks
    }
    
    df$group <- groups
    
    df2 <- df[ , append(keep_cols, "group")]
    df2 <- aggregate(df2, by=list(df2$group), FUN=mean, na.rm = T)
    df2 <- df2[,!(names(df2) %in% c("Group.1"))]
    
    return(df2)
}




#***********************************************************************
# Uses linear interpolation to increase time interval
#***********************************************************************
# time_col needs to be continous
# increase_fact is how many times the time is going to increas (ex. 10 to 5 minutes is 2)
ts_increase_interval <- function(df, 
                                 time_col, 
                                 increase_fact,
                                 impute_cols){
    
    # Get inverse of increase factor for starting point and incrementing
    incr_inverse <- 1/increase_fact
    
    # Create sequence
    incr_vect <- seq(incr_inverse, nrow(df), incr_inverse)
    
    # Round for floating point errors
    incr_vect <- round(incr_vect, 1)
    
    # Add in misisng sequences
    new_df <- add_missing_groups(df, time_col, incr_vect)
    new_df[,time_col] <- 1:nrow(new_df)
    
    # Impute data of added
    for(col in impute_cols){
        new_df <- ts_lin_interp(new_df,
                                col,
                                impute_criteria = TRUE,
                                criteria_col = "missing",
                                replace_col = T)
    }
    
    return(new_df)
}

