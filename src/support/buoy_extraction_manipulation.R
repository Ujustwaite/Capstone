library(ncdf4)
library(lubridate)


#******************************************************************************
# Creates a dataframe from the nc data
#******************************************************************************
# nrow:
nc_to_df <- function(nc_data, nrow, file_date = "1/1/1900"){
    
    # Extract Data from nc file
    time_offset <- ncvar_get(nc_data, "time_offset")
    h_windspeed <- ncvar_get(nc_data, "horizontal_wspd")
    h_wdir <- ncvar_get(nc_data, "horizontal_wdir")
    air_pressure <- ncvar_get(nc_data, "air_pressure")
    rel_humid <- ncvar_get(nc_data, "relative_humidity")
    
    df <- data.frame( time_offset
                      , h_windspeed[nrow,]
                      , h_wdir[nrow,]
                      , air_pressure
                      , rel_humid
    )
    
    # Change column names
    names(df) <- c( "time_offset"
                    , "horizontal_wspd"
                    , "horizontal_wdir"
                    , "air_pressure"
                    , "relative_humidity"
    )
    
    # Add date from file to df
    df$file_date <- file_date
    
    return(df)
    
}


#******************************************************************************
# Averages the data in to a given time measurement, ex. 1min, 5mins, 10mins...
#******************************************************************************
# Args:
#   time_col: Column that has the time you are basing your observations on
#   time_span: Total observation time for a given set of data
#   time_interval: What you want to convert your time to. Units are based of original data.

time_measurment_change <- function(df, time_col, time_span, time_interval, keep_cols){
    
    # start_time <- Sys.time()
    # start_time2 <- Sys.time()

    # total_new_intervals: How many intervals you will get based off of your orignal time units/new time interval. 
    total_new_intervals = time_span/time_interval
    
    start_seq = 0
    end_seq = time_interval
    
    time_vector <- df[,time_col]
    group_vector <- rep(0, total_new_intervals)

    for(i in 1:total_new_intervals){
        
        time_range <- time_vector >= start_seq & time_vector < end_seq
        group_vector[time_range] <- i

        start_seq = start_seq + time_interval
        end_seq = end_seq + time_interval

    }
    
    df$group <- group_vector
    
    # print("Loop Time:")
    # print(Sys.time() - start_time2)

    # Group data by group column
    df2 <- df[ , append(keep_cols, "group")]
    df2 <- aggregate(df2, by=list(df2$group), FUN=mean, na.rm = T)

    # Impute missing values
    base_intervals = seq(1, total_new_intervals)
    missing_intervals <- base_intervals[-df2$group]
    
    if(length(missing_intervals) > 0){
        miss_df <- data.frame(group = missing_intervals)
        miss_df$Group.1 <- NA
        
        for(j in keep_cols){
            miss_df[,j] <- mean(df[,j])
        }
        
        final_df <- rbind(df2, miss_df[, c("Group.1", keep_cols, "group")])
        
    } else{
        final_df <- df2
    }

    final_df <- final_df[order(final_df$group),]
    rownames(final_df) <- 1:nrow(final_df)

    # print("Function Time:")
    # print(Sys.time() - start_time)
    
    return(final_df)

}


#******************************************************************************
# Extracts date from time units
#******************************************************************************
extract_file_date <- function(nc){
    
    x <-nc$dim[1]$time$units
    x <- substr(x, 15, 24)
    x <- as.POSIXlt(x, format = "%Y-%m-%d")
    
    return(x)
}


#******************************************************************************
# Writes data to working directory in a text file
#******************************************************************************
write_to_txt <- function(data, file_name){
    sink(paste0(file_name, ".txt"))
    print(data)
    sink()
}


#******************************************************************************
# Writes data to working directory in a text file
#******************************************************************************
# return[[1]]: All days combined into 1 data frame
# return[[2]]: Each day seperated into list of days

# fname -> dir_path
# flist -> file_list

combine_files <- function(flist, fname, time_col, time_span, time_interval, nrow, keep_cols){

    file_date <- ""
    
    combined_df <- data.frame()
    combined_list  <- list()
    
    incr = 0
    
    for(i in flist){
        
        incr = incr + 1
        
        nc <- nc_open(paste0(fname, "/", i))
        
        df <- nc_to_df(nc, nrow)
        
        if(time_interval > 0){
            new_df <- time_measurment_change(df, time_col, time_span, time_interval, keep_cols)
        } else {
            new_df <- df
        }
        
        file_date <- extract_file_date(nc)
        
        combined_list[[as.character(file_date)]] <- new_df
        
        new_df$file_date <- file_date
        combined_df <- rbind(combined_df, new_df)
        
        nc_close(nc)
        
        print(paste0("File Index: ", i, "   complete: ", round(incr/length(flist), 2) * 100, "%"))
        
    }

    return(list(combined_df, combined_list))
}




#******************************************************************************
# Calculate ASE Score Cross Validation
#******************************************************************************

wind_ase_score <- function(flist, s, d, horizon, p_max = 5, q_max = 2){
    
    df <- data.frame()
    
    for(i in 1:length(flist)){
        
        x_name <- names(flist[i])
        x <- flist[[i]]$horizontal_wspd
        
        est_data <- x
       
        if(d > 0){
            for(i in 1:d){
                est_data <- artrans.wge(est_data, phi.tr = 1)
            }
        }
        
        # Get p and q
        aic_type <- c('aic', 'bic')
        pq_list <- list()
        
        incr <- 1
        
        for(i in aic_type){
            aic_x <- aic5.wge(est_data, p = 0:p_max, q = 0:q_max, type = as.character(i))
            
            for(j in 1:5){
                pq_list[[incr]] <- c(aic_x[[1]][j], aic_x[[2]][j])
                incr <- incr + 1
            }
        }
        
        pq_list <- unique(pq_list)

        # Estimate   
        for(i in 1:length(pq_list)){
            
            # Train
            est_x <- est.arma.wge(est_data, p = pq_list[[i]][1], q = pq_list[[i]][2])
            
            # Test
            ase_sum <- 0
            
            for(j in flist){
                x_fore <- fore.aruma.wge(j[,2], phi = est_x$phi, theta = est_x$theta, d = d, limits = F, 
                                         n.ahead = horizon, lastn = T, plot = F)
                
                length(x_fore$f)
                length(j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])])
                err <- x_fore$f  - j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])]
                ase_sum <- ase_sum + mean(err^2)
            }
            
            ase_sum <- ase_sum/length(flist)
            
            new_df <- data.frame(x_name, pq_list[[i]][1], pq_list[[i]][2], s, d, horizon, ase_sum)
  
            df <- rbind(df, new_df)
        }
    }
    
    names(df) <- c("train_data", "p", "q", "s", "d", "horizon", "Mean ASE")
    
    return(df)   
}





estimate_test <- function(flist, p, q, s = 0, d = 0, horizon){
    
    df <- data.frame()
    
    for(i in 1:length(flist)){
      
        x_name <- names(flist[i])
        x <- flist[[i]]$horizontal_wspd
        
        est_data <- x
       
        if(d > 0){
            for(i in 1:d){
                est_data <- artrans.wge(est_data, phi.tr = 1)
            }
        }
        
        # Train
        est_x <- est.arma.wge(est_data, p = p, q = q)
        
        # Test
        ase_sum <- 0
        
        for(j in flist){
            x_fore <- fore.aruma.wge(j[,2], phi = est_x$phi, theta = est_x$theta, d = d, limits = F, 
                                     n.ahead = horizon, lastn = T, plot = F)
            
            length(x_fore$f)
            length(j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])])
            err <- x_fore$f  - j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])]
            ase_sum <- ase_sum + mean(err^2)
        }
        
        ase_mean <- ase_sum/length(flist)
        
        new_df <- data.frame(x_name, s, d, horizon, ase_mean)
        
        df <- rbind(df, new_df)
    }     
    
    names(df) <- c("train_data", "s", "d", "horizon", "Mean ASE")
    return(df)  
}


#******************************************************************************
# Splits a Data frame into multiple training sets. Returns a list
#******************************************************************************
split_to_train <- function(x, train_size, spacer = 1){
    
    final_df <- data.frame()
    new_list <- list()
    
    for(i in seq(1, (dim(x)[1] - train_size + 1), by = spacer)){
        
        end_pos <- (i + train_size - 1)
        
        if( end_pos > dim(x)[1]){
            end_pos <- dim(x)[1]
        }
        
        new_list[[ as.character(paste0(i,":",end_pos))]] <- x[(i:end_pos),]
 
    }
    return(new_list)
}
