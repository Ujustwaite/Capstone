library(ncdf4)
library(lubridate)


#******************************************************************************
# Creates a dataframe from the nc data
#******************************************************************************
# nrow:
nc_to_df <- function(nc_data, nrow, file_date){
    
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
time_measurment_change <- function(df, time_col, time_span, time_interval, keep_cols){
    
    df$group <- 0

    start_seq = 1
    end_seq = time_interval + 5
    loop_interval = time_span/time_interval
    
    for(i in 1:loop_interval){
        
        # Goes through index of Dataframe
        for(j in start_seq:end_seq) {
            
            minute_dif <- df[j, time_col] - (time_interval * i)
          
            minute_dif_next = 0
            
            if (j < end_seq){
                minute_dif_next <- df[j + 1, time_col] - (time_interval * i)  
            }
            
            if( (minute_dif < 1 & minute_dif > -1) | (minute_dif < -1 & minute_dif_next >= 1) ){
                df[c(seq(start_seq, j)), ]$group <- i
                start_seq = j + 1
                end_seq = start_seq + time_interval + 5
                break
            } 
        }
    }  
    
    x <- df[ , append(keep_cols, "group")]
    x <- aggregate(x, by=list(x$group), FUN=mean, na.rm = T)
    
    return(x)
    
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
combine_files <- function(flist, fname, time_col, time_span, time_interval, keep_cols){
 
    combined_df <- data.frame()
    
    incr = 0
 
    for(i in flist){
        incr = incr + 1
        
        nc <- nc_open(paste0(fname, "/", i))
        
        file_date <- extract_file_date(nc)
        
        df <- nc_to_df(nc, 4, file_date)
        
        new_df <- time_measurment_change(df, time_col, time_span, time_interval, keep_cols)
        
        combined_df <- rbind(combined_df, new_df)
        
        nc_close(nc)
        
        print(paste("File Index: ", i, " % complete: ", incr/length(flist)))
        
    }
    
    return(combined_df)
    
}



