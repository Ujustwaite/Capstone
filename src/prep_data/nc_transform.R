library(ncdf4)
library(lubridate)


#******************************************************************************
# Creates a dataframe from the nc data
#******************************************************************************
# nrow:
nc_to_df <- function(nc_data){
    
    # Extract Data from nc file
    time_offset <- ncvar_get(nc_data, "time_offset")
    h_windspeed <- ncvar_get(nc_data, "horizontal_wspd")
    h_wdir <- ncvar_get(nc_data, "horizontal_wdir")

    df <- data.frame( time_offset
                    , h_windspeed[1,]
                    , h_windspeed[2,]
                    , h_windspeed[3,]
                    , h_windspeed[4,]
                    , h_windspeed[5,]
                    , h_windspeed[6,]
                    , h_wdir[1,]
                    , h_wdir[2,]
                    , h_wdir[3,]
                    , h_wdir[4,]
                    , h_wdir[5,]
                    , h_wdir[6,]
                    )
    
    # Change column names
    names(df) <- c( "time_offset"
                  , "h_wspd_1"
                  , "h_wspd_2"
                  , "h_wspd_3"
                  , "h_wspd_4"
                  , "h_wspd_5"
                  , "h_wspd_6"
                  , "h_wdir_1"
                  , "h_wdir_2"
                  , "h_wdir_3"
                  , "h_wdir_4"
                  , "h_wdir_5"
                  , "h_wdir_6"
                  )
    
    nc_close(nc_data)
    
    return(df)
    
}


#******************************************************************************
# Extracts date from time units
#******************************************************************************
extract_file_date <- function(nc){
    
    x <- nc$dim[1]$time$units
    x <- substr(x, 15, 24)
    x <- as.POSIXlt(x, format = "%Y-%m-%d")
    
    return(x)
}