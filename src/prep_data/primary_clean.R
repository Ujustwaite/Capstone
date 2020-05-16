library(tswge)
library(stringr)


#***************************************************
# USER INPUTS
#***************************************************

# Select Data Set to Use - VA or NJ
place <- "VA"

# File Paths
support_func_path <- 'C:/Users/cwale/OneDrive/Desktop/WindApp/support'
primary_data_path <- "C:/Users/cwale/OneDrive/Desktop/WindApp/data/Met_Data/"
save_path <- "C:/Users/cwale/OneDrive/Desktop/WindApp/data/"


selectInterval <- list()

# Increase Time
selectInterval[[1]] <- c(10, "1min", "expand")
selectInterval[[2]] <- c(2, "5min", "expand")
selectInterval[[3]] <- c(1, "10min", "expand")

# Condense Time
selectInterval[[4]] <- c(48, "30min", "condense")
selectInterval[[5]] <- c(24, "1hour", "condense")
selectInterval[[6]] <- c(144, "1day", "condense")




#***************************************************
# Data Transform
#***************************************************

#Support Functions
source(paste0(support_func_path, "/nc_transform.R"))
source(paste0(support_func_path, "/ts_transform_dt.R"))


fpath <- paste0(primary_data_path,"/primary_1_", place, ".csv")
df <- read.csv(fpath,header = F, sep = ",", stringsAsFactors = F)

# Extract columns from data
df2 <- df[,c(1, 10,11,12,13)]

# Rename
colnames(df2) <- c("date_time", "avg_pressure", "avg_temp", "avg_humidity", "avg_dew_point")

# Remove unwanted characters
df2$new_date_time <- gsub("[{}]",replacement = "",df2$date_time )

# Clean and split date and time
df2$date <- substr(df2$new_date_time,start = 1,stop = 10)
df2$time <- substr(df2$new_date_time,start = 11, stop = 22)

# Drop Original Date Time
df2 <- df2[, !(names(df2) == 'date_time')]
df2 <- df2[, !(names(df2) == 'new_date_time')]

# Extract Minute
df2$min_group <- substr(df2$time, start = 5, stop = 6)
df2$min_group <- as.integer(df2$min_group)

# Extract Hour
df2$hour_group <- substr(df2$time, start = 2, stop = 3)
df2$hour_group <- as.integer(df2$hour_group)

# Group Data
df2$group <- ((df2$hour_group * 60) + df2$min_group)/10


# CORRECT FOR DECIMALS IN GROUP
df2$group <- as.integer(df2$group)

df2[df2$group == 0, ]$group <- 1


keep_cols <- c("group",
               "avg_pressure",
               "avg_temp",
               "avg_humidity",
               "avg_dew_point")

impute_cols <-  c("avg_pressure",
                  "avg_temp",
                  "avg_humidity",
                  "avg_dew_point")




for(sel_int in selectInterval){

    interval_selected <- as.integer(sel_int[1])

    comb_dt <- data.table()
    
    for(i in unique(df2$date)){
        
        dt <- as.data.table(df2)
        
        # Filter to date
        dt <- dt[date == i,]
        
        # Select different columns
        dt <- dt[,..keep_cols]
        
        # Group by group column
        dt <- dt[, lapply(.SD, mean), by = group]
        
        # Add Missing Groups
        dt <- add_missing_groups(dt, "group", c(1:144))
        
        # Impute Missing
        for(col in impute_cols){
            dt <- ts_lin_interp(dt,
                                col,
                                impute_criteria = TRUE,
                                criteria_col = "missing",
                                replace_col = T)
        }
        
        dt[,missing:=NULL]
        
        # Change time interval
        # Increase factor 10: 1 minute
        # Increase factor 2: 5 minutes
        
        if(sel_int[3] == "expand"){
            dt <- ts_increase_interval(dt, "group", interval_selected , impute_cols)    
        } else {
            dt <- ts_condense_interval(dt, "group",  interval_selected ,  impute_cols)    
        }
        
        # Add date columns
        dt[,file_date:=i]
        
        # Combine data
        comb_dt <- rbindlist(list(comb_dt, dt))
    }


    file_path <- paste0(save_path, tolower(place),"/", tolower(place),"_primary_", sel_int[2],".csv")
    write.csv(comb_dt, file_path, row.names = FALSE)

}
