
global_start_time <- Sys.time()

library(tswge)
library(ncdf4)
library(data.table)
library(lubridate)

#***************************************************
# User Input
#***************************************************

place <- "nj"

support_function_path <- "C:/Users/cwale/OneDrive/Desktop/WindApp/support"
buoy_data_folder <- "C:/Users/cwale/OneDrive/Desktop/Capstone Wind/Carl/data/buoy_"
save_file_path <- "/"

# Time Intervals:
# ----------------
# 1440: 1 min intervals
# 288: 5 min intervals
# 144: 10 min intervals
# 24: 1 hour intervals
selectInterval <- list()
selectInterval[[1]]<- c(1440, "1min")
selectInterval[[2]]<- c(288, "5min")
selectInterval[[3]]<- c(144, "10min")
selectInterval[[4]]<- c(48, "30min")
selectInterval[[5]]<- c(24, "1hour")
selectInterval[[6]]<- c(1, "1day")


#***************************************************
# Combine Data
#***************************************************

source(paste0(support_function_path, "/nc_transform.R"))
source(paste0(support_function_path, "/ts_transform_dt.R"))

# Get list of files in folder
fpath <- paste0(buoy_data_folder, place)
flist <- list.files(path = fpath, pattern = "^.*\\.(nc|NC|Nc|Nc)$")

impute_cols <- c("h_wspd_1",
                 "h_wspd_2",
                 "h_wspd_3",
                 "h_wspd_4",
                 "h_wspd_5",
                 "h_wspd_6",
                 "h_wdir_1",
                 "h_wdir_2",
                 "h_wdir_3",
                 "h_wdir_4",
                 "h_wdir_5",
                 "h_wdir_6")



for(sel_int in selectInterval){
    
    local_start_time <- Sys.time()
    
    print(sel_int)
    
    comb_dt <- data.table()
    
    for(i in flist){
    
        #1: Open NC File
        nc <- nc_open(paste0(fpath, "/", i))
        
        # 2: Extract to Data Frame
        df <- nc_to_df(nc)
        
        dt <- as.data.table(df)
        
        # 3: Round time data
        dt[, time := round(time_offset)]
        
        dt[time == 0, time:= 1]
        dt[time > 86400, time:= 86400]
        
        dt[, time_offset := NULL]
        
        dt <- dt[, lapply(.SD, mean), by = time]
    
        dt <- add_missing_groups(dt, 'time', c(1:86400))
        
        # 6: Impute missing data
        for(col in impute_cols){
            dt <- ts_lin_interp(dt,
                                col,
                                impute_criteria = TRUE,
                                criteria_col = "missing",
                                replace_col = T)
        }
        
        dt[,missing:=NULL]
        
        # 7: Convert intervals
        dt <- ts_condense_interval(dt, "time",  as.integer(sel_int[1]),  impute_cols)
        
        fdate <-extract_file_date(nc)
        
        
        # 8: Add file date
        dt[, file_date:= date(fdate)]
        
        # 9: Combine data
        comb_dt <- rbindlist(list(comb_dt, dt))
        
        dt <- NULL
    }
    
    
    
    for(col in impute_cols){
        
        col_imp <- paste0(col, "impute")
        comb_dt[ , eval(col_imp):= FALSE]
        comb_dt[is.na(col), eval(col_imp):= TRUE]
        
        comb_dt <- ts_lin_interp(comb_dt,
                                 col,
                                 impute_criteria = TRUE,
                                 criteria_col = col_imp,
                                 replace_col = T)
        
        comb_dt[,eval(col_imp):= NULL]
    }
        
    
    file_path <- paste0(save_file_path, place,"_", sel_int[2],".csv")
    write.csv(comb_dt, file_path, row.names = FALSE)
    
    print(Sys.time() - local_start_time)

}

print("Global Time:")
print(Sys.time() - global_start_time)

