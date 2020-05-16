library(data.table)

#***************************************************
# USER INPUTS
#***************************************************

place <- "VA"
data_folder_path = "C:/Users/cwale/OneDrive/Desktop/WindApp/data/"
save_file_path = "/"


#***************************************************
# Combine Data
#***************************************************


time <- c(
    "1min",
    "5min",
    "10min",
    "30min",
    "1hour",
    "1day"
)

for(t in time){
    
    wnd <- data.table::fread(paste0(data_folder_path, place, "/", place,"_", t, ".csv"))
    wnd_prim <- data.table::fread(paste0(data_folder_path, place, "/", place, "_primary_", t, ".csv"))
    
    
    comb_dt <- data.table()
    
    main_dates <- unique(wnd[, c(file_date)])

    for(i in main_dates){

        dt_main <- wnd[file_date == i]
 
        dt_prim <- wnd_prim[file_date == i]
  
        dt_prim[, file_date:=NULL]
        
        dt <- dt_main[dt_prim, on = .(time = group)]
    
        comb_dt <- rbindlist(list(comb_dt, dt))
       
    }
    
    file_path = paste0(save_file_path, place, "/", place, "_all_", t, ".csv")
    write.csv(comb_dt, file_path, row.names = FALSE)
    
}
    