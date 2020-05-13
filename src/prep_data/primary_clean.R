
library(stringr)

setwd("C:/Users/cwale/OneDrive/Desktop/Capstone Wind/Carl")

source("ts_imputations.R")

df <- read.csv("data/primary_1_NJ.csv",header = F, sep = ",", stringsAsFactors = F)

train <- df[,c(1, 9,10,11,12)]

colnames(train) <- c("date_time", "avg_pressure", "avg_temp", "avg_humidity", "avg_dew_point")

train$new_date_time <- gsub("[{}]",replacement = "",train$date_time )

# Clean Date Time
train$date <- substr(train$new_date_time,start = 1,stop = 10)
train$time <- substr(train$new_date_time,start = 11, stop = 22)

# Drop Original Date Time
train <- train[, !(names(train) == 'date_time')]
train <- train[, !(names(train) == 'new_date_time')]

# Extract Minute
train$min_group <- substr(train$time, start = 5, stop = 6)
train$min_group <- as.integer(train$min_group)

# Extract Hour
train$hour_group <- substr(train$time, start = 2, stop = 3)
train$hour_group <- as.integer(train$hour_group)

# Group Data
train$group <- ((train$hour_group * 60) + train$min_group + 10)/10

# CORRECT FOR DECIMALS IN GROUP
train$group <- as.integer(train$group)


keep_cols <- c("avg_pressure", "avg_temp", "avg_humidity", "avg_dew_point")

base_group <- seq(1, 144)

dates <- unique(train$date)
final_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(final_df) <- c("group", keep_cols)

df_date_means <- data.frame(matrix(ncol = 5, nrow = length(dates)))

colnames(df_date_means) <- c("file_date", keep_cols)

counter <- 1

for(i in dates){

    tmp_df <- train[train$date == i, c("group", keep_cols)]
    tmp_df <- aggregate(tmp_df, by=list(tmp_df$group), FUN=mean, na.rm = T)
    tmp_df <- tmp_df[, !(names(tmp_df) == 'Group.1')]
    tmp_df$impute <- FALSE
    
    df_date_means[counter, "file_date"] <- i
    
    for(j in keep_cols){
        df_date_means[counter , j] = mean(tmp_df[, j])
    }
    
    # Get Missing Groups
    missing_groups <- base_group[!(base_group %in% tmp_df$group)]
    missing_df <- data.frame(matrix(ncol = 5, nrow = length(missing_groups)))
    colnames(missing_df) <- c("group", keep_cols)
    
    if(length(missing_groups) > 0){
        missing_df$group <- missing_groups 
        missing_df$impute <- TRUE
    }
    
    tmp_df2 <- rbind(tmp_df, missing_df)
    tmp_df2$global_seq <- tmp_df2$group * counter
    tmp_df2$file_date <- i
    tmp_df2 <- tmp_df2[order(tmp_df2$group),]
    
    final_df <- rbind(final_df, tmp_df2)
    
    counter <- counter + 1
    
}


file_date <- final_df[1,]$file_date
file_date <- final_df[nrow(final_df),]$file_date




# Average Pressure Impute
start_mean = df_date_means[df_date_means$file_date == file_date, "avg_pressure"]
end_mean = df_date_means[df_date_means$file_date == file_date , "avg_pressure"]

new_df <- ts_missing_linter(final_df,"avg_pressure",impute_criteria = TRUE, criteria_col = "impute", 
                          start_val = start_mean, end_val = end_mean, indicator = T, indicator_name = "ind_press")

# Average Temperature Impute
start_mean = df_date_means[df_date_means$file_date == file_date, "avg_temp"]
end_mean = df_date_means[df_date_means$file_date == file_date , "avg_temp"]

new_df <- ts_missing_linter(new_df,"avg_temp",impute_criteria = TRUE, criteria_col = "impute", 
                          start_val = start_mean, end_val = end_mean, indicator = T, indicator_name = "ind_temp")


# Average Humidity Impute
start_mean = df_date_means[df_date_means$file_date == file_date, "avg_humidity"]
end_mean = df_date_means[df_date_means$file_date == file_date , "avg_humidity"]

new_df <- ts_missing_linter(new_df,"avg_humidity", impute_criteria = TRUE, criteria_col = "impute", 
                          start_val = start_mean, end_val = end_mean, indicator = T, indicator_name = "ind_humid")


# Average Dew Point Impute
start_mean = df_date_means[df_date_means$file_date == file_date, "avg_dew_point"]
end_mean = df_date_means[df_date_means$file_date == file_date , "avg_dew_point"]

new_df <-ts_missing_linter(new_df,"avg_dew_point", impute_criteria = TRUE, criteria_col = "impute", 
                          start_val = start_mean, end_val = end_mean, indicator = T, indicator_name = "ind_dew")




# Final Cleanup - Average Pressure Impute
na_df<- new_df[is.na(new_df$avg_pressure),]
new_df[new_df$global_seq == na_df$global_seq, ]$impute <- TRUE


start_mean = df_date_means[df_date_means$file_date == file_date, "avg_pressure"]
end_mean = df_date_means[df_date_means$file_date == file_date , "avg_pressure"]

new_df <- ts_missing_linter(new_df,"avg_pressure",impute_criteria = TRUE, criteria_col = "impute", 
                            start_val = start_mean, end_val = end_mean, indicator = T, indicator_name = "ind_press")





file_path = "C:/Users/cwale/OneDrive/Desktop/nj_primary_cleaned.csv"
write.csv(new_df, file_path, row.names = FALSE)










