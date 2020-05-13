library(tswge)
library(vars)
library(nnfor)

setwd("C:/Users/cwale/OneDrive/Desktop/Capstone Wind/Carl")
source("buoy_fore_plot.R")


other_data <- read.csv("data/nj_4_20_other.csv")

head(other_data)

train <- read.csv("data/nj_col4_1yr_at_1min_train2.csv")
train <- train[train$file_date == '2016-04-20',]

train$avg_pressure <- other_data$avg_pressure

train$avg_tempr <-other_data$avg_temp
train$avg_humid <- other_data$avg_humidity


train$hwind<- log(train$horizontal_wdir^2)


horizon <- 15

test <- read.csv("Data/nj_col4_1yr_at_1min_train.csv")
test <- test[test$file_date == '2016-04-21',][1:horizon,]

plotts.wge(train$horizontal_wdir)


# Convert to Time Series
windspeed <- ts(train$horizontal_wspd)
pressure <- ts(train$avg_pressure)
temp <- ts(train$avg_tempr)
humid <- ts(train$avg_humid)
direction <- ts(train$hwind)

xreg_df <- data.frame(temp = train$avg_tempr, humid = train$avg_humid, pressure = train$avg_pressure)

# Forecast XREGS
fit_press <- mlp(pressure, comb = "mean")#, hd.auto.type = "cv")
fore_press <- forecast(fit_press, h = horizon)

fit_temp <- mlp(temp, comb = "mean")#,hd.auto.type = "cv")
fore_temp <- forecast(fit_temp, h = horizon)

fit_humid <- mlp(humid, comb = "mean")
fore_humid <- forecast(fit_humid, h = horizon)

fit_direct <- mlp(direction, comb = "mean")
fore_direction <- forecast(fit_direct, h = horizon)
plot(fore_direction)

xreg_fore_df <- data.frame(temp = c(train$avg_tempr, fore_temp$mean),
                      humid = c(train$avg_humid,fore_humid$mean),
                      pressure = c(train$avg_pressure, fore_press$mean),
                      direction = c(train$hwind, fore_direction$mean))


# FORECAST WINDSPEED
xreg_df <- data.frame(temp = train$avg_tempr, humid = train$avg_humid, 
                      pressure = train$avg_pressure, direction = train$hwind)

fit_mlp <- mlp(windspeed, comb = "mean", xreg = xreg_df)#, hd.auto.type = "cv")
plot(fit_mlp)

fore_mlp <- forecast(fit_mlp, h = horizon, xreg = xreg_fore_df )
plot(fore_mlp)
err <- test$horizontal_wspd- fore_mlp$mean
nn_ase <- mean(err^2)
nn_ase


plotts.wge(c(train[1400:1440,]$horizontal_wspd, test$horizontal_wspd))
plot_fore_obs(train[1400:1440,]$horizontal_wspd, test$horizontal_wspd, fore_mlp$mean, horizon)
