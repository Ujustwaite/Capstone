library(tswge)
library(vars)

setwd("C:/Users/cwale/OneDrive/Desktop/Capstone Wind/Carl")
source("buoy_fore_plot.R")


other_data <- read.csv("data/nj_4_20_other.csv")

head(other_data)

train <- read.csv("data/nj_col4_1yr_at_1min_train2.csv")
train <- train[train$file_date == '2016-04-20',]

train$avg_pressure <- other_data$avg_pressure

train$avg_tempr <-other_data$avg_temp
train$avg_humid <- other_data$avg_humidity


horizon <- 15

test <- read.csv("Data/nj_col4_1yr_at_1min_train.csv")
test <- test[test$file_date == '2016-04-21',][1:horizon,]

plotts.wge(train$avg_tempr)
plotts.wge(train$avg_pressure)
plotts.wge(train$avg_humid)

#***********************************************************************
# VAR
#***********************************************************************
wndspd <-train$horizontal_wspd
hdir <- train$horizontal_wdir
temp <- train$avg_tempr
press <- train$avg_pressure
humid <- train$avg_humid


X <- cbind(wndspd, hdir, temp, press)#, humid)
VARselect(X, lag.max = 15, type = "none", season = NULL, exogen = NULL)

lsfit <- VAR(X, p = 12, type = "none")

plotts.wge(lsfit$varresult$wndspd$residuals)

ljung.wge(lsfit$varresult$wndspd$residuals, K = 24)
ljung.wge(lsfit$varresult$wndspd$residuals, K = 48)



preds <- predict(lsfit, n.ahead = horizon)

# ASE
err <- test$horizontal_wspd - preds$fcst$wndspd[1:horizon, 1]
var_ase <- mean(err^2)
var_ase

# PLOT
plotts.wge(c(train[1400:1440,]$horizontal_wspd, test$horizontal_wspd))
plot_fore_obs(train[1400:1440,]$horizontal_wspd, test$horizontal_wspd, preds$fcst$wndspd[1:horizon,1], horizon)


