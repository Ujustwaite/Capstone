library(tswge)

setwd("C:/Users/ujust/Documents/Github/WindPrediction/")

source("Support/buoy_fore_plot.R")

train <- read.csv("data/nj_col4_1yr_at_1min.csv")
train <- train[train$file_date == '2016-04-20',]

plotts.wge(train$horizontal_wspd)
plotts.sample.wge(train$horizontal_wspd)

horizon <- 20
test <- read.csv("data/nj_col4_1yr_at_1min.csv")
test <- test[test$file_date == '2016-04-21',][1:horizon,]

# DIFFERENCE THE DATA
train_dif <- artrans.wge(train$horizontal_wspd, phi.tr = 1)


# MODEL ESTMATES
# aic5.wge(train_dif, p = 0:10, q = 0:5, type = 'aic')
# aic5.wge(train_dif, p = 0:10, q = 0:5, type = 'bic')


train_est <- est.arma.wge(train_dif, p = 5, q = 2)

plotts.wge(train_est$res)


ljung.wge(train_est$res, K = 24)
ljung.wge(train_est$res, K = 48)


# FORECAST
train_fore <- fore.aruma.wge(train$horizontal_wspd, phi = train_est$phi, theta = train_est$theta, 
               d = 1, n.ahead = horizon, limits = F, lastn = F)


err <- test$horizontal_wspd - train_fore$f 
ase <- mean(err^2)

ase
# plot_fore_obs <- function(obs_data, test_data, fore_data, horizon){

plot_fore_obs(train[1400:1440,]$horizontal_wspd, test$horizontal_wspd, train_fore$f, horizon)

