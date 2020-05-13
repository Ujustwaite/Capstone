library(tswge)

setwd("C:/Users/ujust/Documents/Github/WindPrediction/")

train <- read.csv("data/nj_col4_1yr_at_1min_train.csv")
plotts.wge(train$horizontal_wdir)

train <- train[train$file_date == '2016-04-20',]


horizon <- 20

test <- read.csv("nj_col4_1yr_at_1min_train.csv")
test <- test[test$file_date == '2016-04-21',][1:horizon,]


# ccf(train$air_pressure, train$horizontal_wspd)
ccf(train$horizontal_wdir, train$horizontal_wspd)
# ccf(train$relative_humidity , train$horizontal_wspd)

plotts.wge(train$horizontal_wdir)

t <- 1:1440

# MULTIPLE REGRESSION ANALYSIS

train$air2 <- dplyr::lag(train$air_pressure, 1)

ksfit <- lm(horizontal_wspd ~ t + horizontal_wdir, data = train)

plotts.wge(ksfit$residuals)


phi = aic.wge(ksfit$residuals, p = 0:8, q = 0:0)
fit = arima(train$, order = c(phi$p, 0, 0), xreg = cbind(t, sun2))
fit

preds = predict(fit, newxreg = data.frame(t = t[30:35], sunspot = sun2[30:35]))

plot(seq(1,35,1),melanoma, type = "b")
points(seq(33,35,1),preds$pred,type = "b", pch = 15)



