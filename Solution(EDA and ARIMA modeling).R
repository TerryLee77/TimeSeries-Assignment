# data input and packages loading
data <- read.csv("Beijing.csv")
library(astsa)
library(tseries)
library(forecast)
library(prophet)
library(car)
library(vip)
library(stats)
library(ggplot2)

## definitions of functions

### interpret the wind direction into coordinates
trans_wind <- function(x){
  y <- switch(x,CV = t(c(0,0)),
              NE = t(c(1,1)),
              NW = t(c(-1,1)),
              SE = t(c(1,-1)),
              SW = t(c(-1,-1)));
  return(y);
}


# Pre-Processing
data_new <- data[c("PM2.5","PM10","SO2","NO2","O3","CO","TEMP","PRES","RAIN","HUMI","DEWP","WD","WS")]
data_new <- na.omit(data_new)
for(i in 1:dim(data_new)[1]){
  #### interpret the wind direction into coordinates pay attention to \sqrt(2)
  data_new[i,c("WD","WS")] <- trans_wind(data_new[i,"WD"]) * data_new[i,"WS"] / sqrt(2)
}
names(data_new)[12] <- "EWS"
names(data_new)[13] <- "NSS"
data_res <- data_new[-c(dim(data_new)[1]-10:dim(data_new)),]
data_new <- data_new[c(dim(data_new)[1]-10:dim(data_new)),]

#### Standardization
mean_varia <- rep(0,dim(data_new)[2])
sd_varia <- rep(0,dim(data_new)[2])
for(i in 1:dim(data_new)[2]){
  mean_varia[i] < - mean(as.numeric(data_new[,i]))
  sd_varia[i] <- sd(data_new[,i])
}

for(i in 1:dim(data_new)[1]){
  data_new[i,] <- (as.numeric(data_new[i,]) - mean_varia)/sd_varia
}

data_new[,"EWS"] <- as.numeric(data_new[,"EWS"])


# Exploration Data Analysis

## NO2
plot.ts(data_new[,"NO2"])
hist(data_new[,"NO2"])
acf2(data_new[,"NO2"])
acf2(diff(data_new[,"NO2"],lag = 2,differences = 2))

par(mfrow=c(8,1))
plot.ts(data_new[,"NO2"])
plot.ts(data_new[,"TEMP"])
plot.ts(data_new[,"PRES"])
plot.ts(data_new[,"RAIN"])
plot.ts(data_new[,"HUMI"])
plot.ts(data_new[,"DEWP"])
plot.ts(data_new[,"EWS"])
plot.ts(data_new[,"NSS"])


par(mfrow=c(1,1))




## relations between NO2 and predictors
plot.ts(data_new[,"DEWP"])
plot.ts(data_new[,"NO2"])
plot.ts(data_new[,"TEMP"]) #### Waiting for inserting



## Test for Stationarity
adf.test(data_new[,"NO2"])
adf.test(diff(data_new[,"NO2"]))

## ACF and PACF plots
acf2(data_new[,"NO2"])
acf2(diff(data_new[,"NO2"],lag = 22))
acf2(data_new[,"TEMP"]) #### waiting for inserting


### Test for White Noise
Box.test(data_new[,"NO2"],lag = log(length(data_new[,"NO2"])),type = "Box-Pierce")
Box.test(data_new[,"NO2"],lag = log(length(data_new[,"NO2"])),type = "Ljung-Box")

## Multicollinearity 
vif(lm(NO2~TEMP+PRES+RAIN+HUMI+DEWP+EWS+NSS,data = data_new))
vif(lm(NO2~PRES+RAIN+HUMI+DEWP+EWS+NSS,data = data_new))
vif(lm(NO2~PRES+RAIN+DEWP+EWS+NSS,data = data_new))
vif(lm(NO2~TEMP+PRES+RAIN+HUMI+EWS+NSS,data = data_new))

## Linear Regression
fit_1 <- lm(NO2~TEMP+PRES+RAIN+HUMI+DEWP+EWS+NSS,data = data_new)
summary(fit_1)
fit_2 <- lm(NO2~TEMP+PRES+RAIN+HUMI+EWS+NSS,data = data_new)
summary(fit_2)
vip(fit_2) #### variable importance

# AR/MA/ARMA/ARIMA Modeling & Prediction of 10 points

## Without Covariates

## Modeling
### Auto
NO2_arima <- auto.arima(data_new[,"NO2"],max.p = 10,max.q = 10,ic = "aic")

summary(NO2_arima)

NO2_arima_res <- NO2_arima[["residuals"]]

Box.test(NO2_arima_res)

### Manual with pre-approximation
arima_select_without <- data.frame(p = rep(c(1,2,3,4),times=4),
                           i = rep(1),
                           q = rep(c(1,2,3,4),each=4),
                           AIC = rep(0,4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  NO2_arima <- arima(data_new[,"NO2"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- NO2_arima[["aic"]]
} ### find the best ARIMA model through specific sets

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])

## Prediction
### Back to Auto.arima
NO2_arima_without <- auto.arima(data_new[,"NO2"],max.p = 10,max.q = 10,ic = "aic") ### Best ARIMA model for NO2
pre_without_1 <- predict(NO2_arima_without,n.ahead = 10)
pre_point <- pre_without_1[["pred"]] * sd_varia[4] + mean_varia[4] ### point estimator of 10 points
pre_sd_NO2_upper <- (pre_without_1[["pred"]]+1.96*pre_without_1[["se"]]) * sd_varia[4] + mean_varia[4]
pre_sd_NO2_lower <- (pre_without_1[["pred"]]-1.96*pre_without_1[["se"]]) * sd_varia[4] + mean_varia[4]
pl <- cbind(pre_point,pre_sd_NO2_upper,
            pre_sd_NO2_lower,c(1:10))
plot(x = c(1:10), y = pl[,1], type = "l",ylim = c(0,250))
lines(x = c(1:10), y = pl[,2], type = "l", col = "blue")
lines(x = c(1:10), y = pl[,3], type = "l", col = "blue")
legend("topleft",pch = c(15,15,15),col = c(2,3,3),legend=c("point","upper 95%","lower 95%"))


### Manual Prediction
NO2_arima_without <- arima(data_new[,"NO2"],order = c(2,1,4)) ### Best ARIMA model for NO2
pre_without_2 <- predict(NO2_arima_without,n.ahead = 10)
pre_point <- pre_without_2[["pred"]] * sd_varia[4] + mean_varia[4] ### point estimator of 10 points
pre_sd_NO2_upper <- (pre_without_2[["pred"]]+1.96*pre_without_2[["se"]]) * sd_varia[4] + mean_varia[4]
pre_sd_NO2_lower <- (pre_without_2[["pred"]]-1.96*pre_without_2[["se"]]) * sd_varia[4] + mean_varia[4]
pl <- cbind(pre_point,pre_sd_NO2_upper,
            pre_sd_NO2_lower,c(1:10))
plot(x = c(1:10), y = pl[,1], type = "l",ylim = c(0,250))
lines(x = c(1:10), y = pl[,2], type = "l", col = "blue")
lines(x = c(1:10), y = pl[,3], type = "l", col = "blue")
legend("topleft",pch = c(15,15,15),col = c(2,3,3),legend=c("point","upper 95%","lower 95%"))

## With covariates
NO2_arima <- auto.arima(data_new[,"NO2"],max.p = 10,max.q = 10,max.order = 10,
                        xreg = data.matrix(data_new[,c("TEMP","PRES","RAIN","HUMI","EWS","NSS")]))

summary(NO2_arima)

NO2_arima_res <- NO2_arima[["residuals"]]

Box.test(NO2_arima_res)

arima_select <- data.frame(p = rep(c(1,2,3,4),times=4),
                           i = rep(1),
                           q = rep(c(1,2,3,4),each=4),
                           AIC = rep(0,4*4))

for(j in 1:dim(arima_select)[1]){
  p <- arima_select[j,"p"]
  i <- arima_select[j,"i"]
  q <- arima_select[j,"q"]
  NO2_arima <- arima(data_new[,"NO2"],order = c(p,i,q),
                     xreg = data.matrix(data_new[,c("TEMP","PRES","RAIN","HUMI","EWS","NSS")]))
  arima_select[j,"AIC"] <- NO2_arima[["aic"]]
}#### find the best ARIMA model for NO2 with the selected covariates

print(arima_select[which.min(arima_select[,"AIC"]),])
NO2_arima <- arima(data_new[,"NO2"],order = c(3,1,4),
                   xreg = data.matrix(data_new[,c("TEMP","PRES","RAIN","HUMI","EWS","NSS")]))

summary(NO2_arima)

NO2_arima_res <- NO2_arima[["residuals"]]

Box.test(NO2_arima_res)

## Prediction
### Back to auto.arima
NO2_arima <- auto.arima(data_new[,"NO2"],max.p = 10,max.q = 10,max.order = 10,
                        xreg = data.matrix(data_new[,c("TEMP","PRES","RAIN","HUMI","EWS","NSS")]))
pre_1 <- predict(NO2_arima,n.ahead = 10,newxreg = data.matrix(data_res[,c("TEMP","PRES","RAIN","HUMI","EWS","NSS")]))
pre_arima <- pre_1[["pred"]]
pre_point_NO2 <- pre_arima * sd_varia[4] + mean_varia[4]
pre_sd_NO2_upper <- (pre_1[["pred"]]+1.96*pre_1[["se"]]) * sd_varia[4] + mean_varia[4]
pre_sd_NO2_lower <- (pre_1[["pred"]]-1.96*pre_1[["se"]]) * sd_varia[4] + mean_varia[4]
pl <- cbind(pre_point_NO2,pre_sd_NO2_upper,
            pre_sd_NO2_lower,c(1:10))
plot(x = c(1:10), y = pl[,1], type = "l")
lines(x = c(1:10), y = pl[,2], type = "l", col = "blue")
lines(x = c(1:10), y = pl[,3], type = "l", col = "blue")
legend("topleft",pch = c(15,15,15),col = c(2,3,3),legend=c("point","upper 95%","lower 95%"))
### new data of xreg, method is the same as fitting the best ARIMA model for NO2
#### TEMP
TEMP_arima <- auto.arima(data_new[,"TEMP"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  TEMP_arima <- arima(data_new[,"TEMP"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- TEMP_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_TEMP <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
TEMP_arima <- arima(data_new[,"TEMP"],order = order_TEMP)
pre_TEMP_without <- predict(TEMP_arima,n.ahead = 10)
pre_point_TEMP <- pre_TEMP_without[["pred"]] * sd_varia[7] + mean_varia[7]

#### PRES
PRES_arima <- auto.arima(data_new[,"PRES"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  PRES_arima <- arima(data_new[,"PRES"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- PRES_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_PRES <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
PRES_arima <- arima(data_new[,"PRES"],order = order_PRES)
pre_PRES_without <- predict(PRES_arima,n.ahead = 10)
pre_point_PRES <- pre_PRES_without[["pred"]] * sd_varia[7] + mean_varia[7]

#### RAIN
RAIN_arima <- auto.arima(data_new[,"RAIN"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  RAIN_arima <- arima(data_new[,"RAIN"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- RAIN_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_RAIN <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
RAIN_arima <- arima(data_new[,"RAIN"],order = order_RAIN)
pre_RAIN_without <- predict(RAIN_arima,n.ahead = 10)
pre_point_RAIN <- pre_RAIN_without[["pred"]] * sd_varia[7] + mean_varia[7]

#### HUMI
HUMI_arima <- auto.arima(data_new[,"HUMI"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  HUMI_arima <- arima(data_new[,"HUMI"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- HUMI_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_HUMI <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
HUMI_arima <- arima(data_new[,"HUMI"],order = order_HUMI)
pre_HUMI_without <- predict(HUMI_arima,n.ahead = 10)
pre_point_HUMI <- pre_HUMI_without[["pred"]] * sd_varia[7] + mean_varia[7]

#### EWS
EWS_arima <- auto.arima(data_new[,"EWS"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  EWS_arima <- arima(data_new[,"EWS"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- EWS_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_EWS <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
EWS_arima <- arima(data_new[,"EWS"],order = order_EWS)
pre_EWS_without <- predict(EWS_arima,n.ahead = 10)
pre_point_EWS <- pre_EWS_without[["pred"]] * sd_varia[7] + mean_varia[7]

#### NSS
NSS_arima <- auto.arima(data_new[,"NSS"],max.p = 10,max.q = 10)

arima_select_without <- data.frame(p = rep(c(3,4,5,6),times=8),
                                   i = rep(c(0,1),each=4,times=4),
                                   q = rep(c(1,2,3,4),each=8),
                                   AIC = rep(0,2*4*4))

for(j in 1:dim(arima_select_without)[1]){
  p <- arima_select_without[j,"p"]
  i <- arima_select_without[j,"i"]
  q <- arima_select_without[j,"q"]
  NSS_arima <- arima(data_new[,"NSS"],order = c(p,i,q))
  arima_select_without[j,"AIC"] <- NSS_arima[["aic"]]
}

print(arima_select_without[which.min(arima_select_without[,"AIC"]),])
order_NSS <- as.numeric(arima_select_without[which.min(arima_select_without[,"AIC"]),c("p","i","q")])
NSS_arima <- arima(data_new[,"NSS"],order = order_NSS)
pre_NSS_without <- predict(NSS_arima,n.ahead = 10)
pre_point_NSS <- pre_NSS_without[["pred"]] * sd_varia[7] + mean_varia[7]

newxreg <- cbind(pre_point_TEMP,pre_point_PRES,pre_point_RAIN,
                 pre_point_HUMI,pre_point_EWS,pre_point_NSS) #### combine all the predictions of 10 points of covariates 

predict(NO2_arima,n.ahead = 10,newxreg = newxreg) #### predict NO2 for 10 points with new data of covariates
pre_arima_2 <- predict(NO2_arima,n.ahead = 10,newxreg = newxreg)
pre_point_NO2 <- pre_arima_2[["pred"]]
pre_sd_NO2_upper <- (pre_arima_2[["pred"]]+1.96*pre_arima_2[["se"]])
pre_sd_NO2_lower <- (pre_arima_2[["pred"]]-1.96*pre_arima_2[["se"]])
pl <- cbind(pre_point_NO2,pre_sd_NO2_upper,
            pre_sd_NO2_lower,c(1:10))
plot(x = c(1:10), y = pl[,1], type = "l")
lines(x = c(1:10), y = pl[,2], type = "l", col = "blue")
lines(x = c(1:10), y = pl[,3], type = "l", col = "blue")
legend("topleft",pch = c(15,15,15),col = c(2,3,3),legend=c("point","upper 95%","lower 95%"))



