# Andrea Bruckner
# Predict 413: Homework 3
# Forecasting: Principles and Practice, Chapters 7 and 8 exercises

### All of my responses to questions are preceded by three pound symbols (###). Code comments are preceded by only one #, and new questions have two #s.

library(forecast)
library(fma)
library(lmtest)
library(fpp)

##########################
# Chapter 7.8 Exercises
##########################

## 1 ##

# Data set books contains the daily sales of paperback and hardcover books at the same store.
# The task is to forecast the next four days’ sales for paperback and hardcover books (data set books).

# a.	Plot the series and discuss the main features of the data.
par(mfrow=c(1,1))
plot(books, main = "Daily Book Sales")
### The data has an upward trend and a cyclic pattern.

# b.	Use simple exponential smoothing with the ses function (setting initial="simple")
# and explore different values of α for the paperback series. Record the within-sample SSE
# for the one-step forecasts. Plot SSE against α and find which value of α works best.
# What is the effect of α on the forecasts?
fit1 <- ses(books[,"Paperback"], alpha=0.2, initial="simple", h=4)
fit2 <- ses(books[,"Paperback"], alpha=0.6, initial="simple", h=4)
fit3 <- ses(books[,"Paperback"], alpha=0.8, initial="simple", h=4)
plot(fit1, plot.conf=FALSE, ylab="Daily Paperback Book Sales",
     xlab="Time", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.8)),pch=1)

fit1
fit2
fit3

SSEfit1 <- sum((books[,"Paperback"] - fit1$fitted)^2)
SSEfit2 <- sum((books[,"Paperback"] - fit2$fitted)^2)
SSEfit3 <- sum((books[,"Paperback"] - fit3$fitted)^2)

SSEfit1 # 36329.34
SSEfit2 # 44742.62
SSEfit3 # 53456.14

### A lower α produces a smaller forecasted value and a smaller SSE.

# c. Now let ses select the optimal value of α. Use this value to generate forecasts for the next four days. Compare your results with 2.
fit4 <- ses(books[,"Paperback"], initial="simple", h=4) # I think this shows the optimal value of α since I didn't specify alpha
fit4
SSEfit4 <- sum((books[,"Paperback"] - fit4$fitted)^2)
SSEfit4 # 36313.98

### The SSE value is lower when I let ses select the optimal value of α.

# d. Repeat but with initial="optimal". How much difference does an optimal initial level make?
fit5 <- ses(books[,"Paperback"], initial="optimal", h=4) # I think this shows the optimal value of α since I didn't specify alpha
fit5
SSEfit5 <- sum((books[,"Paperback"] - fit5$fitted)^2)
SSEfit5 # 33944.82

### An optimal initial level lowered the SSE value significantly--by about 3000!

# e. Repeat steps (b)–(d) with the hardcover series.
fit1 <- ses(books[,"Hardcover"], alpha=0.2, initial="simple", h=4)
fit2 <- ses(books[,"Hardcover"], alpha=0.6, initial="simple", h=4)
fit3 <- ses(books[,"Hardcover"], alpha=0.8, initial="simple", h=4)
plot(fit1, plot.conf=FALSE, ylab="Daily Hardcover Book Sales",
     xlab="Time", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.8)),pch=1)

fit1
fit2
fit3

SSEfit1 <- sum((books[,"Hardcover"] - fit1$fitted)^2)
SSEfit2 <- sum((books[,"Hardcover"] - fit2$fitted)^2)
SSEfit3 <- sum((books[,"Hardcover"] - fit3$fitted)^2)

SSEfit1 # 33148.16
SSEfit2 # 33059.93
SSEfit3 # 37641.79

fit4 <- ses(books[,"Hardcover"], initial="simple", h=4) # I think this shows the optimal value of α since I didn't specify alpha
fit4
SSEfit4 <- sum((books[,"Hardcover"] - fit4$fitted)^2)
SSEfit4 # 30758.07

fit5 <- ses(books[,"Hardcover"], initial="optimal", h=4) # I think this shows the optimal value of α since I didn't specify alpha
fit5
SSEfit5 <- sum((books[,"Hardcover"] - fit5$fitted)^2)
SSEfit5 # 30587.69

### The hardcover forecasts are higher than the paperback forecats, and the SSE values are lower.

## 2 ##

# Apply Holt’s linear method to the paperback and hardback series and compute four-day forecasts in each case.
fcastP <- holt(books[,"Paperback"], h=4)
plot(fcastP)
fcastH <- holt(books[,"Hardcover"], h=4)
plot(fcastH)

# a. Compare the SSE measures of Holt’s method for the two series to those of simple exponential smoothing in the previous question.
# Discuss the merits of the two forecasting methods for these data sets.
fitP <- ses(books[,"Paperback"], initial="optimal", h=4)
SSEfitP <- sum(fitP$residuals^2)
fitH <- ses(books[,"Hardcover"], initial="optimal", h=4)
SSEfitH <- sum(fitH$residuals^2)

SSEfcastP <- sum(fcastP$residuals^2)
SSEfcastH <- sum(fcastH$residuals^2)

# Simple Exponential Smoothing
SSEfitP # 33944.82
SSEfitH # 30587.69
# Holt's Linear
SSEfcastP # 30074.17
SSEfcastH # 22581.83

### The SSE values for Holt's method are smaller than the SSE values for the simple exponential smoothing method. 

# b. Compare the forecasts for the two series using both methods. Which do you think is best?
# Paperback
par(mfrow=c(1,2))
plot(fitP, main = "Simple Exponential Smoothing: Paperback Books")
plot(fcastP, main = "Holt's Linear Method: Paperback Books")
# Hardcover
par(mfrow=c(1,2))
plot(fitH, main = "Simple Exponential Smoothing: Hardcover Books")
plot(fcastH, main = "Holt's Linear Method: Hardcover Books")

### I think the Holt's forecasts are best for both paperback and hardcover books because they take into account the upward trend.

# c. Calculate a 95% prediction interval for the first forecast for each series using both methods, assuming normal errors.
# Compare your forecasts with those produced by R.

fitP <- ses(books[,"Paperback"], initial="optimal", h=4, level=c(95))
fitH <- ses(books[,"Hardcover"], initial="optimal", h=4, level=c(95))
fcastP <- holt(books[,"Paperback"], h=4, level=c(95))
fcastH <- holt(books[,"Hardcover"], h=4, level=c(95))

fitP
fitH
fcastP
fcastH

# Point Forecast    Lo 95    Hi 95
# 247.3504          193.5770 301.1237
# 250.3400          196.5667 304.1133
# 253.3296          199.5563 307.1030
# 256.3192          202.5459 310.0926

### The confidence intervals for each forecast have a similar range, but are each 3 values apart.

## 3 BONUS ##

# For this exercise, use the quarterly UK passenger vehicle production data from 1977:1--2005:1 (data set ukcars).

# a. Plot the data and describe the main features of the series.
par(mfrow=c(1,1))
plot(ukcars, ylab = "Number of Cars (thousands)") # quarterly data
### The plot reveals seasonality. There's not a clear trend.

# b. Decompose the series using STL and obtain the seasonally adjusted data.
fitSTL <- stl(ukcars, s.window = "periodic")
plot(fitSTL)
STLadj <- seasadj(fitSTL)
plot(STLadj)

# c. Forecast the next two years of the series using an additive damped trend method
# applied to the seasonally adjusted data. Then reseasonalize the forecasts. Record
# the parameters of the method and report the RMSE of the one-step forecasts from your method.
HWAdamp <- holt(STLadj, damped=TRUE, h = 8)
plot(ukcars)
lines(HWAdamp$mean + fitSTL$time.series[2:9,"seasonal"], col = "blue")
HWAdampRMSE <- sqrt(mean(((HWAdamp$fitted + fitSTL$time.series[,"seasonal"]) - ukcars)^2))
HWAdampRMSE

# d. Forecast the next two years of the series using Holt's linear method applied to
# the seasonally adjusted data. Then reseasonalize the forecasts. Record the parameters
# of the method and report the RMSE of of the one-step forecasts from your method.
HWfit <- holt(STLadj, h = 8)
plot(ukcars)
lines(HWfit$mean + fitSTL$time.series[2:9,"seasonal"], col = "blue")
HWfitRMSE <- sqrt(mean(((HWAdamp$fitted + fitSTL$time.series[,"seasonal"]) - ukcars)^2))
HWfitRMSE

# e. Now use ets() to choose a seasonal model for the data.
fitETS <- ets(ukcars)
fcastfitETS <- forecast(fitETS, h = 8)
plot(fcastfitETS)
etsRMSE <- sqrt(mean((fcastfitETS$fitted - ukcars)^2))
etsRMSE

# f. Compare the RMSE of the fitted model with the RMSE of the model you obtained
# using an STL decomposition with Holt's method. Which gives the better in-sample fits?
HWfitRMSE
etsRMSE
### HWfitRMSE give the better in-sample fits

# g. Compare the forecasts from the two approaches? Which seems most reasonable?
HWfit # I need to figure out how to calculate the AIC for this model so I can compare the two models.
fitETS

## 4 BONUS ##

# For this exercise, use the monthly Australian short-term overseas visitors data, May 1985--April 2005. (Data set: visitors.)

# a. Make a time plot of your data and describe the main features of the series.
visitors
plot(visitors, ylab = "Number of People (Thousands)")
### The plot definitely shows an upward trend and seasonality.

# b. Forecast the next two years using Holt-Winters' multiplicative method.
HWMfcast <- hw(visitors, h=24, seasonal="multiplicative")
plot(HWMfcast)

# c. Why is multiplicative seasonality necessary here?
### The seasonal pattern increases in size rather than remain at a steady magnitude. This makes multiplicative seasonality appropriate.

# d. Experiment with making the trend exponential and/or damped.

HWMfcastdamp <- hw(visitors, h=24, seasonal="multiplicative", damped=TRUE, exponential=FALSE)
plot(HWMfcastdamp, main = "Damped Trend Visitors Forecasts")

# e. Compare the RMSE of the one-step forecasts from the various methods. Which do you prefer?
c(RMSE = sqrt(HWMfcast$model$mse), RMSEdamp = sqrt(HWMfcastdamp$model$mse))
### The damped model has a lower RMSE, so I prefer that one.

# f. Now fit each of the following models to the same data:
#   - a multiplicative Holt-Winters' method;
HWMfcast <- hw(visitors, h=24, seasonal="multiplicative")
plot(HWMfcast)
#   - an ETS model;
fitETS <- ets(visitors)
ETSfcast <- forecast(fitETS, h = 24)
plot(ETSfcast)
#   - an additive ETS model applied to a Box-Cox transformed series;
fitETSbc <- ets(visitors, model = "AAA")
fcastETSbc <- forecast(fitETSbc, h = 24)
plot(fcastETSbc)
#   - a seasonal naive method applied to the Box-Cox transformed series;
fcastSnaive <- snaive(visitors, h = 24)
plot(fcastSnaive)
#   - an STL decomposition applied to the Box-Cox transformed data followed by an ETS model
#     applied to the seasonally adjusted (transformed) data.
fitSTL <- stlm(visitors, method = "ets")
fcastSTL <- forecast(fitSTL, h = 24)
plot(fcastSTL)

# g. For each model, look at the residual diagnostics and compare the forecasts for the next two years. Which do you prefer?
par(mfrow=c(1,5))
plot(HWMfcast$residuals, ylab = "Residuals", type="p")
abline(h = 0, col = "gray")

plot(ETSfcast$residuals, ylab = "Residuals", type="p")
abline(h = 0, col = "gray")

plot(fcastETSbc$residuals, ylab = "Residuals", type="p")
abline(h = 0, col = "gray")

plot(fcastSnaive$residuals, ylab = "Residuals", type="p")
abline(h = 0, col = "gray")

plot(fcastSTL$residuals, ylab = "Residuals", type="p")
abline(h = 0, col = "gray")

### The residuals look best for either the ETS or STL forecasts.

##########################
# Chapter 8.11 Exercises
##########################

## 5 ##

# Use R to simulate and plot some data from simple ARIMA models.

# a. Use the following R code to generate data from an AR(1) model with ϕ1 = 0.6 and σ2 = 1. The process starts with y0 = 0.
par(mfrow=c(1,1))

fitAR1 <- function(phi)
{
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]
return(y)
}
plot(fitAR1(.6))

# b. Produce a time plot for the series. How does the plot change as you change ϕ1?
par(mfrow=c(1,3))
plot(fitAR1(-.6))
plot(fitAR1(0))
plot(fitAR1(.6))

### The spikes change intensity and location as phi changes.

### I have no idea how to do c. - f. based on the reading. ###
# c. Write your own code to generate data from an MA(1) model with ϕ1=0.6 and σ2=1.
# d. Produce a time plot for the series. How does the plot change as you change θ1?
# e. Generate data from an ARMA(1,1) model with ϕ1 = 0.6 and θ1 = 0.6 and σ2=1.
# f. Generate data from an AR(2) model with ϕ1 = -0.8, ϕ2= 0.3 and σ2=1.
par(mfrow=c(1,1))
fitAR2 <- function(phi)

y <- ts(numeric(100))
e <- rnorm(100, 0, 1)
for(i in 2:100)
{
  y[i] <- -0.8*y[i-1] + 0.3*y[i-2] + e[i]
}
plot(fitAR2)

# (Note that these parameters will give a non-stationary series.)
# g. Graph the latter two series and compare them.
### I can't compare since I couldn't figure out the code.

## 6 ##

# Consider the number of women murdered each year (per 100,000 standard population) in the United States (data set wmurders).

# a. By studying appropriate graphs of the series in R, find an appropriate ARIMA(p,d,q) model for these data.
plot(wmurders)
AR1 <- auto.arima(wmurders) # this chooses (1,2,1)
plot(forecast(AR1, h=5))

#log_wmurders <- log(wmurders)
#plot(log_wmurders) # log doesn't seem to make a huge differnce

# b. Should you include a constant in the model? Explain.
### No, I don't think so. Including a constant implies there's a trend in the data, but the plot shows no trend.

# c. Write this model in terms of the backshift operator.
### I have no idea how to do this based off the reading.

# d. Fit the model using R and examine the residuals. Is the model satisfactory?
AR1res <- AR1$residuals
par(mfrow=c(1,2))
plot(AR1res, type="p")
abline(0,0, col="gray")
hist(AR1res)

### The histogram of the residuals follows a pretty normal distribution, but the scatterplot shows a funnel shape.
### These residual plots suggest the model probably is not satisfactory.

par(mfrow=c(1,1))
# e. Forecast three times ahead. Check your forecasts by hand to make sure you know how they have been calculated.
fcast1 <- forecast(AR1, h=5)
fcast2 <- forecast(AR1, h=10)
fcast3 <- forecast(AR1, h=15)
fcast1
fcast2
fcast3

### I'm not sure how to check this by hand.

# f. Create a plot of the series with forecasts and prediction intervals for the next three periods shown.
par(mfrow=c(3,1))
plot(forecast(AR1, h=5))
plot(forecast(AR1, h=10))
plot(forecast(AR1, h=15))

# g. Does auto.arima give the same model you have chosen? If not, which model do you think is better?
### I used auto.arima, so I don't know, but I'll plot another model to compare:
AR2 <-Arima(wmurders, c(0,1,0))
plot(forecast(AR2, h=5))
c(AR1$aicc, AR2$aicc)
### The AIC values actually shows that auto.arima did not perform as well!

## 7 BONUS ##

# Consider the quarterly number of international tourists to Australia for the period 1999–2010. (Data set austourists.)

# a. Describe the time plot.
plot(austourists)
### There's an upward seasonal trend.

# b. What can you learn from the ACF graph?
acf(austourists)
### I think there's differencing.

# c. What can you learn from the PACF graph?
pacf(austourists)
### I think there's differencing.

# d. Produce plots of the seasonally differenced data (1−B4)Yt. What model do these graphs suggest?
plot(diff(austourists))
### I don't know which model the plot suggests.

# e. Does auto.arima give the same model that you chose? If not, which model do you think is better?
auto.arima(austourists)

# f. Write the model in terms of the backshift operator, and then without using the backshift operator.
### Not sure.

## 8 BONUS - only partially attempted ##

# Consider the total net generation of electricity (in billion kilowatt hours) by the U.S. electric industry
# (monthly for the period 1985–1996). (Data set usmelec.) In general there are two peaks per year: in mid-summer and mid-winter.

# a. Examine the 12-month moving average of this series to see what kind of trend is involved.
plot(usmelec)
lines(ma(usmelec, 12), col = "blue")
### There's an upward trend.

# b. Do the data need transforming? If so, find a suitable transformation.
usmelecBC <- BoxCox.lambda(usmelec)
plot(BoxCox(usmelec, usmelecBC))
### It looks like this helps a little bit.

# c. Are the data stationary? If not, find an appropriate differencing which yields stationary data.
# d. Identify a couple of ARIMA models that might be useful in describing the time series.
# Which of your models is the best according to their AIC values?
# e. Estimate the parameters of your best model and do diagnostic testing on the residuals.
# Do the residuals resemble white noise? If not, try to find another ARIMA model which fits better.
# f. Forecast the next 15 years of generation of electricity by the U.S. electric industry.
# Get the latest figures from http://data.is/zgRWCO to check on the accuracy of your forecasts.
# g. How many years of forecasts do you think are sufficiently accurate to be usable?