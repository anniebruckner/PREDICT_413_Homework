# Andrea Bruckner
# Predict 413: Homework 1
# Forecasting: Principles and Practice, Chapters 2 and 4 exercises

### All of my responses to questions are preceded by three pound symbols (###). Code comments are preceded by only one #, and new questions have two #s.

install.packages("forecast")
library(forecast)
install.packages("fma")
library(fma)
data(package="fma")

##########################
# Chapter 2.8 Exercises
##########################

## 1 ##

# For each of the following series (from the fma package), make a graph of the data. If transforming seems appropriate, do so and describe the effect.

# a.) Monthly total of people on unemployed benefits in Australia (January 1956–July 1992).
# a.) dataset = dole
options(scipen=5) # prevents scientific notation in labeling
# Graph A1
plot(dole,main="Monthly total of people on unemployed benefits in Australia \n (January 1956–July 1992)",
     xlab="Year", ylab="Number of People")
# Graph A2
lambda <- BoxCox.lambda(dole) # = 0.329
plot(BoxCox(dole,lambda))
### The number of people on unemployment benefits has generally increased drastically over time.
### I tried a Box-Cox transformation to see if it lessened the severity of the peaks of the graph--
### it make the peaks in the 1950s and 1960s more pronounced while toning down the peaks in the 1980s and 1990s.

# b.) Monthly total of accidental deaths in the United States (January 1973–December 1978).
# b.) dataset = usdeaths
# Graph B1
plot(usdeaths, main="Monthly total of accidental deaths in the United States \n (January 1973–December 1978)",
     xlab="Time", ylab="Number of Deaths")
# Graph B2
lambda <- BoxCox.lambda(usdeaths) # = -0.034
plot(BoxCox(usdeaths,lambda))
### The transformation did not change the graph much.

# c.) Quarterly production of bricks (in millions of units) at Portland, Australia (March 1956–September 1994).
# c.) dataset = bricksq
# Graph C1
plot(bricksq, main="Quarterly production of bricks (in millions of units) at Portland, Australia \n (March 1956–September 1994)",
     xlab="Time", ylab="Number of Bricks")
# Graph C2
lambda <- BoxCox.lambda(bricksq) # = 0.255
plot(BoxCox(bricksq,lambda))
### The transformation made the peaks less drastic.

## 2 ##

# Use the Dow Jones index (data set dowjones) to do the following:

# a.) Produce a time plot of the series.
plot(dowjones, main = "Dow Jones Index Prices Over Time", xlab = "Time", ylab = "Price (dollars)")

# b.) Produce forecasts using the drift method and plot them.
h=20
dowDrift <- rwf(dowjones,h, drift=TRUE)  # drift forecast
plot(dowDrift)

# c.) Show that the graphed forecasts are identical to extending the line drawn between the first and last observations.
first <- 110.94 # I found the first and last value by using print(dowjones)
last <- 121.23
length <- 77 # 78 data points - 1 = length
slope <- (last-first)/length
intercept <- first-slope

h=20
dowDrift <- rwf(dowjones,h, drift=TRUE)  # drift forecast
plot(dowDrift)
abline(intercept, slope, lty = 4, col = "orange")

# d.) Try some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?
h=20
dowDrift <- rwf(dowjones,h, drift=TRUE)  # drift forecast
plot(dowDrift, main = "Dow Jones Index Prices Over Time", xlab = "Time", ylab = "Price (dollars)")
lines(meanf(dowjones, h)$mean, col="purple")
lines(naive(dowjones, h)$mean, col="red")
# lines(snaive(dowjones, h)$mean, col="green")  # plots the same as naive method
legend("topleft",
       legend = c("Drift", "Mean", "Naive"),
       col = c("blue", "purple","red"), lty=1)

accuracy(meanf(dowjones, h))
accuracy(naive(dowjones, h))
accuracy(rwf(dowjones, h, drift=TRUE))

### The Drift method produced the most accurate forecasts given the RMSE, MAE, etc. output, 
### but the Naive method was very good as well. The Mean method was the worst per these metrics.

## 3 ##

# Consider the daily closing IBM stock prices (data set ibmclose).

# a.) Produce some plots of the data in order to become familiar with it.
summary(ibmclose)
str(ibmclose)
head(ibmclose)

plot(ibmclose)
lines(ibmclose)

min(ibmclose) # $306
max(ibmclose) # $603

t=seq(1:length(ibmclose))  # use a temporary variable for plotting the sequence
scatter.smooth(t,ibmclose, col="orange", main="IBM Close Prices") # simple scatter
lines(ibmclose~t,col="blue")  # simple line
abline(reg=lm(ibmclose~t), col="red")  # simple regression

# b.) Split the data into a training set of 300 observations and a test set of 69 observations.
ibmTrain = window(ibmclose, start = 1, end = 300)
ibmTest = window(ibmclose, start = 301)

# c.) Try various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
# I think the Drift method did best.
fit1 = meanf(ibmTrain, h=30) # mean method
fit2 = naive(ibmTrain, h=30) # naive method
fit3 = rwf(ibmTrain, h=30, drift = TRUE) # drift method

plot(fit1, plot.conf = TRUE, main = "Forecasts for daily closing IBM stock prices", 
     xlab = "Time (days)", ylab = "Daily Closing Price (dollars)", pch=NA,
     ylim=c(300, 600))
lines(fit2$mean, col=2)
lines(fit3$mean, col=3)
lines(ibmclose)
legend("topleft", lty=1, col=c(4,2,3), legend = c("Mean", "Naive", "Drift"))

accuracy(fit1, ibmTest) # mean
accuracy(fit2, ibmTest) # naive
accuracy(fit3, ibmTest) # drift

### The Mean method performed the worst. The Drift and Naive methods performed much better and showed similar accuracy.
### The Drift method was best if you base it on RMSE, but the Naive method was best if you base it on MAE.

## 4 ##

# Consider the sales of new one-family houses in the USA, Jan 1973 – Nov 1995 (data set hsales).

# a.) Produce some plots of the data in order to become familiar with it.
summary(hsales)
str(hsales)
head(hsales)

plot(hsales)
lines(hsales)

min(hsales) # 24
max(hsales) # 89

t=seq(1:length(hsales))  # use a temporary variable for plotting the sequence
scatter.smooth(t,hsales, col="orange", main="Sales of new one-family houses in the USA (Jan 1973 – Nov 1995") # simple scatter
lines(hsales~t,col="blue")  # simple line
abline(reg=lm(hsales~t), col="red")  # simple regression

# b.) Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
houseTrain = window(hsales, end = c(1993,12))
houseTest = window(hsales, start = c(1994,1)) # 1994 and 1995 are last 2 years of data

# c.) Try various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
# Either the Mean or Native method did best.
fit1 = meanf(houseTrain, h=30) # mean method
fit2 = naive(houseTrain, h=30) # naive method
fit3 = snaive(houseTrain, h=30) # seasonal naive method
fit4 = rwf(houseTrain, h=30, drift = TRUE) # drift method

plot(fit1, plot.conf = TRUE, main = "Forecasts for home sales", 
     xlab = "Years", ylab = "Houses Sold", pch=NA,
     ylim=c(20, 90))
lines(fit2$mean, col=2)
lines(fit3$mean, col=3)
lines(fit4$mean, col=5)
lines(hsales)
legend("topleft", lty=1, col=c(4,2,3,5), legend = c("Mean", "Naive", "Seasonal Naive", "Drift"))

accuracy(fit1, houseTest) # mean
accuracy(fit2, houseTest) # naive
accuracy(fit3, houseTest) # seasonal naive
accuracy(fit4, houseTest) # drift

### Per RMSE and MAE, the Drift method did best, followed by Naive, Seasonal Naive, and then Mean methods.

##########################
# Chapter 4.10 Exercises
##########################

## 1 ##

# Electricity consumption was recorded for a small town on 12 randomly chosen days. The following maximum temperatures (degrees Celsius) and consumption (megawatt-hours) were recorded for each day. 
# Day	1	2	3	4	5	6	7	8	9	10	11	12
# Mwh	16.3	16.8	15.5	18.2	15.2	17.5	19.8	19.0	17.5	16.0	19.6	18.0
# temp	29.3	21.7	23.7	10.4	29.7	11.9	9.0	23.4	17.8	30.0	8.6	11.8

# a.) Plot the data and find the regression model for Mwh with temperature as an explanatory variable. Why is there a negative relationship?
plot(Mwh ~ temp, data=econsumption)
fit  <- lm(Mwh ~ temp, data=econsumption)
abline(fit, col="red")
### There is a negative relationship because warmer temperatures usually signify longer daylight hours,
### which would cause residents to not need to use as much electricity for lighting. People also consume 
### more electricity when it is cold probaby to heat their homes.

# b.) Produce a residual plot. Is the model adequate? Are there any outliers or influential observations?
plot(residuals(fit) ~ temp, data=econsumption)
abline(0, 0, col="red")
### The model is generally adequate since the residuals appear random with mean zero.
### There is one outlier/influential point at around where temp = 25, residuals(fit) = 2.0, but we won't know
### its impact unless we have more data.

# c.) Use the model to predict the electricity consumption that you would expect for a day with maximum temperature 10∘ and a day with maximum temperature 35∘. Do you believe these predictions?
forecast(fit, newdata=data.frame(temp=c(10,35)))
### Although we should interpret the predictions for 35 with caution since 35 is beyond the range of the original data,
### these predictions seem mostly reasonable. If you plot the forecasted values on the plot in step a., 
### they seem like they'd fit in.

# d.) Give prediction intervals for your forecasts.
###             Lo 80    Hi 80    Lo 95    Hi 95
### For 10 deg: 17.27010 20.22579 16.34824 21.14766
### For 35 deg: 13.50469 16.73335 12.49768 17.74035

## 2 ##

# The following table gives the winning times (in seconds) for the men’s 400 meters final in each Olympic Games from 1896 to 2012 (data set `olympic`).
# 1896	54.2	1928	47.8	1964	45.1	1992	43.50
# 1900	49.4	1932	46.2	1968	43.8	1996	43.49
# 1904	49.2	1936	46.5	1972	44.66
# 1908	50.0	1948	46.2	1976	44.27
# 1912	48.2	1952	45.9	1980	44.60
# 1920	49.6	1956	46.7	1984	44.27
# 1924	47.6	1960	44.9	1988	43.87

# a.) Update the data set `olympic` to include the winning times from the last few Olympics.
olympicNew <- olympic
olympicNew = rbind(olympic, data.frame(Year = c(2000, 2004, 2008, 2012),time = c(43.84, 44.00, 43.75, 43.94)))
print(olympicNew)
### I found this data on Wikipedia.

# b.) Plot the winning time against the year. Describe the main features of the scatterplot.
plot(time ~ Year, data = olympicNew)
### The scatterplot shows a downward linear trend. It looks like the world record occurred in the late 1990s, and current Olympians are still trying to beat it.
### The first data point, Year 1896, is an outlier.

# c.) Fit a regression line to the data. Obviously the winning times have been decreasing, but at what *average* rate per year?
plot(time ~ Year, data = olympicNew)
abline(lm(time ~ Year, data = olympicNew), col="red")
(fit = lm(time ~ Year, data = olympicNew))
summary(fit) # Shows Year parameter estimate = -0.06585
### The winning times have been decreasing at an average rate of -0.06585 seconds per year.

# d.) Plot the residuals against the year. What does this indicate about the suitability of the fitted line?
plot(fit$residuals ~ Year, data = olympicNew)
abline(h = 0, col = "green")
### The fitted line does not describe most of the data. The data for the very first record is an outlier, 
### and the residuals are not random starting in the 1980s. The residuals show an upward trend at this point.

# e.) Predict the winning time for the men’s 400 meters final in the 2000, 2004, 2008 and 2012 Olympics. Give a prediction interval for each of your forecasts. What assumptions have you made in these calculations?
olympicTrain <- olympicNew[1:23,] # 1-23 represent Olympic Years prior to 2000
olympicTest <- olympicNew[24:27,] # 24-27 represent Olympic Years 2000-2012
olympicFit <- lm(time ~ Year, data = olympicTrain)
olympicForecast <- plot(forecast(olympicFit, newdata = olympicTest[,"Year"]))
lines(time ~ Year, data = olympicTest, col = "orange", type = "p")
olympicForecast # shows all the prediction intervals

# This code produces easier to interpret output, but it shows the same predictions and intervals as the code above.
forecast(olympicFit, newdata=data.frame(Year=2000)) # prediction: 42.49977; actual: 43.84
forecast(olympicFit, newdata=data.frame(Year=2004)) # prediction: 42.19261; actual: 44.00
forecast(olympicFit, newdata=data.frame(Year=2008)) # prediction: 41.88545; actual: 43.75
forecast(olympicFit, newdata=data.frame(Year=2012)) # prediction: 41.57829; actual: 43.94

### I assumed the data is linear since I calculated the predictions using a linear model.
### From looking at earlier graphs, we know that the data is not linear in recent years,
### which means the predictions would probably not be very accurate.

# f.) Find out the actual winning times for these Olympics (see www.databaseolympics.com). How good were your forecasts and prediction intervals?
# Year = c(2000, 2004, 2008, 2012),time = c(43.84, 44.00, 43.75, 43.94)

### All of the forecasts were 1-2 seconds faster than the actual times, which shows that the forecasts were not good.
### However, all of the actual forecasts fell within the 80 and 95 percent confidence intervals, so these were accurate.

## 3 ##

# An elasticity coefficient is the ratio of the percentage change in the forecast variable (y) to the percentage change in the predictor variable (x). Mathematically, the elasticity is defined as (dy/dx)×(x/y)(dy/dx)×(x/y). Consider the log-log model,
# logy=β0+β1logx+ε

# a.) Express y as a function of x and show that the coefficient β1 is the elasticity coefficient.
### y = 10, x = 100
### log(10) = β0+β1log(100)+ε
### 1 = β0+2β1+ε
### y = β0+2β1*x+ε

### y = 1000, x = 10
### log(1000) = β0+β1log(10)+ε
### 3 = β0+1β1+ε
### y = β0+β1*x+ε

### In the above two equations, the value for β1 remains the same but the slope changes.