# Andrea Bruckner
# Predict 413: Homework 2
# Forecasting: Principles and Practice, Chapters 5 and 6 exercises

### All of my responses to questions are preceded by three pound symbols (###). Code comments are preceded by only one #, and new questions have two #s.

install.packages("forecast")
install.packages("fma")
install.packages("lmtest")
library(forecast)
library(fma)
library(lmtest)
data(package="fma")

##########################
# Chapter 5.8 Exercises
##########################

## 1 ##

# The data below (data set fancy) concern the monthly sales figures of a shop which opened in January 1987 
# and sells gifts, souvenirs, and novelties. The shop is situated on the wharf at a beach resort town in 
# Queensland, Australia. The sales volume varies with the seasonal population of tourists. There is a large
# influx of visitors to the town at Christmas and for the local surfing festival, held every March since 1988.
# Over time, the shop has expanded its premises, range of products, and staff.

# a.	Produce a time plot of the data and describe the patterns in the graph. Identify any unusual or unexpected fluctuations in the time series.
options(scipen=5)
par(mfrow = c(1,1))
plot(fancy, main="Monthly Sales",
     xlab="Year", ylab="Sales Volume")

### The plot shows seasonal data where the sales spike near the end of the year.
### There's a small spike also around March for the surf festival every year except 1987.
### The sale spike in March increases in proportion to the increase in sales each year.
### Overall there is an upward trend.
### The sales between 1993 and 1994 follow a similar pattern as the other years,
### but the sales from the middle to end of the year increase rapidly with curvature.

# b.	Explain why it is necessary to take logarithms of these data before fitting a model.
### Logarithms minimize the variation and make the data easier to interpret.

# c.	Use R to fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable.
log_fancy <- log(fancy)

surfFest <- cycle(log_fancy) == 3 # selects just March
surfFest[3] <- 0 # March 1987 = 0 since fest started in 1988
surfFest

fit <- tslm(log_fancy ~ trend + season + surfFest)
summary(fit)

plot(log_fancy, ylab = "Sales Volume")
lines(fitted(fit), col = "blue")
legend("topleft", legend = c("Original data", "Fitted values"),
       col = c("blue", "black"), lty = 1)

# d.	Plot the residuals against time and against the fitted values. Do these plots reveal any problems with the model?
par(mfrow=c(1,2))
res <- residuals(fit)
plot(res, type="p", ylab = "Residuals")
abline(h = 0, col="orange")

res_fit <- fitted(fit)
#plot(res_fit, type="p")
#abline(h = 0, col="orange")

# plot(res_fit ~ res, xlab="Fitted Values", ylab="Residuals")
## Not sure why this keeps showing years instead of points...

plot(c(res) ~ c(res_fit), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col="orange")

### There are no classically shaped residual patterns to indicate a problem,
### but the res plot kind of resembles a sine curve. Perhaps there is autocorrelation.

# e.	Do boxplots of the residuals for each month. Does this reveal any problems with the model?
par(mfrow=c(1,1))
month <- cycle(res)
boxplot(res ~ month, notch=FALSE)

### The boxpltos don't seem to reveal any problems.

# f.	What do the values of the coefficients tell you about each variable?
summary(fit)

### All the coefficients are positive and reveal how greatly sales increase according
### to the month. The coefficients for Nov and Dec are highest, which supports our observation
### that these months have the highest sales. The variable, surfFest, shows that sales increase by 0.50
### compared to months when the surfing festival is not occuring.

# g.	What does the Durbin-Watson statistic tell you about your model?

dwtest(fit, alternative="two.sided")
# It is recommended that the two-sided test always be used
# to check for negative as well as positive autocorrelation

### Because the p-value is very small, the Durbin-Watson statistic suggests
### that autocorrelation is present in the residuals, indicating that the model
### can and should be improved.

# h.	Regardless of your answers to the above questions, use your regression model to predict the monthly sales for 1994, 1995, and 1996. Produce prediction intervals for each of your forecasts.

newFest <- rep(0, 36) # 36 months to represent the 3 years to forecast
newFest
newFest[c(3, 15, 27)] <- 1 # only festival months have 1 populated
newFest
fcastFest = forecast(fit, h = 36, newdata=data.frame(surfFest = newFest))
plot(fcastFest)
fcastFest # prediction intervals

# i.	Transform your predictions and intervals to obtain predictions and intervals for the raw data.
newFest_log <- log(newFest)
# fit2 <- lm(newFest_log ~ surfFest)
# summary(fit2)

### I couldn't get the code right. I'm not sure if we need to transform the predictions
### and intervals though since the original data was already transformed.

# j.	How could you improve these predictions by modifying the model?

### I used a linear model on non-linear data, so choosing a different modeling technique
### would likely improve the predictions.


## 2 ##

# The data below (data set texasgas) shows the demand for natural gas 
# and the price of natural gas for 20 towns in Texas in 1969.

# a.	Do a scatterplot of consumption against price. The data are clearly not linear. Three possible nonlinear models for the data are given below
# Ci=exp(a+bPi+ei)={a1+b1Pi+eia2+b2Pi+eiwhen Pi≤60when Pi>60;=a+b1P+b2P2.
# Ci=expa(a+bPi+ei)Ci={a1+b1Pi+eiwhen Pi≤60a2+b2Pi+eiwhen Pi>60;Ci=a+b1P+b2P2.
# The second model divides the data into two sections, depending on whether the price is above or below 60 cents per 1,000 cubic feet.

plot(consumption ~ price, data = texasgas)

# b.	Can you explain why the slope of the fitted line should change with P?

### The slope should change with P because the prices change. A higher price won't have the same slope as a lower price.

# c.	Fit the three models and find the coefficients, and residual variance in each case.
# Shorten names for variables of interest
price <- texasgas$price
consumption <- texasgas$consumption

# Model 1
model1 <- lm(log(consumption) ~ price)
plot(consumption ~ price, data = texasgas)
lines(price, exp(fitted(model1)), col = "blue")
summary(model1)

# Model 2--not sure how to do

# Model 3
model3 <- lm(consumption ~ price + price^2, data = texasgas)
plot(consumption ~ price, data = texasgas)
lines(price, fitted(model3), col = "red")
summary(model3)

# For the second model, the parameters a1a1, a2a2, b1b1, b2b2 can be estimated by simply fitting a regression with four regressors but no constant:
# (i) a dummy taking value 1 when P≤60P≤60 and 0 otherwise;
# (ii) P1=PP1=P when P≤60P≤60 and 0 otherwise;
# (iii) a dummy taking value 0 when P≤60P≤60 and 1 otherwise;
# (iv) P2=PP2=P when P>60P>60 and 0 otherwise.

# d.	For each model, find the value of R2 and AIC, and produce a residual plot. Comment on the adequacy of the three models.

# Model 1
res1 <-exp(fitted(model1)) - consumption
plot(res1 ~ price, ylab = "Residuals for Model 1")
abline(h = 0, col = "orange")

SSE1 <- sum(res1^2)
N <- length(consumption)
R2 <- 1 - SSE1/sum((consumption - sum(consumption)/N)^2)
R2

AIC <- N * log(SSE1/N) + 2 * 3
AIC


# Model 2--not sure how to do

# Model 3
res3 <-exp(fitted(model3)) - consumption
plot(res1 ~ price, ylab = "Residuals for Model 3")
abline(h = 0, col = "orange")

SSE3 <- sum(res3^2)
N <- length(consumption)
R2 <- 1 - SSE3/sum((consumption - sum(consumption)/N)^2)
R2

AIC <- N * log(SSE3/N) + 2 * 3
AIC

### The residuals look the same for Models 1 and 3. The residuals don't appear completely random,
### but there's not a clear pattern either.

# e.	For prices 40, 60, 80, 100, and 120 cents per 1,000 cubic feet, compute the forecasted per capita demand using the best model of the three above.
newPrices <- data.frame(price = c(40, 50, 60, 61, 70, 80, 90, 100, 110, 120))
pricesfcast <- forecast(model1, newdata = newPrices, level = 95)
plot(price, consumption)
lines(newPrices$price, pricesfcast$mean, col="blue")
abline(h = 0, col = "orange") # Why is this not showing up?

# f.	Compute 95% prediction intervals. Make a graph of these prediction intervals and discuss their interpretation.
### Not sure.

# g.	What is the correlation between P and P2? Does this suggest any general problem to be considered in dealing with polynomial regressions---especially of higher orders?

cor(price, price^2) # 0.9904481
### This is a very high correlation.

##########################
# Chapter 6.7 Exercises
##########################

## 1 ##

# Show that a 3×5 MA is equivalent to a 7-term weighted moving average with weights of 0.067, 0.133, 0.200, 0.200, 0.200, 0.133, and 0.067.

ma5 <- ma(plastics, order=5, centre=FALSE)
ma5
ma3x5 <- ma(plastics, order=5, centre=TRUE)
ma3x5

### I have no idea.

## 2 ##

# The data below represent the monthly sales (in thousands) of product A for a plastics manufacturer for years 1 through 5 (data set plastics).

# a.	Plot the time series of sales of product A. Can you identify seasonal fluctuations and/or a trend?
plot(plastics, ylab = "Sales of Product A")

### There is an upward trend and smooth seasonal fluctuations.

# b.	Use a classical multiplicative decomposition to calculate the trend-cycle and seasonal indices.
fit <- decompose(plastics, type="multiplicative")
plot(fit) # confirms upward trend

# c.	Do the results support the graphical interpretation from part (a)?

### Yes, the decomposition supports that there is an upward trend and that the seasonal fluctuations are smooth.

# d.	Compute and plot the seasonally adjusted data.
seasonalAdj <- seasadj(fit)
plot(seasonalAdj, ylab = "Seasonally Adjusted Plastics Sales Data")

# e.	Change one observation to be an outlier (e.g., add 500 to one observation), and recompute the seasonally adjusted data. What is the effect of the outlier?
plasticsMod <- plastics
plasticsMod[16] <- plasticsMod[16] + 500
fit2 <- decompose(plasticsMod, type="multiplicative")
plot(fit2)
seasonalAdj2 <- seasadj(fit2)
plot(seasonalAdj2, ylab = "Seasonally Adjusted Modified Plastics Sales Data")

par(mfrow=c(1,2))
plot(seasonalAdj, ylab = "Seasonally Adjusted Plastics Sales Data")
plot(seasonalAdj2, ylab = "Seasonally Adjusted Modified Plastics Sales Data")

### The outlier didn't seem to make any difference. I'm going to try again using a different data point for the outlier.

plasticsMod2 <- plastics
plasticsMod2[50] <- plasticsMod2[50] + 500
fit3 <- decompose(plasticsMod2, type="multiplicative")
plot(fit3)
seasonalAdj3 <- seasadj(fit3)
plot(seasonalAdj3, ylab = "Seasonally Adjusted Modified Plastics Sales Data")

par(mfrow=c(1,3))
plot(seasonalAdj, ylab = "Seasonally Adjusted Plastics Sales Data")
plot(seasonalAdj2, ylab = "Seasonally Adjusted Modified Plastics Sales Data")
plot(seasonalAdj3, ylab = "Seasonally Adjusted Modified Again Plastics Sales Data")

### This time the outlier had a huge effect.

# f.	Does it make any difference if the outlier is near the end rather than in the middle of the time series?

### Yes. When I put the outlier close to the end of the data, it made a huge difference.

# g.	Use a random walk with drift to produce forecasts of the seasonally adjusted data.

par(mfrow=c(1,1)) # reset plotting window
fitRWF <- decompose(plastics, type="multiplicative")
seasonalAdjRWF <- seasadj(fitRWF)
rwfFit <- rwf(seasonalAdjRWF, drift=TRUE, h=12)
plot(rwfFit)

# h.	Reseasonalize the results to give forecasts on the original scale.

plot(rwfFit, ylim = c(0, 2000)) # omitting ylim causes the next line of code to extend beyond the plot's bounds
lines(plastics, col = "green")

### I'm not quite sure how to do this.

## 3 ##

# Figure 6.13 shows the result of decomposing the number of persons in the civilian labor force in Australia each month from February 1978 to August 1995.

# a.	Write about 3–5 sentences describing the results of the seasonal adjustment. Pay particular attention to the scales of the graphs in making your interpretation.

### The data and trend plots show an upward trend. The data has a noticable dip in the early 1990s.
### The seasonal plot shows that there is seasonality, which seems to increase in strength as the years progress.

# b.	Is the recession of 1991/1992 visible in the estimated components?

### Yes, the recession is clearly visible in the data, trend, and remainder plots.


#### SCRAPPED CODE ####

# nrow = 12 month * 7 years = 84 rows

dummy_fest_mat <- matrix(0, nrow=84, ncol=1)
for(h in 1:84)
  if(h%%12 == 3)   # this loop builds a vector of length 84 with
    dummy_fest_mat[h,1] = 1   # 1 corresponding to each month March
dummy_fest_mat[3,1] = 0 # festival started one year later

surfing_festival_dummy <- ts(dummy_fest_mat, freq = 12, start=c(1988,1))
f

fit <- tslm(log_fancy + log_fancyD$trend + seasonaldummy(log_fancyD$seasonal))
summary(fit)

log_fancyD <- decompose(log_fancy)

log_fancyD

beer2 <- window(ausbeer,start=1992,end=2006-.1)
fit <- tslm(beer2 ~ trend + season)
summary(fit)

