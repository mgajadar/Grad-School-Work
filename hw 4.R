#Question 7.1


#Question 7.2

#housekeeping
library(pacman)
pacman::p_load(rio, ggplot2, ggfortify, tidyverse, reshape, survMisc, forecast, lubridate)


temps <- import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 3/temps.txt")
head(temps) #preview data

#turn data into time series object
temp_ts<-ts(as.vector(unlist(temps[,2:21])),start=1996,frequency=123)
autoplot(temp_ts) +
  ggtitle("Time Series Plot of Daily High Temperatures") +
  xlab("Time") +
  ylab("Temperature")

set.seed(123) #reproducibility 

#holtwinter additive
hw_add<- HoltWinters(temp_ts,seasonal="additive")
autoplot(hw_add) +
  ggtitle("Holt-Winters Additive") +
  xlab("Time") +
  ylab("Temperature")

#holtwinter multiplicative 
hw_mul<- HoltWinters(temp_ts, seasonal="multiplicative")
autoplot(hw_mul) +
  ggtitle("Holt-Winters Multiplicative") +
  xlab("Time") +
  ylab("Temperature")

#hw add results (greeks)
print("Additive method:")
print(paste("\tBase factor (Alpha):", hw_add$alpha))
print(paste("\tTrend factor (Beta):", hw_add$beta))
print(paste("\tSeasonal factor (Gamma):", hw_add$gamma))
print(paste("\tSum of Squared Errors (SSE):", hw_add$SSE))

#hw mult results (greeks)
print("For multiplicative method:")
print(paste("\tBase factor (Alpha):", hw_mul$alpha))
print(paste("\tTrend factor (Beta):", hw_mul$beta))
print(paste("\tSeasonal factor (Gamma):", hw_mul$gamma))
print(paste("\tSum of Squared Errors (SSE):", hw_mul$SSE))

#visualize fitted values
par(mfrow=c(1,2)) #set up plotting area
plot(fitted(hw_add))
plot(fitted(hw_mul))

hw_add$fitted[1:5,] #first 5 rows of fitted values 
