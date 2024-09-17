#housekeeping
library(pacman)
pacman::p_load(rio,outliers, ggplot2, tidyverse, tidyr, lubridate, repr, reshape)

#Question 5.1
#load data
crimeData <- import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 3/uscrime.txt")
head(crimeData) #preview of data 
summary(crimeData$Crime) #summary of the last column (crime rate: number of offenses per 100,000 population in 1960)


#visualization (box and whisker)
options(repr.plot.width=2, repr.plot.height=5) #height and width of plot
ggplot(crimeData, aes(x="",y=Crime))+ #use crime dataset for plot, y-axis is the crime, x-axis empty
  geom_boxplot(width=0.2,outlier.color="maroon",outlier.shape = 1, color = "purple")+ #initialize boxplot + color and shape
  labs(title="Number of offenses per 100,000 population in 1960")+ #title
  theme(plot.title = element_text(hjust=0.5,size=12)) #center title, change font

#grubbs
#?grubbs.test
grubbs.test(crimeData$Crime) #default(type 10) checks single outlier either min or max)

#Question 6.1


#Question 6.2a
temps <- import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 3/temps.txt")

avgTempDate <- rowMeans(temps[c(2:length(temps))], dims=1, na.rm=T) #average temp for each day across the years in the txt
avgMean <- mean(avgTempDate) #mean temp of the avg time series
tempDev <- avgTempDate - avgMean #deviations for each day
C <- 4 #slack/allowance value 
adjDev <- tempDev - C #adjusting deviation = tweaking sensitivity to detect significant change
cusum <- numeric(length(adjDev) + 1) #empty vector, extra 0 for loop (i+1)
for (i in 1:length(adjDev)) #iterate through each day, check cusum, update index with appropriate value
{
  checker <- cusum[i] + adjDev[i] #if check is positive, it is added to cusum, otherwise reset to 0
  ifelse(checker > 0, cusum[i+1] <- checker, cusum[i+1] <- 0) 
}
plot(cusum)

maxIndex <- which.max(cusum) #peak of cusum
temps[maxIndex, 1]  #the corresponding day 
#which cusum is higher than 85 (threshold set based on graph)
which(cusum >= 85)
temps[56, 1]
temps[58, 1]
temps[59, 1]
temps[60, 1]
temps[61, 1]
#peak + downward trend

#Question 6.2b


