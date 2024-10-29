#Question 8.1


#Question 8.2
#housekeeping
library(pacman)
pacman::p_load(rio, stats, DAAG)

#import data
data <- import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 5/uscrime.txt")
#swap column to fit formula (crime is first column)
orData <- data[c(16, 1:15)]

#subset pred. plot the predictors/response 
pred = orData[-1] #predictor variables 
headers = list(
  "M",
  "So",
  "Ed",
  "Po1",
  "Po2",
  "LF",
  "M.F",
  "Pop",
  "NW",
  "U1",
  "U2",
  "Wealth",
  "Ineq",
  "Prob",
  "Time"
)
par(mfrow = c(4, 4)) #15 plots for each variable (4x4)
for (i in 1:15) {
  plot(pred[, i], orData$Crime, xlab = headers[i])
}

#dataframe with pre-defined values of the socio-economic factors 
cityData = data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)

#fitting first linear regression model 
formula1 <- formula(orData)
model1 <- lm(formula1, orData)
summary(model1) #focusing on r^2, pvalue, and coeff for predictors 
coef1 <- model1$coefficients #coeff (beta values)
par(mfrow = c(2, 2))
plot(model1) #diagnostic plots to assess fit 
crimePred <- predict.lm(model1, cityData) #use model to predict crime rate on predefined data (cityData)
crimePred

#filter pred by p-value
pvalue <- summary(model1)$coefficients[, 4] #extract p value from summary 
coef <- model1$coefficients
orDataFitted <- orData[1]
n <- 2
for (i in 2:16) {
  if (pvalue[i] < 0.08) { #remove if pvalue if > 0.08, keeping U2 and Po1
    orDataFitted[n] = orData[i]
    n = n + 1
  }
}

#fitting second linear regression model (filtered pred)
formula2 <- formula(orDataFitted) #second formula using filtered pred 
model2 <- lm(formula2, orDataFitted)
summary(model2)
plot(model2)
crimePredAdj <- predict.lm(model2, cityData)
crimePredAdj

#cross validate on both models 
par(mfrow = c(1, 1))
CrossVal1 <- cv.lm(orData, model1, m = 5)
CrossVal2 <- cv.lm(orData, model2, m = 5)
#compare R^2 values of CV models
TotalSumSquare <- sum((orData$Crime - mean(orData$Crime)) ^ 2) #Total SS for Crime variable 
SumSquare1 <- attr(CrossVal1, "ms") * nrow(orData) # ss for cv model 1
SumSquare2 <- attr(CrossVal2, "ms") * nrow(orData) # ss for cv model 2
RSquareCV1 = 1 - SumSquare1 / TotalSumSquare # r^2 cv model 1
RSquareCV2 = 1 - SumSquare2 / TotalSumSquare # r^2 cv model 2
RSquareCV1
RSquareCV2