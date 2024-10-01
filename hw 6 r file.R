#housekeeping
library(pacman)
pacman::p_load(rio, stats, pls, DAAG)

set.seed(123)
#import data
data <- import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 5/uscrime.txt")
#swap column to fit formula (so that crime is first column)
orData <- data[c(16, 1:15)]
pred = orData[-1] #predictors 
crime = orData[1]
PCA = prcomp(~ ., pred, scale = TRUE)
summary(PCA)
attributes(PCA)

biplot(PCA, scale = 0)

screeplot(PCA, xlab = 'PCs')
var = PCA$sdev ^ 2 #variance
propVar = var / sum(var) #proportion of var
plot(propVar) #plots prop vs the PC number

x = 7 # number of pc

PCs = PCA$x[, 1:x]
PCdata = cbind(crime, PCs)

model = lm(Crime ~ ., PCdata)
summary(model)

variables = data.frame(
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

model$coefficients #coeff in pca
betas = model$coefficients[-1]
beta0 = model$coefficients[1]


#convert the coefficients back into the de-scaled space
alphas = PCA$rotation[, 1:x] %*% betas

p_mean = sapply(pred, mean)
p_sd = sapply(pred, sd)
a_orig = alphas / p_sd
a_orig  #de-scaled coefficients

a0 = beta0 - sum(alphas * p_mean / p_sd)
a0 #de-scaled intercept

prediction = a0 + sum(a_orig * variables)  #equation of regression line
prediction


PClist = as.data.frame(PCA$x[, 1:x])
PCcv = cbind(crime, PClist)
model2 = lm(Crime ~ ., PCcv)
cv = cv.lm(PCcv, model2, m = 5)
mn = mean(crime[, 1])
R2 = 1 - attr(cv, "ms") * nrow(orData) / sum((crime - mn) ^ 2)
R2
