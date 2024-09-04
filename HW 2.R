#housekeeping
library(pacman)
pacman::p_load(rio, kknn, datasets,factoextra, ggplot2)

#import dataset (without header)
data <- (import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 1/credit_card_data.txt"))

#randomize data  
set.seed(123)
n <- nrow(data) #number of entries in data set 
randindex <- sample(1:n) #randomize order of indices 

#split randomized indices
aIndexTrain <- randindex[1:round(0.7 * n)] #70% for training
aIndexTest <- randindex[(round(0.7 * n) + 1):n] #30% for test 

#create sets using randomized indices 
trainingSet <- data[aIndexTrain,]
testSet <- data[aIndexTest,]

# Leave one out cross validation to find k value 
loocv<-train.kknn((V11)~., data=trainingSet, kmax=100, scale=T)
#model results
summary(loocv)

#training the accuracy 
predTrain <- rep(0,(nrow(trainingSet))) #vector of 0s for training set predictions

#perform loocv via knn on training set 
for (i in 1:nrow(trainingSet)){ #using loocv best k and kernel
  trainmodel <- kknn((V11)~., trainingSet[-i,], trainingSet[i,],k=48,kernel="optimal", scale = TRUE) 
  predTrain[i]<- as.integer(fitted(trainmodel)+0.5) #rounding to 0 or 1
}
accTrain <- sum(predTrain == trainingSet$V11) / nrow(trainingSet)
print(paste0("Training data accuracy: ", round(accTrain * 100), "%"))


#Testing accuracy from training set 
predTest<- rep(0,(nrow(testSet))) #vector of 0s for test set predictions

#perform same loop on test set
for (i in 1:nrow(testSet)){ 
  testmodel <- kknn((V11)~., testSet[-i,], testSet[i,],k=48,kernel="optimal", scale = TRUE) 
  predTest[i]<- as.integer(fitted(testmodel)+0.5) #rounding to 0 or 1
}
accTest <- sum(predTest == testSet$V11) / nrow(testSet)
print(paste0("Test data accuracy: ", round(accTest * 100), "%"))


#Question 3.2b
set.seed(123)
bRandIndex <- sample(1:n) #randomize data set (n = total entries, initialized prior)
#split random indices into sets
indexTrain <- bRandIndex[1:round(0.7 * n)]
indexVal <- bRandIndex[(round(0.7 * n) + 1):(round(0.85 * n))]
indexTest <- bRandIndex[(round(0.85 * n) + 1):n]
#create actual sets 
bTrain <- data[indexTrain,] #training set 
bVal <- data[indexVal,] #validation set for tuning
bTest <- data[indexTest,] #test set for evaluating 
kmax <- 100
bAccTrain <- rep(0,kmax) # b Accuracy Training 

for (k in 1:kmax) {
  #fit knn on training set, validate on val set
  knntrainmodel <- kknn(V11~., bTrain, bVal, k = k, scale = TRUE)
  #compare each k result using val set 
  bPredTrain <- as.integer(fitted(knntrainmodel)+0.5) # round to 0 or 1
  bAccTrain[k] <- sum(bPredTrain == bVal$V11) / nrow(bVal)
}
#accuracy for each k
for (k in 1:kmax) {
  print(paste0("k = ", k, ": Validation Accuracy = ", round(bAccTrain[k] * 100), "%"))
}

#find best K value
bestk <- which.max(bAccTrain)
#print best k value 
print(paste0("Best k = ", bestk, " with Validation Accuracy = ", round(bAccTrain[bestk] * 100), "%"))

#test data (test acc of training set)
set.seed(123)
knntestmodel <- kknn(V11~., bTrain, bTest, k = bestk, scale = TRUE)
bPredTest <- as.integer((fitted(knntestmodel) + 0.5)) #round to 0 or 1 for binary comparison 
bAccTest <- sum(bPredTest == bTest$V11) / nrow(bTest)
print(paste0("Test data using best k, k = ", bestk, " yields accuracy = ", round(bAccTest * 100), "%"))


#Question 4.2
irisData <- iris #load data into new variable 

#convert species (response variable) to number
#we will use the numerical value to calc accuracy
mapping <- c("setosa" = 1, "versicolor" = 2, "virginica" = 3) 
irisData$Species <- mapping[irisData$Species]

#initialize function that performs kmean and return wss
performKMeans <- function(data, k) {
  kmeans(data, centers = k, nstart = 25)$tot.withinss
}

#elbow method/WSS used to find optimal k
fviz_nbclust(irisData[-5], kmeans, method = "wss")
optimalK <- 3

#combo of predictors
ComboList <- list(
  c("Sepal.Length", "Sepal.Width"),
  c("Sepal.Length", "Petal.Length"),
  c("Sepal.Length", "Petal.Width"),
  c("Sepal.Width", "Petal.Length"),
  c("Sepal.Width", "Petal.Width"),
  c("Petal.Length", "Petal.Width"),
  c("Sepal.Length", "Sepal.Width", "Petal.Length"),
  c("Sepal.Length", "Sepal.Width", "Petal.Width"),
  c("Sepal.Length", "Petal.Length", "Petal.Width"),
  c("Sepal.Width", "Petal.Length", "Petal.Width"),
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

#create list for results(wss)
results <- list()

#test each combo
for (predictors in ComboList) {
  subsetData <- irisData[, predictors]
  wssValues <- performKMeans(subsetData, optimalK)
  
  #store wss for each combo in results
  results[[paste(predictors, collapse = ", ")]] <- wssValues
}

#prints results
print(results)

#manually choose best predictors 
bestpredictors <- c("Sepal.Width", "Petal.Width", "Petal.Length")
irisFiltered <- irisData[, bestpredictors]

#reproducibility
set.seed(123)
#kmeans  with optimal k
kMeansCluster <- kmeans(irisFiltered, centers = optimalK, nstart = 25)
#create and print plot
plot1 <- fviz_cluster(kMeansCluster, geom = "point", data = irisFiltered) + ggtitle("k = 3")
print(plot1)

#clustering info
str(kMeansCluster)
#store cluster assignments
predictedClusters <- kMeansCluster$cluster

#create mapping of clusters to species based on majority
#create contingency table of cluster assignments to species
clusterToSpecies <- table(predictedClusters, irisData$Species)
print(clusterToSpecies)

#find most common species in a cluster
clusterSpeciesMapping <- apply(clusterToSpecies, 1, which.max)
#map predicted clusters to species
predictedSpecies <- clusterSpeciesMapping[predictedClusters]
#calculate accuracy
trueSpecies <- irisData$Species
accuracy <- sum(predictedSpecies == trueSpecies) / length(trueSpecies)
#print accuracy
print(paste0(accuracy, " or ", round(accuracy * 100), "%"))
