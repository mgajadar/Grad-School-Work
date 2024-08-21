#housekeeping
library(pacman)
pacman::p_load(rio, kernlab, kknn)

#import dataset (without header)
data <- (import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 1/credit_card_data.txt"))

#previewing data 
head(data)

acc_knn = function(X){
  pred <- rep(0,(nrow(data))) # initialize vector of 0s
  
  for (i in 1:nrow(data)){
    
    # data[-i] means remove row i of the data when finding NN. 
    # using scaled data
    
    model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,data[-i,],data[i,],k=X, scale = TRUE)
    
    #for rounding to 0 or 1
    pred[i] <- as.integer(fitted(model)+0.5)
  }
  
    #calculation for accuracy 
    acc = sum(pred == data[,11]) / nrow(data)
    return(acc)
}


#initalize vector of 30 0s
knn_test <- rep(0,30)  
for (X in 1:30){
  knn_test[X] = acc_knn(X) #tests knn with x neighbors
  
}

knn_test