#housekeeping
library(pacman)
pacman::p_load(rio, kernlab, kknn)

#import dataset (without header)
data <- (import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 1/credit_card_data.txt"))

#previewing data 
head(data)

#call ksvm
other_kern <- list("rbfdot","laplacedot","polydot","tanhdot","besseldot","anovadot","splinedot")
for (k in other_kern) {
  model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), 
                type="C-svc",
                kernel=k,
                C=100, 
                scaled=TRUE)
  
  #prediction
  pred <- predict(model,data[,1:10])
  
  # accuracy 
  acc = sum(pred == data[,11]) / nrow(data)
  print(paste0(k, "=", acc))
  
}
