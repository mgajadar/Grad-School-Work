#housekeeping
library(pacman)
pacman::p_load(rio, kernlab, kknn, rmarkdown, pandoc, knitr)

#import dataset (without header)
data <- (import("D:/Users/Marcus/Desktop/grad school/FALL 2024/Analytic Modeling/hw 1/credit_card_data.txt"))

#previewing data 
head(data)

#call ksvm
model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), 
              type="C-svc",
              kernel="vanilladot",
              C=100, 
              scaled=TRUE)

#calculate a1...am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a

#calculate a0
a0 <- -model@b
a0

#prediction model
pred <-predict(model, data[,1:10])
pred

#model prediction vs actual classification
sum(pred ==  data[,11]) / nrow(data)


