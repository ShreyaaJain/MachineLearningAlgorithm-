setwd("~/Shreya/BigDataEng/midterm")

insurance <- read.table("result_train.csv", 
                  sep=",", 
                  header = TRUE, 
                  quote = "",
                  stringsAsFactors = FALSE ) 

test_data <- read.table("result_test.csv", 
                        sep=",", 
                        header = TRUE, 
                        quote = "",
                        stringsAsFactors = FALSE ) 


install.packages("e1071")
library(e1071)

lmfit <- svm(insurance$Response ~ ., data = insurance )
print(lmfit)
summary(lmfit)
head(lmfit)

predictedY <- predict(lmfit, insurance)
head(predictedY)
table(insurance, predictedY)

library(MASS)

tuneResult <- tune(svm, Response~., data = insurance,  cost = 2^(2:9))
print(tuneResult)
plot(tuneResult)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- insurance$Response - predictedY
PredictionRMSE <- rmse(error)

predict1 <- predict(lmfit ,test_data)
print(predict1)
install.packages("Metrics")
library(Metrics)
ScoreQuadraticWeightedKappa(test_data$Response, round(predict1))

