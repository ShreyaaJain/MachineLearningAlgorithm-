install.packages("aod")
library(aod)
  
setwd("/Users/sjain/Downloads")

concrete <- readWorksheetFromFile("/Users/sjain/Shreya/BigDataEng/Concrete_Data.xlsx", sheet = 1, header = TRUE)
str(concrete)
summary(concrete)
sapply(concrete, sd)

test_index <- sample(1000, 100)
test_dataset <- concrete[test_index,]
train_dataset <- concrete[-test_index,]

lmfit <- lm( strength ~ Cement + Blast + FlyAsh + Water +  
               Superplasticizer + CoarseAggregate + FineAggregate + 
               Age, data = train_dataset)

lmfit1 <- lm( strength ~ Cement + Blast + FlyAsh + Water +  
                Superplasticizer + CoarseAggregate + FineAggregate + 
                Age, data = test_dataset)

summary(lmfit)


anova(lmfit)

par(mfrow=c(2,2)) 
plot(lmfit)

fitted(lmfit)
anova(lmfit,lmfit1)

# K-fold cross-validation
install.packages("DAAG")
library(DAAG)
??cv.lm()
cv.lm(data = train_dataset, lmfit, m=2) # 3 fold cross-validation
cv.lm(data = test_dataset, lmfit1, m=2)


install.packages("MASS")
library(MASS)
step <- stepAIC(lmfit, direction="both")

step$anova # display results


install.packages("leaps")
library(leaps)
require(leaps)
##### Searching all subset models up to size number of variables
regfit.full=regsubsets (strength ~. ,data=train_dataset ,nvmax=9,really.big = TRUE)
exhaustiveRegSummary =summary(regfit.full)
print(exhaustiveRegSummary)
names(exhaustiveRegSummary)
exhaustiveRegSummary$rss
exhaustiveRegSummary$adjr2


## Plotting and choosing the subset
par(mfrow=c(2,2))
plot(exhaustiveRegSummary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(exhaustiveRegSummary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")

#### Forward selection
regfit.fwd=regsubsets(strength ~. ,data=train_dataset ,nvmax=8, method="forward") 
forwardSearchSummary=summary(regfit.fwd)
names(forwardSearchSummary)
forwardSearchSummary
forwardSearchSummary$rss
forwardSearchSummary$adjr2
coef(regfit.fwd,5)


#Measures of predictive accuracy
install.packages("forecast")
library(forecast)
pred = predict(lmfit, test_dataset)

performanceMetrics <- accuracy(pred, train_dataset$strength)
head(performanceMetrics)

#creating a transpose to swap row and column which creates performance metrics like MAE, MAPE, RMSE
t(performanceMetrics)


# Evaluate regression model
res <- residuals(lmfit)
plot(jitter(res)~jitter(train_dataset$strength), ylab="Residuals", xlab="Strength", data=train_dataset)
abline(0,0)

plot(train_dataset$strength, res, ylab="Residuals", xlab="Strength", main="Evaluation") 
abline(98.0054, 0.9528)


            