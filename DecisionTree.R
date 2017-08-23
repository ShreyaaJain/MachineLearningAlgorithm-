#install.packages("caret")
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit
summary(dataset) 

table(dataset$Class)
sample_size <- floor(0.850 * nrow(dataset))
set.seed(111)
test_index <- sample(seq_len(nrow(dataset)), size = sample_size)

train_dataset <- dataset[test_index,]
test_dataset <- dataset[-test_index,]

library(rpart)
fit <- rpart(Class~ .,method = "class", data=train_dataset)
summary(fit)

str(dataset)
View(dataset)
printcp(fit) #display the results
plotcp(fit) #visualize cross-validation results
summary(fit) # detailed summary of splits

#create additional plots
par(mfrow=c(1,2)) #two plots on one page
rsq.rpart(fit) # visualize cross-validation results

#plot tree
plot(fit, uniform=TRUE, main="Decision tree for German Credit")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


fit2 <- rpart(Class~ ., control = rpart.control( minsplit = 50,maxdepth = 5),data=test_dataset)
summary(fit2)

plot(fit2,uniform = T,compress = T, margin = 0.2, branch = 0.3)
text(fit2, use.n = T, digits = 3,cex = 0.6)
labels(fit2)

pred <- predict(fit, test_dataset, type = 'class') 
head(pred)
table(pred, test_dataset$Class)


