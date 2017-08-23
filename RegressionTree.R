library(XLConnect)
setwd("/Users/sjain/Downloads")

concrete <- readWorksheetFromFile("/Users/sjain/Shreya/BigDataEng/Concrete_Data.xlsx", sheet = 1, header = TRUE)
str(concrete)
summary(concrete)

sample_size <- floor(0.80 * nrow(concrete))
set.seed(111)
test_index <- sample(seq_len(nrow(concrete)), size = sample_size)

train_dataset <- concrete[test_index,]
test_dataset <- concrete[-test_index,]

lmfit <- rpart( strength ~ Cement + Blast + FlyAsh + Water + Superplasticizer + CoarseAggregate + FineAggregate + Age,
                method ="anova", data = train_dataset, control = rpart.control(cp = 0.05))
printcp(lmfit)
plotcp(lmfit)
summary(lmfit)

#create additional plots
par(mfrow=c(1,2)) #two plots on one page
rsq.rpart(lmfit) # visualize cross-validation results

#plot tree
plot(lmfit, uniform=TRUE, main="Regression tree for Strength")
text(lmfit, use.n=TRUE, all=TRUE,digits = 3, cex=.8)

lmfit2 <- rpart( strength ~ Cement + Blast + FlyAsh + Water + Superplasticizer + CoarseAggregate + FineAggregate + Age,
                 method ="anova", data = test_dataset, control = rpart.control(cp = 0.05))
summary(dt2)

plot(lmfit2,uniform = T,compress = T, margin = 0.2, branch = 0.3)

text(lmfit2, use.n = T, digits = 3,cex = 0.6)

labels(lmfit2)

pred <- predict(lmfit2, test_dataset, type = 'matrix') 
head(pred)

?tree
library(tree)
require(tree)
treefit = tree(strength ~. ,data=train_dataset)
treefit1 = tree(strength ~ Cement+Age ,data=train_dataset)
plot(treefit)
text(treefit,cex=0.75)
?quantile
?cut
strengthVal = quantile(train_dataset$strength,0:10/10)
cut.strength = cut(train_dataset$strength,strengthVal,include.lowest=TRUE)
plot(train_dataset$Cement,train_dataset$Age,col=grey(10:2/11)[cut.strength],pch=20,
     xlab="cement",ylab="age")
partition.tree(treefit1,ordvars=c("Cement","Age"),add=TRUE)
summary(treefit)

#cleaner tree
treefit2 <- tree(strength ~Cement+Age, data=train_dataset)
plot(train_dataset$Cement,train_dataset$Age,col=grey(10:2/11)[cut.strength],pch=20,
     xlab="cement",ylab="age")
partition.tree(treefit3,ordvars=c("Cement","Age"),add=TRUE,cex=0.3)
plot(treefit3)
text(treefit3,cex=0.5,digits=3)



#using rpart
tree1 <- rpart(strength~ ., control = rpart.control( minsplit = 50, maxdepth = 5), data=train_dataset)

#Minimum error
min.xerror <- tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]
tree1.pruned <- prune(tree1,cp = min.xerror)
plot(tree1.pruned)
text(tree1.pruned)
# Evaluate on test data
test.pred.tree1 <- predict(tree1.pruned,test_dataset)
#Gives the MRSE value
RMSE.tree1.pruned <- sqrt(mean((test.pred.tree1-test_dataset$strength)^2))
RMSE.tree1.pruned
#Gives the MAE value
MAE.tree1.pruned <- mean(abs(test.pred.tree1-test_dataset$strength))
MAE.tree1.pruned


