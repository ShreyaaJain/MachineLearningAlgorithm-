# Loading neccessary packages and dataset for classification. 
#The data that I am  working on is GermanCredit. Dataset is the created data frame for German Credit.
# Summary gives the summarization of the data. It gices the min,mean,median,max for all the columns in the dataframe.
install.packages("caret")
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit
summary(dataset)  

# Str defines the structure of the data. It tells us the datatype of all the columns in the data frame.
# Use scale to change the datatype from int to number. 1:7 - This is done to change the datatype for the columns from 1 to 7
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)

# Divide the dataset into training and test dataset in the proportion of 10:2 
# with train data containing 1000 columns and the remaining in the test.
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]

# Trying SVM function to test which model gives the best confusion matrix. By changing the cost and gamma we can tune the data better. 
# It looks like the model with  kernel = linear, cost 10 and gamma 0.1 gives the better confusion matrix
model = svm(Class ~ ., kernel = 'linear', cost = 10, gamma = 0.01, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'linear', cost = 10, gamma = 0.1, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'linear', cost = 1, gamma = 0.1, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'linear', cost = 20, gamma = 0.5, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'linear', cost = 10, gamma = 0.2, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'polynomial', cost = 10, gamma = 0.01, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = 'linear', degree = 3, cost = 1, gamma = 0.1, data = train_dateset, scale = F)

# Prediction with the test data. redication 
predictions <-  predict(model, test_dateset[-10])
# Creates the confusion matrix result of prediction, using command table to compare the result of SVM prediction 
# It tells that the model gives 23 results for bad which are bad, 37 were predicted good which were bad, 
# 15 were predicted as good for bad and 125 were predicted right ie good for good
table(test_dateset[,10], predictions)

