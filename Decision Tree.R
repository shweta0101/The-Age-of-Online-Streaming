#Decision Tree

#Importing libraries
library(rpart)        #used for DT
library(rpart.plot)
library(ggplot2)
library(caret)
library(caTools)      #used for sample.split() function
library(e1071)

#Importing dataset
mydata <- read.csv(file.choose() , header = T)
str(mydata)
summary(mydata)

#Converting To Factor
mydata$Future_Pay = factor(mydata$Future_Pay)

#Checking For Missing Value
colSums(is.na(mydata))

#Data Partion
set.seed(1234)
sample = sample.split(mydata$Future_Pay, SplitRatio = 0.70)
train = subset(mydata, sample == TRUE)
test = subset(mydata, sample == FALSE)

#Training Data
tree <- rpart(Future_Pay ~., train, control = rpart.control(minsplit = 50,
                                                            xval = 10,
                                                            maxdepth = 5))
summary(tree)

#Prediction for train
p1 = predict(tree, train , type = 'class')

#Evaluating for train
confusionMatrix(p1, train$Future_Pay)

#Predicting for test
pred = predict(tree, test, type = 'class')

#Evaluating for test
confusionMatrix(pred, test$Future_Pay)

#Visualizing

#Method 1
prp(tree)

#Method 2
plot(tree)
text(tree)

#Method 3
rpart.plot(tree)

