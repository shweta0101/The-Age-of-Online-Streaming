#Logistic Regression

#Importing Dataset
mydata = read.csv(file.choose(), header = T)
str(mydata)
summary(mydata)

#Converting to Factor Variables
mydata$Gender = as.factor(mydata$Gender)
mydata$Age = as.factor(mydata$Age)
mydata$Employment = as.factor(mydata$Employment)
mydata$Education = as.factor(mydata$Education)
mydata$Income = as.factor(mydata$Income)
mydata$Subscription_Charges = as.factor(mydata$Subscription_Charges)
mydata$Censoring = as.factor(mydata$Censoring)
mydata$WiFi = as.factor(mydata$WiFi)
mydata$Watch_Time = as.factor(mydata$Watch_Time)
mydata$Future_Pay = as.factor(mydata$Future_Pay)

#Checking Missing Values
colSums(is.na(mydata))

# Two-way table of factor variables
xtabs(~Future_Pay + Gender , data = mydata)
xtabs(~Future_Pay + Age , data = mydata)
xtabs(~Future_Pay + Employment , data = mydata)
xtabs(~Future_Pay + Education , data = mydata)
xtabs(~Future_Pay + Income , data = mydata)
xtabs(~Future_Pay + Subscription_Charges , data = mydata)
xtabs(~Future_Pay + Censoring , data = mydata)
xtabs(~Future_Pay + WiFi , data = mydata)
xtabs(~Future_Pay + Watch_Time , data = mydata)

# Partition Data - Train (80%) & Test (20%)
set.seed(1234)
ind = sample(2, nrow(mydata), replace = TRUE , prob = c(0.8,0.2))
train = mydata[ind==1,]
test = mydata[ind==2,]

#Drawing Logistic Graph: Sigmoid Graph

predicted.data = data.frame(Probability.of.Future_Pay=mymodel$fitted.values,
                            Future_Pay=train$Future_Pay)

predicted.data = predicted.data[order(predicted.data$Probability.of.Future_Pay,
                                      decreasing = FALSE),]

predicted.data$rank = 1:nrow(predicted.data)

library(ggplot2)

ggplot(data=predicted.data, aes(x=rank, y=Probability.of.Future_Pay)) +
  geom_point(aes(color=Future_Pay), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability whether a customer will pay") +
  ggtitle("Sigmoid Curve")

# Logistic regression model
mymodel = glm(Future_Pay ~., data = train , family = "binomial")
summary(mymodel)

#Prediction for train
p1 = predict(mymodel, train , type = "response")

#Misclassificaion error - train data
pred1 = ifelse(p1>0.5 , 1, 0)
tab1 = table(Predicted = pred1 , Actual = train$Future_Pay)
tab1
1 - sum(diag(tab1)/sum(tab1))

#Prediction for test
p2 = predict(mymodel , test , type = 'response')

#Misclassification error - test data
pred2 = ifelse(p2>0.5, 1 , 0)
tab2 = table(Predicted = pred2 , Actual = test$Future_Pay)
tab2
1 - sum(diag(tab2)/sum(tab2))

#StepWise Regression
stp = step(mymodel, direction = "both")
summary(stp)

#Mulicollinearity
library(car)
vif(stp)

#Goodness-of-fit test/Residual Chi-Square
with(mymodel , pchisq(null.deviance - deviance , df.null-df.residual, 
                      lower.tail = F))

#Hosmer and Lemshow GOODNESS OF FIT TEST
library(ResourceSelection)
hoslem.test(train$Future_Pay, fitted(stp), g=10)

#Wald's Test
library(survey)

regTermTest(stp, "Gender")
regTermTest(stp,"Age")
regTermTest(stp, "Employment")
regTermTest(stp, "Education")
regTermTest(stp, "Income")
regTermTest(stp,"Subscription_Charges")
regTermTest(stp,"Censoring")
regTermTest(stp,"WiFi")
regTermTest(stp,"Watch_Time")


#ROC Curve
library(InformationValue)
plotROC(actuals = train$Future_Pay , predictedScores = as.numeric(fitted(stp)))

#Kolmogrov Smirnov Chart
ks_plot(actuals = train$Future_Pay, predictedScores = p1)
