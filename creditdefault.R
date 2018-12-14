#########################
## Logistic regression ##
#########################

##############
## Packages ##
##############

#if(!require("")) install.packages(""); library("")
#General package for time series
if(!require("ISLR")) install.packages("ISLR"); library("ISLR")
if(!require("ggvis")) install.packages("ggvis"); library("ggvis")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("boot")) install.packages("boot"); library("boot")
if(!require("caTools")) install.packages("caTools"); library("caTools")
if(!require("ROCR")) install.packages("ROCR"); library("ROCR")

#Original sample 

set.seed(123)

#Information on the data
str(Default)
#income
#student
#balance 
#default

#Information about data 
summary(Default)

#We first plot balance and income against the default type
#no difference for income
#large difference in 
par(mfrow=c(1,2))
boxplot(balance~default, data = Default, col= c("pink", "blue"), main= "Balance per default")
boxplot(income~default, data = Default, col= c("pink", "blue"), main="Income per default")

#We randomly split the data to a train and test set
set.seed(123)
randomtrain     = sample.split(Default$default, 0.7)
train           = subset(Default, randomtrain == TRUE)
test            = subset(Default, randomtrain == FALSE)

#Boxplot as previous, just for training data, looks alike
par(mfrow=c(1,2))
boxplot(balance~default, data = train, col= c("pink", "blue"), main= "Balance per default train")
boxplot(income~default, data = train, col= c("pink", "blue"), main="Income per default train")


#Convert factor variables to binary variable-target
class(Default$default)
levels(Default$default) = c("0","1")
Default$default = as.numeric(levels(Default$default))[Default$default]
str(Default$default)

str(train$default)
levels(train$default) = c("0","1")
train$default = as.numeric(levels(train$default))[train$default]
str(train$default)



#Model estimation
#We estimate now our logit on the train data
logit       = glm(default~., family = "binomial", data = train)

#We see that only balance is significant
summary(logit) #only balance
#In the estimated model only one parameter is significant. We should reformulate the model

#Model correction as only balance is a significant variable 
logit2      = glm(default~balance, family = "binomial", data = train)
estimated   = predict(logit2, newdata = train, type= "response") #just for the purpose of drawing


#PLOT DATA
#See sigmoid curve!
par(mfrow=c(1,1))
plotdata = data.frame(balance=seq(min(Default$balance), max(Default$balance),len=100))
plotdata$default = predict(logit2, newdata = plotdata, type = "response")
plot(default~balance, data=Default, col="grey", main="Probability of default")
lines(default~balance, data=plotdata, col="pink", lwd=2)

test$prrisk = predict(logit2, newdata = test, type= "response")

tabelle = table(test$default, as.numeric(test$prrisk) >= 0.5)

matricax = as.matrix(tabelle)
dim(matrica)

###Create function for Accuracy, since we will need to use it again
accuracy = function(matrica){
 (matrica[1,1] + matrica[2,2])/(matrica[1,1] + matrica[2,2]+matrica[1,2]+ matrica[2,2])
}

#accuracy 
accuracy(matricax)
# 0.9733333 extremely high, but might be due to the fact that most do not default
#AREA UNDER THE CURVE

### According to econometric theory n(1-\bar{y}) potentially mispecified
#number of potentionally errous predictions
(nwrong =length(default)*(1-mean(default)))
#1 9667

#almost all !!!


####ROC
#Need ROC because of unequal balance of cases
#####AREA UNDER THE CURVE


#AREA UNDER THE CURVE
rocf = prediction(test$prrisk, test$default)
as.numeric(performance(rocf, "auc")@y.values)
#0.9477069

# Make predictions on training set
predictTrain = predict(logit, type="response")

# Prediction function
ROCRpred = prediction(predictTrain, train$default)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="Area under the curve")

#accuracy under new cutoff
tabelle = table(test$default, as.numeric(test$prrisk) >= 0.07)

matricanew = as.matrix(tabelle)

#sensitivity
(sensitivity = 81/(81+19)) #very high-total TP over all positives
(specificity = 2679/(2679 +221)) #true negatives over all negatives

#accuracy 
accuracy(matricanew) 
# 0.9006667 
#lower but still very high !
