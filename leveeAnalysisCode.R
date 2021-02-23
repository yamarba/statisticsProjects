#DATA EXPLORATION

#dataset name: levee

str(levee)
levee$Fail <-as.factor(levee$Fail)
levee$Seds <-as.factor(levee$Seds)
levee$B.Pit <-as.factor(levee$B.Pit)
levee$Mea <-as.factor(levee$Mea)
levee$L.Cover <-as.factor(levee$L.Cover)
levee$Rev <-as.factor(levee$Rev)

head(leveee)
tail(leveee)

levee$Fail<-ifelse(test = 0, yes = "not fail", no = "fail")
xtabs( ~ Fail + Seds, data=levee)
xtabs( ~ Fail + B.Pit, data=levee)
xtabs( ~ Fail + Mea, data=levee)
xtabs( ~ Fail + L.Cover, data=levee)
xtabs( ~ Fail + Rev, data=levee)

ggplot(aes(x=F.Width,y=V.Width),data=levee) + geom_point()
ggplot(aes(x=F.Width,y=Dredge),data=levee) + geom_point()
ggplot(aes(x=R.Mile,y=Dredge),data=levee) + geom_point()
ggplot(aes(x=C.Width,y=Dredge),data=levee) + geom_point()
ggplot(aes(x=F.Width,y=R.Mile),data=levee) + geom_point()
ggplot(aes(x=V.Width,y=Dredge),data=levee) + geom_point()
ggplot(aes(x=R.Mile,Width,y=Sin),data=levee) + geom_point()                                           
ggplot(aes(x=R.Mile,y=C.Factor),data=levee) + geom_point()                                        
ggplot(aes(x=R.Mile,y=C.Factor),data=levee) +  geom_point()                                          
ggplot(aes(x=V.Width,y=C.Factor),data=levee) + geom_point()
ggplot(aes(x=C.Width,y=C.Factor),data=levee) +  geom_point()                                         
ggplot(aes(x=F.Width,y=C.Factor),data=levee) + geom_point()                                        
ggplot(aes(x=Dredge,y=C.Factor),data=levee) + geom_point()                                        
ggplot(aes(x=C.Factor,y=Sin),data=levee) + geom_point()
ggplot(aes(x=F.Width,y=Sin),data=levee) +  geom_point()
ggplot(aes(x=C.Width,y=Sin),data=levee) + geom_point()
ggplot(aes(x=Dredge,y=Sin),data=levee) + geom_point()
ggplot(aes(x=V.Width,y=Sin),data=levee) + geom_point()


glm(formula = Fail ~ Seds, family = "binomial", data = levee)
logistic2 <- glm(Fail ~ ., data=levee, family = "binomial")
summary(logistic2)
BIC(logistic2)

summary(logistic3)
BIC(logistic3)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.fail)) + 
  geom_point(aes(color=Fail), alpha=1, shape=4,stroke=2) + xlab("Index") + 
  ylab("Predicted probability of levee failure")

output.forest1 <- randomForest(fail ~ rmile + seds + bpit + mea + cwidth + 
                                 fwidth + cfactor + lcover + vwidth + sin + dredge  + 
                                 rev, data = leveee)
randomForest::importance(output.forest1)
print(boruta_output)
print(boruta_signif)

logisticModel1<- glm(fail ~ rmile + seds + bpit + mea + cwidth + fwidth + 
                       cfactor + lcover + vwidth + sin + dredge + rev,
                     data = trainingData, family = binomial(link="logit"))

AIC(logisticModel1)
BIC(logisticModel1)

summary(logisticModel2)
BIC(logisticModel2)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.fail)) + 
  geom_point(aes(color=Fail), alpha=1, shape=4,stroke=2) + xlab("Index") + 
  ylab("Predicted probability of levee failure")

mytable1 <- table(levee$Fail, levee$Predict)
  mytable1 #observed vs predicted

mytable1 <- table(levee$fail, leveee$Predict)
   mytable1 #observed vs predicted
  

logistic <- glm(Fail ~ Seds, data=levee, family = "binomial")
summary(logistic)
logistic2 <- glm(Fail ~ ., data=levee, family = "binomial")
summary(logistic2)


#METHOD 1 of logistic Regression
summary(logistic2)
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logistic2$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logistic2$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logistic2$coefficients)-1))

summary(logistic3)
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logistic3$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logistic3$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logistic3$coefficients)-1))

PLOT
predicted.data <-data.frame(probability.of.fail=logistic2$fitted.values, Fail=levee$Fail)
#sort data from low probability to high probabilities.
predicted.data<-predicted.data[order(predicted.data$probability.of.fail, decreasing = FALSE),]
#then we add a new column to the data.frame that has the rank of each sample, from low probability to high probability.
predicted.data$rank <-1:nrow(predicted.data)

library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.fail)) + geom_point(aes(color=Fail), alpha=1, shape=4,stroke=2) + xlab("Index") + ylab("Predicted probability of levee failure")

predicted.data <-data.frame(probability.of.fail=logistic3$fitted.values, Fail=levee$Fail)
#sort data from low probability to high probabilities.
predicted.data<-predicted.data[order(predicted.data$probability.of.fail, decreasing = FALSE),]
#then we add a new column to the data.frame that has the rank of each sample, from low probability to high probability.
predicted.data$rank <-1:nrow(predicted.data)

#confusion matrix
levee$Predict <- ifelse(logistic2$fitted.values >.5, "1", "0")
mytable1 <- table(levee$Fail, levee$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logistic2 and scoring the values back on the 
#original dataset (testData1) for comparison
pred <-predict(logistic2, testData1, "link")
head(pred)
#create a new class prediction column on orignal testData1 dataset 
#for side-by-side comparison 
testData1$class <-ifelse(pred >0, "1", "0")
#look at first 6 and last 6 rows
head(testData1)
tail(testData1)


#METHOD 2 of logistic regression using MACHINE LEARNING

#dataset name: leveee

par(mfrow = c(4,5))
for( i in 1:14){
  hist(leveee[,i], main = colnames(leveee)[i],xlab = colnames(leveee)[i], col = 'darkorchid')
}

colnames(leveee) <- c("fail","yr","rmile","seds","bpit","mea", "cwidth", "fwidth", "cfactor", "lcover", "vwidth", "sin", "dredge", "rev")
# 2 Creating training and test sample sets
#create training set
data1 = leveee[which(leveee$fail == "0"), ]
data2 = leveee[which(leveee$fail == "1"), ]

set.seed(140)
#set training set to 75%
trainingSet1 <- sample(1:nrow(data1), .75*nrow(data1))
trainingSet2 <- sample(1:nrow(data2), .75*nrow(data2))

training_1 <- data1[trainingSet1, ]
training_2 <- data2[trainingSet2, ]

trainingData <- rbind(training_1, training_2)

#create Test Set
test_1 <- data1[-trainingSet1, ]
test_2 <- data2[-trainingSet2, ]

testData <- rbind(test_1, test_2)

# 3 Get predictor values with 2 methods (random forest and Boruta method):
install.packages("randomForest")
install.packages("Boruta")
library(randomForest)
output.forest1 <- randomForest(fail ~ rmile + seds + bpit + mea + cwidth + fwidth + cfactor + lcover + vwidth + sin + dredge + rev,
                               data = leveee)
randomForest::importance(output.forest1)
#Boruta method for determining importance of variables (a great way to visualize this):
library(Boruta)
boruta_output <- Boruta(fail ~ ., data=na.omit(leveee), doTrace=2)
print(boruta_output) #lists important and iunimportant  variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")]) #collect confirmed
print(boruta_signif) #significant variables
#visualize the variables and their importance
par(mfrow = c(1,1))
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="variable importance") #plot variable importance

# 4 build logistic regression models and get predictions
logisticModel1<- glm(fail ~ rmile + seds + bpit + mea + cwidth + fwidth + cfactor + lcover + vwidth + sin + dredge + rev,
                     data = trainingData, family = binomial(link="logit"))
AIC(logisticModel1)
BIC(logisticModel1)
logisticModel2<- glm(fail ~ rmile + seds + mea + vwidth + dredge,
                     data = trainingData, family = binomial(link="logit"))
AIC(logisticModel2)
BIC(logisticModel2)

#predict scores back on the test dataset(testData)
predicted <-predict(logisticModel2, testData) #type =  response
testData$Predicted <- predicted

modelSummary<- summary(logisticModel2)
modelSummary$coefficients
#Need low AIC/BIC values, lower is better
AIC(logisticModel2)
BIC(logisticModel2)

#5 test and graph  the model
library(ggplot2)
#plot fail vs non failure of levee by presence or absence sediments 
ggplot(leveee, aes(x=fail, y=mea)) + geom_point() +
  stat_smooth(method="glm", se=FALSE)
par(mfrow = c(1,1))
plot(fail ~ mea, data=trainingData)
#analysis of variance
#backs up our earlier finding? and what are p-values
anova(logisticModel2, test="Chisq")
anova(logisticModel1, logisticModel2, test = "Chisq")

#also modelSummary
summary(logisticModel2)
par(mfrow = c(1,1))
hist(logisticModel2$fitted.values,main = "Histogram", xlab = "Probability of levee failure or not", col ="light green")

head(logisticModel2)
#PLOTTING Predicted Values
predicted.data <-data.frame(probability.of.fail=logisticModel1$fitted.values, Fail=leveee$fail)

#sort data from low probability to high probabilities.
predicted.data<-predicted.data[order(predicted.data$probability.of.fail, decreasing = FALSE),]

#then we add a new column to the data.frame that has the rank of each sample, from low probability to high probability.
predicted.data$rank <-1:nrow(predicted.data)

library(ggplot2)
library(cowplot)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.fail)) + geom_point(aes(color=Fail), alpha=1, shape=4,stroke=2) + xlab("Index") + ylab("Predicted probability of levee failure")

#PSUEDO R^2 for logistic models for method 2
summary(logisticModel1)
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel1$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel1$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel1$coefficients)-1))

summary(logisticModel2)
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel2$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel2$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel2$coefficients)-1))



#confusion matrix
trainingData$Predict <- ifelse(logisticModel2$fitted.values >.5, "1", "0")
mytable1 <- table(trainingData$fail, trainingData$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logisticModel2 and scoring the values back on the 
#original dataset (testData1) for comparison

pred <-predict(logisticModel2, testData1, "link")
head(pred)

#create a new class prediction column on orignal testData1 dataset 
#for side-by-side comparison 
testData1$class <-ifelse(pred >0, "1", "0")
#look at first 6 and last 6 rows
head(testData1)
tail(testData1)







