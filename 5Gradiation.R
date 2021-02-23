
###########################   MODEL TRIES


wmod1 <- multinom(formula = education ~ government+concerned+opinion, data=wnf)
wmod2 <- multinom(formula = education ~ government+concerned, data=wnf)
wmod3 <- multinom(formula = education ~ government, data=wnf)
wmod4 <- multinom(formula = education ~ concerned+opinion, data=wnf)
wmod5 <- multinom(formula = education ~ opinion, data=wnf)
wmod6 <- multinom(formula = education ~ concerned, data=wnf)

wmod7 <- multinom(formula = news ~ government+concerned+opinion, data=wnf)
wmod8 <- multinom(formula = news ~ government+concerned, data=wnf)
wmod9 <- multinom(formula = news ~ government, data=wnf)

wmod10 <- multinom(formula = knowledge ~ government+concerned+opinion, data=wnf)
wmod11 <- multinom(formula = knowledge ~ government+concerned, data=wnf)
wmod12 <- multinom(formula = knowledge ~ government, data=wnf)

wmod13 <- multinom(formula = government ~ knowledge+concerned+opinion, data=wnf)
wmod14 <- multinom(formula = government ~ knowledge+concerned, data=wnf)
wmod15 <- multinom(formula = government ~ knowledge, data=wnf)


wmod166 <- multinom(formula = government ~ education+concerned+opinion, data=wnf)
wmod17 <- multinom(formula = heard ~ ., data=wnf)
wmod16 <- multinom(formula = government ~ age, data=wnf)

wmod18 <- multinom(formula = heard ~ news+education+prioritize, data=wtest_relev1)
wmod19 <- multinom(formula = risk ~ opinion+knowledge+education, data=wnf)
wmod20 <- multinom(formula = concerned ~heard+opinion+prioritize, data=wnf)


AIC(wmod18)
AIC(wmod14)
AIC(wmod15)
AIC(wmod16)
AIC(wmod17)
AIC(wmod18)
AIC(wmod19)
AIC(wmod20)

AIC(wmod1)
AIC(wmod2)
AIC(wmod3)
AIC(wmod4)
AIC(wmod5)
AIC(wmod6)

AIC(wmod7)
AIC(wmod8)
AIC(wmod9)

AIC(wmod10)
AIC(wmod11)
AIC(wmod19)

summary(wmod1)
summary(wmod2)
summary(wmod3)
summary(wmod4)
summary(wmod5)
summary(wmod6)

##########     CLEANING

library(faraway)
library(MASS)
library(dplyr)
library(ggplot2)
library(gridExtra)
install.packages("printr")
library(printr)
library(tibble)
library(purrr)
library(broom)
theme_set(theme_minimal()) # automatically set a simpler ggplot2 theme for all graphics
library(randomForest)
library(nnet)

head(wn)
#delete columns
wn1 <- wn[, -c(1:13)]
ncol(wn1)
#delete row 1
wn2<- wn1[-c(1), ]
#delete rows with na
wnn<-na.omit(wn2)

wnf<-wnn


#set columns as factors
wnf$education <-as.factor(wnf$education)
wnf$knowledge <-as.factor(wnf$knowledge)
wnf$better <-as.factor(wnf$better)
wnf$safer <-as.factor(wnf$safer)
wnf$risk <-as.factor(wnf$risk)
wnf$news <-as.factor(wnf$news)
wnf$prioritize <-as.factor(wnf$prioritize)
wnf$heard <-as.factor(wnf$heard)
wnf$opinion <-as.factor(wnf$opinion)
wnf$concerned <-as.factor(wnf$concerned)
wnf$government <-as.factor(wnf$government)
wnf$device <-as.factor(wnf$device)
wnf$age <-as.factor(wnf$age)
wnf$region <-as.factor(wnf$region)
wnf$gender <-as.factor(wnf$gender)
wnf$income <-as.factor(wnf$income)





######################    Model1
#Use this



wtest<-wnf

levels(wtest$government) <- gsub("1", "agree", levels(wtest$government))
levels(wtest$government) <- gsub("2", "neutral", levels(wtest$government))
levels(wtest$government) <- gsub("3", "disagree", levels(wtest$government))

levels(wtest$news) <- gsub("1", "main", levels(wtest$news))
levels(wtest$news) <- gsub("2", "social", levels(wtest$news))
levels(wtest$news) <- gsub("3", "both", levels(wtest$news))
levels(wtest$news) <- gsub("4", "other", levels(wtest$news))
levels(wtest$news) <- gsub("5", "none", levels(wtest$news))

levels(wtest2$prioritize) <- gsub("1", "agree", levels(wtest2$prioritize))
levels(wtest2$prioritize) <- gsub("2", "disagree", levels(wtest2$prioritize))
levels(wtest2$prioritize) <- gsub("3", "neutral", levels(wtest2$prioritize))

levels(wtest$opinion) <- gsub("1", "positive", levels(wtest$opinion))
levels(wtest$opinion) <- gsub("2", "negative", levels(wtest$opinion))
levels(wtest$opinion) <- gsub("3", "neutral", levels(wtest$opinion))

levels(wtest$heard) <- gsub("1", "yes", levels(wtest$heard))
levels(wtest$heard) <- gsub("2", "no", levels(wtest$heard))

levels(wtest$concerned) <- gsub("1", "agree", levels(wtest$concerned))
levels(wtest$concerned) <- gsub("2", "neutral", levels(wtest$concerned))
levels(wtest$concerned) <- gsub("3", "disagree", levels(wtest$concerned))

levels(wtest$knowledge) <- gsub("1", "very", levels(wtest$knowledge))
levels(wtest$knowledge) <- gsub("2", "some", levels(wtest$knowledge))
levels(wtest$knowledge) <- gsub("3", "none", levels(wtest$knowledge))

levels(wtest$safer) <- gsub("1", "agree", levels(wtest$safer))
levels(wtest$safer) <- gsub("2", "disagree", levels(wtest$safer))
levels(wtest$safer) <- gsub("3", "neutral", levels(wtest$safer))

levels(wtest$better) <- gsub("1", "agree", levels(wtest$better))
levels(wtest$better) <- gsub("2", "disagree", levels(wtest$better))
levels(wtest$better) <- gsub("3", "neutral", levels(wtest$better))



wtest_relev1<-wtest

wtest_relev1$opinion <- relevel(factor(wtest_relev$opinion, 
                                      ordered = FALSE), "positive")

wtest_relev1$heard <- relevel(factor(wtest_relev1$heard, 
                                     ordered = FALSE), "yes")

xwmod <- multinom(formula = government ~ age+gender , data=wtest)
xwmod <- multinom(formula = better ~ knowledge+news+opinion , data=wtest)
xwmod <- multinom(formula = government ~ better +news+opinion , data=wtest)
wmod20 <- multinom(formula = concerned ~heard+opinion+prioritize, data=wtest_relev1)
AIC(xwmod)


xwmodi <- step(xwmod, trace = 1)
wmod20i<- step(wmod20, trace = 1)

xwmode <- multinom(formula = government ~ news+concerned, data=wtest)
xwmod20e <- multinom(formula = concerned ~ heard+opinion, data=wtest_relev1)

AIC(xwmode)
AIC(xwmod20e)
#visualizing model
wtest %>% 
  group_by(news, government) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = news, y = prop, linetype = government)) +
  geom_line(aes(group = government))

wtest_relev1 %>% 
  group_by(opinion, concerned) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = opinion, y = prop, linetype = concerned)) +
  geom_line(aes(group = concerned))



wtest_relev1


deviance(xwmode)-deviance(xwmod)
#[1] 0.7179939
deviance(xwmod20e)-deviance(wmod20)
#[1] 3.375279

pchisq(0.7179939,xwmod$edf-xwmode$edf, lower=F)
#[1] 0.9490915

pchisq(3.375279,wmod20$edf-xwmod20e$edf, lower=F)
#[1] 0.497094

summary(xwmodi)
#broom::tidy(xwmodi)
summary(wmod20i)


cc<-c(0,-0.1820732847,0.0002725097)
exp(cc)/sum(exp(cc))
# given that they get their news from the mainstream the prob
# that they agree with statement, "the government prioritizes
# my health are indicated below.
#[1] 0.3528815 0.2941409 0.3529776


ccc<-c(0,-1.213029,-1.819157)
exp(ccc)/sum(exp(ccc))
#Given that you have a positive opinion about 5G 
#the probability that you are not concerned about wireless radiantion
#is 0.6851860. 
#neutral 0.2037026 
#disagree 0.1111114

broom::tidy(xwmodi, exponentiate = TRUE)
# A tibble: 10 x 6
#y.level  term          estimate std.error statistic p.value
#<chr>    <chr>            <dbl>     <dbl>     <dbl>   <dbl>
#1 neutral  (Intercept)      0.834     0.428 -0.425     0.671 
#2 neutral  newssocial       2.70      0.738  1.35      0.178 
#3 neutral  newsboth         1.56      0.600  0.741     0.459 
#4 neutral  newsother    22075.       95.9    0.104     0.917 
#5 neutral  newsnone         7.19      1.16   1.70      0.0894
#6 disagree (Intercept)      1.00      0.408  0.000667  0.999 
#7 disagree newssocial       0.750     0.866 -0.332     0.740 
#8 disagree newsboth         1.30      0.586  0.448     0.654 
#9 disagree newsother   128783.       95.9    0.123     0.902 
#10 disagree newsnone         8.99      1.13   1.94      0.0520


#When you go from getting your news from mainstream media to social media
#the odds ratio that you  disagree as to whether or not
#the government priroritizes your health versus agreeing with
#the statement is 0.750  .

broom::tidy(wmod20i, exponentiate = TRUE)

#y.level  term       estimate std.error statistic  p.value
#<chr>    <chr>         <dbl>     <dbl>     <dbl>    <dbl>
#1 neutral  (Intercep…    0.297     0.343     -3.53  4.12e-4
#2 neutral  opinionne…    5.05      0.975      1.66  9.70e-2
#3 neutral  opinionne…    6.53      0.455      4.12  3.73e-5
#4 disagree (Intercep…    0.162     0.440     -4.13  3.57e-5
#5 disagree opinionne…    9.25      1.01       2.20  2.82e-2
#6 disagree opinionne…    2.18      0.647      1.20  2.30e-1

#When you go from having  a positive opinion to a negative opnion 
#the odds ratio that you are concerned about your wireless radiation vs 
#not being concerned  9.25.





####### Model 2


wtest_relev <- wtest
wtest_relev$opinion <- relevel(factor(wtest_relev$opinion, 
                                       ordered = FALSE), "positive")


xwmod2 <- multinom(formula = risk ~ opinion+knowledge+education, data=wtest_relev)
#ttt<- multinom(formula = risk ~ opinion+knowledge, data=wtest)
AIC(xwmod2)


xwmod2i <- step(xwmod2, trace = 1)

xwmod2e <- multinom(formula = risk ~ opinion+knowledge, data=wtest_relev)
summary(xwmod2e)

#visualizing model
wtest_relev %>% 
  group_by(opinion, risk) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = opinion, y = prop, linetype = risk)) +
  geom_line(aes(group = risk))


deviance(xwmod2e)-deviance(xwmod2)
#[1] 4.117266

pchisq(4.117266,xwmod2$edf-xwmod2e$edf, lower=F)
#0.3903682

summary(xwmod2i)
#broom::tidy(xwmod2i)

cc<-c(0,-2.101941,-2.002295)
exp(cc)/sum(exp(cc))
# given that they have a positive opinion about 5G networks the prob
# that they beleive wireless networks 
# pose no adverse health risk is 0.79539054. 
# pose adverse health risk is 0.09721181 
# neutral is 0.10739764

broom::tidy(xwmod2i, exponentiate = TRUE)
#odds ratios
# y.level  term          estimate std.error statistic p.value
# <chr>    <chr>            <dbl>     <dbl>     <dbl>   <dbl>
#  1 disagree (Intercept)      0.122     0.797    -2.64  8.37e-3
# 2 disagree opinionnegat…   63.2       1.27      3.27  1.07e-3
# 3 disagree opinionneutr…    8.00      0.779     2.67  7.62e-3
# 4 disagree knowledge2       0.536     0.848    -0.736 4.62e-1
# 5 disagree knowledge3       0.879     0.925    -0.139 8.89e-1
# 6 neutral  (Intercept)      0.135     0.727    -2.76  5.86e-3
# 7 neutral  opinionnegat…    3.84      1.27      1.06  2.90e-1
# 8 neutral  opinionneutr…    6.09      0.479     3.78  1.60e-4
# 9 neutral  knowledge2       3.64      0.753     1.72  8.63e-2
# 10 neutral  knowledge3       6.45      0.809     2.31  2.12e-2


#When you go from having a positive opinion on wireless networks to a negative one
#the odds ratio that you disagree  with the statement that
#the government priroritizes your health versus agreeing with
#the statement is 63.2

####### Model 3

wtest2<-wnf

head(wtest2$heard)


wtest_relev1<-wtest2

wtest_relev1$heard <- relevel(factor(wtest_relev1$heard, 
                                      ordered = FALSE), "yes")

xwmod3<-multinom(heard ~ knowledge+safer+better, family = binomial, wtest_relev1)
xwmod3i <- step(xwmod3, trace = 1)
xwmod3e<-glm(heard ~ knowledge+safer, family = binomial, wtest_relev1)

xtabs(~ knowledge + heard, data = wtest_relev1)
#           heard
#knowledge yes no
#  very    16  2
#  some    41 20
#  none    20 19


wtest_relev1 %>% 
  group_by(knowledge, heard) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = knowledge, y = prop, linetype = heard)) +
  geom_line(aes(group = heard))


summary(xwmod3i)$coefficients
xwmod3

cc<-c(0,-1.4567647,2.1673635)
exp(cc)/sum(exp(cc))
#[1] 0.10031889 0.02337318 0.87630792

#Given that you believe that you are very knowledgeable about wireless 
#networks, the probability you have not heard of negative health 
#effects of wireless networks is 0.876


broom::tidy(xwmod3i, exponentiate = TRUE)
#y.level term           estimate std.error statistic p.value
#<chr>   <chr>             <dbl>     <dbl>     <dbl>   <dbl>
#1 no      (Intercept)   0.159         0.773    -2.38   0.0174
#2 no      knowledgesome 4.29          0.824     1.77   0.0770
#3 no      knowledgenone 8.74          0.872     2.49   0.0130
#4 no      saferdisagree 0.0000367    64.7      -0.158  0.875 
#5 no      saferneutral  0.735         0.493    -0.624  0.532 

#When you go from claiming to have knowledge of wireless networks to 
#not having any knowledge the odds ration that you have not heard of 
#any negative health effects concerning wireless 
#networks vs having knowledge is 8.74.


ncol(wtest_relev1)

wn1<-wnn

wn11<-as.numeric(wn1)

wn1$education <-as.numeric(wn1$education)
wn1$knowledge <-as.numeric(wn1$knowledge)
wn1$better <-as.numeric(wn1$better)
wn1$safer <-as.numeric(wn1$safer)
wn1$risk <-as.numeric(wn1$risk)
wn1$news <-as.numeric(wn1$news)
wn1$prioritize <-as.numeric(wn1$prioritize)
wn1$heard <-as.numeric(wn1$heard)
wn1$opinion <-as.numeric(wn1$opinion)
wn1$concerned <-as.numeric(wn1$concerned)
wn1$government <-as.numeric(wn1$government)
wn1$device <-as.numeric(wn1$device)
wn1$age <-as.numeric(wn1$age)
wn1$region <-as.numeric(wn1$region)
wn1$gender <-as.numeric(wn1$gender)
wn1$income <-as.numeric(wn1$income)

head(wn1)





library(corrplot)
correlations <- cor(wn1[,1:16])
corrplot(correlations, method="circle")

pairs(wn1, col=wn1$concerned)

install.packages("caret")
library(caret)
x <- wn1[,1:16]
y <- wn1[,3]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

wmod20

model2 <- glm(formula = concerned ~heard+opinion+prioritize, data=wn1)
model2b <- glm(formula = concerned ~opinion, data=wn1)

model1<-glm(heard ~ knowledge+safer+better, family = binomial, wn1)
model1b<-glm(heard ~ knowledge, family = binomial, wn1)

glm.probs <- predict(model2,type = "response")
glm.probs[1:3]
summary(model2)
head(wn1$heard) #1 yes 2 is no
head(wtest_relev1$heard)
levee$Fail<-ifelse(test = 0, yes = "not fail", no = "fail")
head(levee$Fail)


# 2 Creating training and test sample sets
#create training set
data1 = wn1[which(wn1$heard == "1"), ]
data2 = wn1[which(wn1$heard == "2"), ]
nrow(wn1) #118
set.seed(236)
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

#3 Get predictor values with 2 methods (random forest and Boruta method):
install.packages("randomForest")
install.packages("Boruta")
library(randomForest)
output.forest1 <- randomForest(heard ~ education+knowledge+better+safer+news+risk+
                                 prioritize+opinion+concerned+government, data = wn1) 
randomForest::importance(output.forest1)
#Boruta method for determining importance of variables (a great way to visualize this):
library(Boruta)
boruta_output <- Boruta(heard ~ education+knowledge+better+safer+news+risk+
                          prioritize+opinion+concerned+government, 
                        data=na.omit(wn1), doTrace=2)
print(boruta_output) #lists important and iunimportant  variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")]) #collect confirmed
print(boruta_signif) #significant variables
#visualize the variables and their importance
par(mfrow = c(1,1))
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="variable importance") #plot variable importance

# 4 build logistic regression models and get predictions

wn1b<-wn1
trainingData$heard <- sub(1, 1,trainingData$heard)
trainingData$heard <- sub(2, 0,trainingData$heard)
trainingData$heard<-as.numeric(trainingData$heard)

wn1b$heard <- sub(1, 1,wn1b$heard)
wn1b$heard <- sub(2, 0,wn1b$heard)
wn1b$heard<-as.numeric(wn1b$heard)

str(wn1)

logisticModel2<- glm(heard ~ safer+knowledge+better,
                     data = trainingData, family = binomial(link="logit"))

logisticModel3<- glm(heard ~ risk+knowledge+news,
                     data = trainingData, family = binomial(link="logit"))

logisticModel1<- glm(heard ~ education+knowledge+news,
                     data = trainingData, family = binomial(link="logit"))


logisticModel4<- glm(heard ~ knowledge,
                     data = trainingData, family = binomial(link="logit"))


AIC(logisticModel1)
AIC(logisticModel2)
AIC(logisticModel3)
AIC(logisticModel4)


#predict scores back on the test dataset(testData)
predicted <-predict(logisticModel2, testData) #type =  response
testData$Predicted <- predicted

modelSummary<- summary(logisticModel2)
modelSummary$coefficients
#Need low AIC/BIC values, lower is better
AIC(logisticModel2)
BIC(logisticModel2)


#also modelSummary
summary(logisticModel2)
par(mfrow = c(1,1))
hist(logisticModel2$fitted.values,main = "Histogram", xlab = "Probability of hearing of dangers", col ="light green")

#PSUEDO R^2 for logistic models for method 2
summary(logisticModel1) #B
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel1$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel1$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel1$coefficients)-1))

summary(logisticModel2)#A
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel2$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel2$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel2$coefficients)-1))


summary(logisticModel3) #C
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel3$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel3$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel3$coefficients)-1))


summary(logisticModel4) #ModelAB
#Calculate McFallen’s pseudo R^2, we can pull the log-likelihood of the null model out if the logistic variable by getting the value of the null deviance and dividing by -2
ll.null<-logisticModel4$null.deviance/-2
#And we can pull the log-likehood for the fancy model out of the logistic variable by getting the value for the residual deviance nad dividing by -2. 
ll.proposed <- logisticModel4$deviance/-2
#Then we just do the math and we end up with a pseudo R^2=.?. This can be interpreted as the overall effect size.
(ll.null - ll.proposed) / ll.null
# And we can use those same likelihoods to calculate the p-value for that R^2 using a chi-square distribution. 
1 - pchisq(2*(ll.proposed - ll.null), df =(length(logisticModel4$coefficients)-1))



summary(logisticModel3) #C
summary(logisticModel2) #A

#confusion matrix
trainingData$Predict <- ifelse(logisticModel3$fitted.values >.5, "1", "0")
mytable1 <- table(trainingData$heard, trainingData$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logisticModel2 and scoring the values back on the 
#original dataset (testData1) for comparison

#confusion matrix
trainingData$Predict <- ifelse(logisticModel1$fitted.values >.5, "1", "0")
mytable1 <- table(trainingData$heard, trainingData$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logisticModel2 and scoring the values back on the 
#original dataset (testData1) for comparison

#confusion matrix
trainingData$Predict <- ifelse(logisticModel2$fitted.values >.5, "1", "0")
mytable1 <- table(trainingData$heard, trainingData$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logisticModel2 and scoring the values back on the 
#original dataset (testData1) for comparison

#confusion matrix
trainingData$Predict <- ifelse(logisticModel4$fitted.values >.5, "1", "0")
mytable1 <- table(trainingData$heard, trainingData$Predict)
rownames(mytable1) <- c("Obs. neg", "Obs. pos")
colnames(mytable1) <- c("Pred. neg", "Pred. pos")
mytable1 #observed vs predicted
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency # what is the % accuracy
#final prediction of predicting the logisticModel2 and scoring the values back on the 
#original dataset (testData1) for comparison

