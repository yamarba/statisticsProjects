# Determining variables which best explain 
# average points scored by NBA players, 2018-2019

# General Linear Models I - Fall 2019

# Yamar Ba
# Samantha Benedict
# Cesar Rene Pabon Bernal
# Johnny C. Mathis
# Fouad Yared
# source: https://www.basketball-reference.com/leagues/NBA_2019_per_game.html

##########################################################
#################### Loading in libraries ################
##########################################################

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plyr)
library(lattice)
library(readxl)
library(read.csv)
library(ggrepel)
library(data.table)
library(baytrends)
library(lmtest)
library(qualityTools)
library(alr3)
library(gmodels)
library(stats)
library(stringr)
library(sf)
library(ggmap)
library(ISLR)
library(glmnet)
library(car)
library(caret)
library(zoo) 
library(chron) 
library(xts)
library(rugarch)
library(writexl)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(leaps)
library(corpcor)
library(mctest)
library(ppcor)
library(rlang)

###########################################################
#################### Multiple Regression ##################
# Running subset grouping (with forward, backward methods)#
######  Using cross validation to confirm results #########
############# Testing for multicollinearity ###############
###########################################################

################### LOAD DATA
df= read_excel("shots_bball_2018-19.xlsx")
print("dimensions of df:");dim(df)

######### A. EXPLORE DATASET 
#1. summary 
summary(df)
str(df)

#3 Address missing values 
table(is.na(df))
sapply(df, function(x) sum(is.na(x)))

df= df %>% drop_na()
dim(df)
sapply(df, function(x) sum(is.na(x)))

df3= df%>% select(FG_Perc = "FG%", 
                  ThreePoint_Perc= "3P%",
                  TwoPoint_Perc="2P%",
                  eFG_Perc="eFG%",
                  FT_Perc="FT%",
                  everything())
dim(df3)

#5  simple correlation matrix 
df2= select (df,-c(Player,Tm,Rk,Pos));df2 
M = cor(df2)
corrplot(M, type="lower", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))

#6  simple linear regression of full model 
simple_MLRM = lm(PTS~Age+G+GS+MP+
                   ThreePoint_Perc+TwoPoint_Perc+
                   FT_Perc+ORB+DRB+AST+STL+BLK+TOV+PF,data=df3)

simple_MLRM_summary = summary(simple_MLRM)
RSS1 <- c(crossprod(simple_MLRM_summary$residuals))
MSE1 <- RSS1 / length(simple_MLRM_summary$residuals)
RMSE1 <- sqrt(MSE1);RMSE1
sig2_1 <- RSS1 / simple_MLRM_summary$df.residual; sig2_1

print("RSME 14 variables :"); sqrt(mean(simple_MLRM_summary$residuals^2))

#cat("simple_MLRM_summary\n", file = "1_SMLR.txt", append = TRUE)
#capture.output(simple_MLRM_summary, file = "1_SMLR.txt", append = TRUE)

######### B. MODEL SELECTION 
#########    1.We will use a combination of best subset selection w/ backward/forward step 
########     2.Cross Validation: test & train best subset selection w/ backward/forward step
########     3.Multicollinearity and Statistical Significance: VIFs


######## Regsubsets identifies the best model for a given number of k predictors, 
######## where best is quantified using RSS.
best_subset = regsubsets(PTS~Age+G+GS+MP+
                           ThreePoint_Perc+TwoPoint_Perc+
                           FT_Perc+ORB+DRB+
                           AST+STL+BLK+TOV+PF,
                         data=df3, nvmax = 14)
summary(best_subset)
#the best model w/ 3 variables: Age, MP, TOV
#the best model w/ 4 variables: MP, DRB, TOV, PF
#the best model w/ 5 varaibles: Age, MP, DRB, TOV, PF
# best subset selection can lead to overfitting and high variance of the coefficient estimates. 

######      1.forward & backward step 
#forward
best_subset_f = regsubsets(PTS~Age+G+GS+MP+
                             ThreePoint_Perc+TwoPoint_Perc+
                             FT_Perc+ORB+DRB+AST+STL+
                             BLK+TOV+PF,data=df3, 
                           nvmax = 14, method = "forward")
summary(best_subset_f)

#backward
best_subset_b = regsubsets(PTS~Age+G+GS+MP+
                             ThreePoint_Perc+TwoPoint_Perc+
                             FT_Perc+ORB+DRB+AST+
                             STL+BLK+TOV+PF,data=df3, 
                           nvmax = 14,  method = "backward")
summary(best_subset_b)

######     2. Cross Validation: test & train best subset selection w/ backward/forward step
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(df3), 
                 replace = T, prob = c(0.6,0.4))
train <- df3[sample, ]
test <- df3[!sample, ]

######     Regsubsets using train
best_subset_train = regsubsets(PTS~Age+G+GS+MP+
                                 ThreePoint_Perc+TwoPoint_Perc+
                                 FT_Perc+ORB+DRB+AST+
                                 STL+BLK+TOV+PF,data=train, nvmax = 14)
results= summary(best_subset_train)
tibble(predictors = 1:14,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  ggtitle("Best Subset (No Stepwise)")+
  facet_wrap(~ statistic, scales = "free")

print("best model using best stubset train data of R^2:");which.max(results$adjr2)
coef(best_subset_train, 11)
print("best model using best stubset train data of BIC:");which.min(results$bic)
coef(best_subset_train, 8)
print("best model using best stubset train data of cp:");which.min(results$cp)
coef(best_subset_train, 10)

######     Regsubsets using train w/ step forward
best_subset_f_train = regsubsets(PTS~Age+G+GS+MP+
                                   ThreePoint_Perc+TwoPoint_Perc+
                                   FT_Perc+ORB+DRB+AST+
                                   STL+BLK+TOV+PF,data=train, 
                                 nvmax = 14, method = "forward")


results= summary(best_subset_f_train)
tibble(predictors = 1:14,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  ggtitle("Best Subset Stepwise-Forward")+
  facet_wrap(~ statistic, scales = "free")




######     Regsubsets using train w/ step backward
best_subset_b_train = regsubsets(PTS~Age+G+GS+MP+
                                   ThreePoint_Perc+TwoPoint_Perc+
                                   FT_Perc+ORB+DRB+
                                   AST+STL+BLK+TOV+PF,
                                 data=train, nvmax = 14, 
                                 method = "backward")

results= summary(best_subset_b_train)
tibble(predictors = 1:14,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic)%>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F)  +
  ggtitle("Best Subset Stepwise-Backward")+
  facet_wrap(~ statistic, scales = "free")


print("best model using best stubset train data w/ 
      step forward of bic:");which.min(summary(best_subset_f_train)$bic)
print("best model using best stubset train data w/ 
      step forward of cp:");which.min(summary(best_subset_f_train)$cp)
print("best model using best stubset train data w/ 
      step forward of adj. r^2:");which.min(summary(best_subset_f_train)$adjr2)
print("best model using best stubset train data w/ 
      step back of bic:");which.min(summary(best_subset_b_train)$bic)
print("best model using best stubset train data w/ 
      step back of cp:");which.min(summary(best_subset_b_train)$cp)


######     Regsubsets using train
test_m = model.matrix(PTS~Age+G+GS+MP+ThreePoint_Perc+TwoPoint_Perc+
                        FT_Perc+ORB+DRB+AST+STL+BLK+TOV+PF,data=test)

validation_errors = vector("double", length = 14)
for(i in 1:19) {
  coef_x <- coef(best_subset_train, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$PTS - pred_x)^2)  # compute test error btwn actual & predicted salary
}
plot(validation_errors, type = "b", col="blue",
     lwd=3, main= "Validation Errors for Test data of Best Subsets" )


predict.regsubsets <- function(object, newdata, id ,...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(df3), replace = TRUE)
cv_errors <- matrix(NA, k, 14, dimnames = list(NULL, paste(1:14)))

for(j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset <- regsubsets(PTS~Age+G+GS+MP+
                              ThreePoint_Perc+TwoPoint_Perc+
                              FT_Perc+ORB+DRB+AST+
                              STL+BLK+TOV+PF, 
                            df3[folds != j, ], nvmax = 14)
  
  # perform cross-validation
  for( i in 1:14) {
    pred_x <- predict.regsubsets(best_subset_train, df3[folds == j, ], id = i)
    cv_errors[j, i] <- mean((df3$PTS[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors)
plot(mean_cv_errors, type = "b", col="red",lwd=3, 
     main= "Mean CV Errors for Test data of Best Subsets")

print("Based on the CV of best subset our final model should have 8 variables")
final_subset_best = regsubsets(PTS~Age+G+GS+MP+
                                 ThreePoint_Perc+TwoPoint_Perc+
                                 FT_Perc+ORB+DRB+
                                 AST+STL+BLK+TOV+PF, data = df3 , 
                               nvmax = 14)

plot(final_subset_best, scale = "adjr2", 
     col= final_subset_best$lopt, 
     main="Model Evaluation of 8 variables")
coef(final_subset_best, 8)

final_lrm= lm(PTS~MP+ ThreePoint_Perc+ TwoPoint_Perc+ 
                FT_Perc+ DRB+  AST+ TOV+ PF, data=df3)

final_MLRM_summary= summary(final_lrm)   
print("RSME 8 variables :"); sqrt(mean(final_MLRM_summary$residuals^2))
#cat("final_MLRM_summary\n", file = "2_SMLR.txt", append = TRUE)
#capture.output(final_MLRM_summary, file = "2_SMLR.txt", append = TRUE)

########     3.Multicollinearity and Statistical Significance
#            Recursively remove variables with VIF > 4
all_vifs <- car::vif(final_lrm)
print(all_vifs)


#cat("all_vifs\n", file = "allvif_SMLR.txt", append = TRUE)
#capture.output(all_vifs, file = "allvif.txt", append = TRUE)


signif_all <- names(all_vifs)    
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("PTS ~ ", 
                             paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=df3)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
final_model_vif= summary(selectedMod);final_model_vif
#final
#cat("final_model_vif\n", file = "final_model_vif.txt", append = TRUE)
#capture.output(final_model_vif, file = "final_model_vif.txt", append = TRUE)

car::vif(selectedMod)
#cat("vif_final\n", file = "vif_final.txt", append = TRUE)
#capture.output(vif_final, file = "vif_final.txt", append = TRUE)

final_lrm_VIF= lm(PTS~ThreePoint_Perc+ TwoPoint_Perc+ 
                    FT_Perc+ DRB+  AST+ PF, data=df3)
summ= summary(final_lrm_VIF)  
print("RSME 6 variables :"); sqrt(mean(simple_MLRM_summary$residuals^2))
df_final= select (df3,c(ThreePoint_Perc, TwoPoint_Perc, FT_Perc, DRB,  AST, PF))
M_final = cor(df_final)
corrplot(M_final, type="lower", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))

cat("summ\n", file = "summ.txt", append = TRUE)
capture.output(summ, file = "summ.txt", append = TRUE)

#leaps  to continue variable selection
leaps_model= leaps(x = cbind( df3$ThreePoint_Perc , 
                              df3$TwoPoint_Perc,df3$FT_Perc,
                              df3$DRB, df3$AST, df3$PF),
                   y = df3$PTS, nbest = 6, method ="adjr2")

leaps_model

leaps_model2= leaps(x = 
                      cbind(df3$Age,df3$BLK,df3$STL, 
                              df$df3$FT_Perc, df3$ORB, 
                              df3$DRB, df3$AST , df3$TOV, 
                            df3$PF, df3$G, df3$GS, 
                              df3$MP, df3$ThreePoint_Perc , 
                            df3$TwoPoint_Perc),y = df3$PTS, 
                    nbest = 6, method ="adjr2")
leaps_model2

## plot final models and confirm elimination of collinearity 
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(final_lrm_VIF)
par(mfrow=c(1,1))


df_VIF_PTS= select(df3, ThreePoint_Perc, TwoPoint_Perc , FT_Perc, AST, PF, DRB,PTS)
df_VIF_X= select(df3, ThreePoint_Perc, TwoPoint_Perc , FT_Perc, AST, PF,DRB )

dim(df_VIF_PTS)
ggpairs(df_VIF_PTS)
ggpairs(df_VIF_X)
cor2pcor(cov(df_VIF_PTS))
omcdiag(df_VIF_X,df_VIF_PTS$PTS)
imcdiag(df_VIF_X,df_VIF_PTS$PTS)
pcor(df_VIF_X, method = "pearson")

##########################################################
################# Multiple linear regression #############
########## Comparing Ordinary Least Squares to ###########
############# Ridge and Lasso Regression #################
# Evaluating Multiple Linear Regression Model Diagnostics#
##########################################################

bball <- read_excel("shots_bball_2018-19.xlsx")
names(bball)

####################################### 
#### changing variable names, keeping only complete cases, 
# removing unnecessary vars

bball_1 <-
  bball %>% 
  select(FG_Perc = "FG%", 
         ThreePoint_Perc= "3P%",
         TwoPoint_Perc="2P%",
         eFG_Perc="eFG%",
         FT_Perc="FT%",
         everything())

bball_1_woMissing <-
  bball_1[complete.cases(bball_1),]

bball_1_woMissingX <-
  bball_1_woMissing %>% 
  select(-Player, -Pos,-Tm)

names(bball_1_woMissingX)
head(bball_1_woMissingX)

# create 60/40 cross-validation set

set.seed(2019)

samples=sample(nrow(bball_1_woMissingX), 
               0.6*nrow(bball_1_woMissingX))
train = bball_1_woMissingX[samples, ]
test = bball_1_woMissingX[-samples,]
ytest = test$PTS

names(bball_1_woMissingX)
x <- model.matrix(PTS~.,bball_1_woMissingX)[,-27]
y <- bball_1_woMissingX$PTS

samples=sample(nrow(x), 
               0.6*nrow(x))

train1 = x[samples, ]
test1 = x[-samples,]
ytrain1 = y[samples]
ytest1 = y[-samples]

#######################################
names(bball_1_woMissingX)

# ordinary least squares for 3 models: 8,7,6 variable models
par(mfrow=c(2,2))
plot(lm_bball_8vars)
lm_bball_8vars <- lm(PTS~MP+ThreePoint_Perc+TwoPoint_Perc+
                       FT_Perc+DRB+AST+TOV+PF,data=bball_1_woMissingX)
lm_bball_7vars <- lm(PTS~MP+ThreePoint_Perc+TwoPoint_Perc+
                       FT_Perc+DRB+AST+PF,data=bball_1_woMissingX)
lm_bball_6vars <- lm(PTS~ThreePoint_Perc+TwoPoint_Perc+
                       FT_Perc+DRB+AST+PF,data=bball_1_woMissingX)

# can't use matrix for x..
bball_1_woMissingX_8vars <-
  bball_1_woMissingX %>% 
  select(PTS,MP,ThreePoint_Perc,TwoPoint_Perc,
         FT_Perc,DRB,AST,TOV,PF)

bball_1_woMissingX_8vars_pred <- 
  predict(lm_bball_8vars,
          bball_1_woMissingX_8vars,
          interval="prediction")

summary(bball_1_woMissingX_8vars_pred)
# need to fix..
# sum(ytest1 - (predict(lm_bball_8vars,bball_1_woMissingX_8vars)^2))

coef_lm_8vars <-as.data.frame(coef(lm_bball_8vars))
coef_lm_7vars <-as.data.frame(coef(lm_bball_7vars))
coef_lm_6vars <-as.data.frame(coef(lm_bball_6vars))

summary(lm_bball_8vars)$adj.r.squared
summary(lm_bball_7vars)$adj.r.squared 
summary(lm_bball_6vars)$adj.r.squared

mse <- function(sm) mean(sm$residuals^2)

mse(lm_bball_8vars) #8 variable mse 4.57
mse(lm_bball_7vars) #7 variable mse 6.49
mse(lm_bball_6vars) #6 variable mse 9.56

car::vif(lm_bball_8vars)
car::vif(lm_bball_7vars)
car::vif(lm_bball_6vars)

# how VIF is calculated for each predictor..
1/(1-.8604) #7.16
1/(1-.7089) #3.43

# producing model matrices for ridge,lasso models
x_8vars <- model.matrix(PTS~MP+ThreePoint_Perc+TwoPoint_Perc+
                          FT_Perc+DRB+AST+TOV+PF,
                        bball_1_woMissingX)
x_7vars <- model.matrix(PTS~MP+ThreePoint_Perc+TwoPoint_Perc+
                          FT_Perc+DRB+AST+PF,
                        bball_1_woMissingX)
x_6vars <- model.matrix(PTS~ThreePoint_Perc+TwoPoint_Perc+
                          FT_Perc+DRB+AST+PF,
                        bball_1_woMissingX)

y <- bball_1_woMissingX$PTS

lambda <- 10^seq(10,-2,length=100)  

########################################################
########## Ridge regression, 8 variables ###############
########################################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
ridge_mod_8vars <- 
  glmnet(x_8vars[samples,],y[samples],alpha=0,lambda=lambda)

# As the lambda value increases, the coefficients get closer to zero.
predict(ridge_mod_8vars, s=.1, type='coefficients')
predict(ridge_mod_8vars, s=1, type='coefficients')
predict(ridge_mod_8vars, s=10, type='coefficients')
predict(ridge_mod_8vars, s=100, type='coefficients')

cv_ridge_mod_8vars <- 
  cv.glmnet(x_8vars[samples,],y[samples],alpha=0)
cv_ridge_mod_8vars # shows lambda, other values
bestlam_8vars <- cv_ridge_mod_8vars$lambda.min
bestlam_8vars # best lambda is 0.5067

plot(cv_ridge_mod_8vars,main="Ridge Regression")
r_squared_8vars=1-cv_ridge_mod_8vars$cvm/var(y[samples])
r_squared_8vars_max <- 
  max(1-cv_ridge_mod_8vars$cvm/var(y[samples]))

plot(cv_ridge_mod_8vars$lambda,r_squared_8vars,
     main=expression(R^{2}~"values for different lambda parameters in 8 variable Ridge Regression Model"))

plot(ridge_mod_8vars,xvar="lambda",
     main="
     Coefficients of predictors in 8 variable Ridge Regression model
     
     ")

plot(ridge_mod_8vars, main="Ridge Regression,
     Relationship of L1 Norm and Coefficients")

ridge_mod_8varsBest <- glmnet(x_8vars[samples,],
                              y[samples],
                              alpha=0,lambda=bestlam_8vars)
ridge_mod_coefs_8varsBest <-
  predict(ridge_mod_8varsBest,
          type="coefficients",s=bestlam_8vars)

ridge_pred_8varsBest <- 
  predict(ridge_mod_8varsBest,
          s=bestlam_8vars,newx=x_8vars[-samples,])

# test MSE=4.46
mean((ridge_pred_8varsBest-ytest1)^2) 
# test MSE with only Y intercept=31.42
mean((mean(y[samples])-ytest1)^2) 

########################################################
########## Lasso regression, 8 variables ###############
########################################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod_8vars <- 
  glmnet(x_8vars[samples,],y[samples],alpha=1,lambda=lambda)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod_8vars, s=.1, type='coefficients')
predict(lasso_mod_8vars, s=1, type='coefficients')
predict(lasso_mod_8vars, s=10, type='coefficients')
predict(lasso_mod_8vars, s=100, type='coefficients')

cv_lasso_mod_8vars <- 
  cv.glmnet(x_8vars[samples,],y[samples],alpha=1)
cv_lasso_mod_8vars # shows lambda, other values
bestlam_lasso_8vars <- cv_lasso_mod_8vars$lambda.min
bestlam_lasso_8vars # best lambda is 0.00685

plot(cv_lasso_mod_8vars,main="Lasso Regression")
r_squared_8vars_lasso=1-cv_lasso_mod_8vars$cvm/var(y[samples])
r_squared_8vars_lasso_max <- 
  max(1-cv_lasso_mod_8vars$cvm/var(y[samples]))

plot(cv_lasso_mod_8vars$lambda,r_squared_8vars_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in 8 variable Lasso Regression Model"))

plot(lasso_mod_8vars,xvar="lambda",
     main="
     Coefficients of predictors in 8 variable Lasso Regression model
     
     ")

plot(lasso_mod_8vars, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_mod_8varsBest <- glmnet(x_8vars[samples,],
                              y[samples],
                              alpha=1,lambda=bestlam_lasso_8vars)
lasso_mod_coefs_8varsBest <-
  predict(lasso_mod_8varsBest,
          type="coefficients",s=bestlam_lasso_8vars)

lasso_pred_8varsBest <- 
  predict(lasso_mod_8varsBest,
          s=bestlam_8vars,newx=x_8vars[-samples,])

# test MSE=4.15
mean((lasso_pred_8varsBest-ytest1)^2) 
# test MSE with only Y intercept=31.42
mean((mean(y[samples])-ytest1)^2) 

########################################################
########## Ridge regression, 6 variables ###############
########################################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
ridge_mod_6vars <- 
  glmnet(x_6vars[samples,],y[samples],alpha=0,lambda=lambda)

# As the lambda value increases, the coefficients get closer to zero.
predict(ridge_mod_6vars, s=.1, type='coefficients')
predict(ridge_mod_6vars, s=1, type='coefficients')
predict(ridge_mod_6vars, s=10, type='coefficients')
predict(ridge_mod_6vars, s=100, type='coefficients')

cv_ridge_mod_6vars <- 
  cv.glmnet(x_6vars[samples,],y[samples],alpha=0)
cv_ridge_mod_6vars # shows lambda, other values
bestlam_6vars <- cv_ridge_mod_6vars$lambda.min
bestlam_6vars # best lambda is 0.4069

plot(cv_ridge_mod_6vars,main="Ridge Regression")
r_squared_6vars=1-cv_ridge_mod_6vars$cvm/var(y[samples])
r_squared_6vars_max <- 
  max(1-cv_ridge_mod_6vars$cvm/var(y[samples]))

plot(cv_ridge_mod_6vars$lambda,r_squared_6vars, 
     main=expression(R^{2}~"values for different lambda parameters in 6 variable Ridge Regression Model"))

plot(ridge_mod_6vars,xvar="lambda",
     main="
     Coefficients of predictors in 6 variable Ridge Regression model
     
     ")

plot(ridge_mod_6vars, main="Ridge Regression,
     Relationship of L1 Norm and Coefficients")

ridge_mod_6varsBest <- glmnet(x_6vars[samples,],
                              y[samples],
                              alpha=0,lambda=bestlam_6vars)
ridge_mod_coefs_6varsBest <-
  predict(ridge_mod_6varsBest,
          type="coefficients",s=bestlam_6vars)

ridge_pred_6varsBest <- 
  predict(ridge_mod_6varsBest,
          s=bestlam_6vars,newx=x_6vars[-samples,])

# test MSE=9.19
mean((ridge_pred_6varsBest-ytest1)^2) 
# test MSE with only Y intercept=31.42
mean((mean(y[samples])-ytest1)^2) 

########################################################
########## Lasso regression, 6 variables ###############
########################################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod_6vars <- 
  glmnet(x_6vars[samples,],y[samples],alpha=1,lambda=lambda)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod_6vars, s=.1, type='coefficients')
predict(lasso_mod_6vars, s=1, type='coefficients')
predict(lasso_mod_6vars, s=10, type='coefficients')
predict(lasso_mod_6vars, s=100, type='coefficients')

cv_lasso_mod_6vars <- 
  cv.glmnet(x_6vars[samples,],y[samples],alpha=1)
cv_lasso_mod_6vars # shows lambda, other values
bestlam_lasso_6vars <- cv_lasso_mod_6vars$lambda.min
bestlam_lasso_6vars # best lambda is 0.01845

plot(cv_lasso_mod_6vars,main="Lasso Regression")
r_squared_6vars_lasso=1-cv_lasso_mod_6vars$cvm/var(y[samples])
r_squared_6vars_lasso_max <- 
  max(1-cv_lasso_mod_6vars$cvm/var(y[samples]))

plot(cv_lasso_mod_6vars$lambda,r_squared_6vars_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in 6 variable Lasso Regression Model"))

plot(lasso_mod_6vars,xvar="lambda",
     main="
     Coefficients of predictors in 6 variable Lasso Regression model
     
     ")
plot(lasso_mod_6vars, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_mod_6varsBest <- glmnet(x_6vars[samples,],
                              y[samples],
                              alpha=1,lambda=bestlam_lasso_6vars)
lasso_mod_coefs_6varsBest <-
  predict(lasso_mod_6varsBest,
          type="coefficients",s=bestlam_lasso_6vars)

lasso_pred_6varsBest <- 
  predict(lasso_mod_6varsBest,
          s=bestlam_6vars,newx=x_6vars[-samples,])

# test MSE=9.13
mean((lasso_pred_6varsBest-ytest1)^2) 
# test MSE with only Y intercept=31.42
mean((mean(y[samples])-ytest1)^2)

##########################################################
#################### Simple linear regression ############
##########################################################

# Evaluate correlation of outcome, predictors (two_PA, FT)
cor(bbdata_1_$PTS, bbdata_1_$two_PA)
cor(bbdata_1_$PTS, bbdata_1_$FT)

# Gather the coefficients of linear model
coef(lm(bbdata_1_$PTS~bbdata_1_$two_PA))
coef(lm(bbdata_1_$PTS~bbdata_1_$FT))

# Gather a 95% non-simultaneous confidence interval for b0,b1
confint(lm(bbdata_1_$PTS~bbdata_1_$two_PA))
confint(lm(bbdata_1_$PTS~bbdata_1_$FT))

############################

################################################################
#################### Simple linear regression ##################
# Creating confidence bands for Regression Lines and Predictions#
# Using the F-test to measure Lack of Fit #######################
# Calculating Simulataenous Confidence Intervals for B0, B1 #####
# Calculating Simulataenous Confidence Intervals and Predictions#
#################################################################

# 2.14
# a)
# FG.PTS

b.not<-0.01875
b.1<-2.72863
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]11.16863

mean(bbdata$FG)
x.bar<-mean(bbdata$FG)

#confidence interval set up
sum((bbdata$FG-x.bar)^2)
# [1]2742.158
x.h<-4.1
(x.h-x.bar)^2
# [1]0.6425462
n<-630
1/n
# [1]0.001587302
MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$FG))^2)/628)
MSE
# [1]4.123873e-08

#t-score = 1.645
#CI
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$FG-x.bar)^2))
# [1]0.01530756
error<-1.645*0.01530756
left<-Y.hat-error
right<-Y.hat+error
left
# [1]11.15786
right
# [1]11.17941

# Therefore, we are 90% confident that the mean points in 
# games on which a average of 4.1 Field goals are made per 
# player is between the interval 11.15786 and 11.17941.

# a)
two_PA

b.not<-1.620
b.1<-1.612
x<-3
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]6.456
MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$two_PA))^2)/628)
MSE
# [1]0.002067975

#t-score = 1.645
#CI
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$two_PA-x.bar)^2))
# [1]0.01941818

error<-1.645*0.01941818
error
# [1]0.03194291
left<-Y.hat-error
right<-Y.hat+error
left
# [1]6.424057
right
# [1]6.487943

# Therefore, we are 90% confident that the mean points scored in 
# games in which an average of 3 field goals are made by a player 
# is between the interval 6.424057 and 6.487943.

# a)
# FT

b.not<- 3.179
b.1<- 4.133
x<-1.2
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]8.1386
x.bar<-mean(bbdata$FT)
x.bar
# [1]1.403816

MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$FT))^2)/628)
MSE
# [1]0.0001160828
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$FT-x.bar)^2))
# [1]0.006549819

error<-1.645*0.006549819
error
# [1]0.01077445
left<-Y.hat-error
right<-Y.hat+error
left
# [1]8.127826

right
# [1]8.149374

# Therefore, we are 90% confident that the mean points 
# per games in which an average of 1.2 free-throw points 
# are made player is between the interval 8.127826 and 8.149374.

# b)
# FG.PTS

sqrt(MSE*(1+(1/n)+((x.h-x.bar)^2)/sum((bbdata$FG-x.bar)^2)))
# [1]0.0002032581
error1<-1.645*0.0002032581

left<-Y.hat-error1
right<-Y.hat+error1
left
# [1]11.1683
right
# [1]11.16897

# The 90% prediction interval for the mean field goal on the next 
# game in which a player scores an average of 6 points is between 
# the interval 11.1683 and 11.16897.

# b)
# two_PA
sqrt(MSE*(1+(1/n)+((x.h-x.bar)^2)/sum((bbdata$PTS-x.bar)^2)))
# [1]0.04551275


error1<-1.645*0.04551275
left<-Y.hat-error1
right<-Y.hat+error1
left
# [1]6.381132
right
# [1]6.530868

# The 90% prediction interval for the mean points on the next 
# game in which a player makes an average of 2 point field 
# goal attempts is between the interval  6.381132 and 6.530868.

# b)
# FT

sqrt(MSE*(1+(1/n)+((x.h-x.bar)^2)/sum((bbdata$FG-x.bar)^2)))
# [1]0.01078277

error1<-1.645*0.01078277
left<-Y.hat-error1
right<-Y.hat+error1
left
# [1]8.120862
right
# [1]8.156338

# The 90% prediction interval for the mean PTS on the next 
# game in which a an average of 1.2 FT are made is between 
# the interval  8.120862 and 8.156338.

# d)
# FG

#Y.hat = 11.1579
# W^2 = 2F(.90; 1, 627) = 2*(2.714) = 5.428, W = 2.3298
Y.hat-(2.3298*0.01530756)
# [1]11.12224
Y.hat+(2.3298*0.01530756)
# [1]11.19356

# The confidence interval in part a) is between the 
# interval (2.194226, 2.262174) compared to this interval 
# (2.180,2.276), is wider. As it should be as it is from 
# the entire regression line of real numbers. 

# d)
# two_PA
#Y.hat = 6.456
# W^2 = 2F(.90; 1, 627) = 2*(2.714) = 5.428, W = 2.3298
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$PTS-x.bar)^2))
# [1]0.01941818
Y.hat-(2.3298*0.01941818)
# [1]6.41076
Y.hat+(2.3298*0.01941818)
# [1]6.50124

# The confidence interval in part a) is between the 
# interval 3.041, 3.067 compared to this interval 
# (2.780, 3.328), is wider. As it should be as it 
# is from the entire regression line of real numbers. 

# d)
# FT

Y.hat
# [1]8.1386
# W^2 = 2F(.90; 1, 627) = 2*(2.714) = 5.428, W = 2.3298
Y.hat-(2.3298*0.006549819)
# [1]8.12334
Y.hat+(2.3298*0.006549819)
# [1]8.15386

# The confidence interval in part a) is between the interval
# .7952 and .8632. compared to this interval (0.7810946, 0.8773054), 
# is wider. As it should be as it is from the entire 
# regression line of real numbers.

# 3.13 
# F-test for lack of fit

# FT
Reduced <- lm(PTS ~ FT, data = bbdata) 
Full <- lm(PTS ~ 0 + as.factor(FT), data = bbdata)
anova (Reduced, Full)

#Analysis of Variance Table
#
#Model 1: PTS ~ FT
#Model 2: PTS ~ 0 + as.factor(FT)
#  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    627 4237.4                                  
#2    571 3354.9 56    882.54 2.6823 4.527e-09 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# H0:E(Y)=B0+B1X
# HA: E(Y)does not equal B0+B1X
F(.95,56,571)= 1.353
# F*=(882.54/56)/(3354.9/571)=2.682

# a)
# two_PA

Reduced <- lm(PTS ~ two_PA, data = bbdata) 
Full <- lm(PTS ~ 0 + as.factor(two_PA), data = bbdata)
anova (Reduced, Full)
#Analysis of Variance Table
#
#Model 1: PTS ~ two_PA
#Model 2: PTS ~ 0 + as.factor(two_PA)
#  Res.Df    RSS  Df Sum of Sq      F   Pr(>F)   
#1    627 3802.7                                 
#2    501 2759.5 126    1043.2 1.5031 0.001217 **
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# H0:E(Y)=B0+B1X
# HA: E(Y)does not equal B0+B1X
F(.95,126,501)=1.25
# F*=(1043.2/126)/(2759.5/501)=1.503

# a)
# FG
Reduced <- lm(PTS ~ FG, data = bbdata) 
Full <- lm(PTS ~ 0 + as.factor(FG), data = bbdata)
anova (Reduced, Full)
#Analysis of Variance Table
#
#Model 1: PTS ~ FG
#Model 2: PTS ~ 0 + as.factor(FG)
#  Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
#1    627 434.58                                 
#2    535 272.20 92    162.38 3.469 < 2.2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#H0:E(Y)=B0+B1X
#HA: E(Y)does not equal B0+B1X
F(.95,92,535)=1.28
# F*=(162.38/92)/(272.2/535)=3.469

# F*<= Fail to reject
# F*>= We reject H0

# We conclude HA on on 3 cases, that the regression function is linear

# 4.3
# b)
bbdata.FG.lm <- lm(bbdata$PTS~bbdata$FG)
bbdata.FG.lm

#Call:
#lm(formula = bbdata$PTS ~ bbdata$FG)
#
#Coefficients:
#(Intercept)    bbdata$FG  
#   -0.01875      2.72863  

joint.fam <- 2 #number of joint betas we want.

alpha <- 0.05
joint.level <- alpha/joint.fam
bonf.int <- confint(bbdata.FG.lm, level = 1-joint.level)

#bonf.int
#                1.25 %  98.75 %
#(Intercept) -0.1581915 0.120694
# bbdata$FG    2.6929116 2.764353

# b)
# two_PA
bbdata.two_PA.lm <- lm(bbdata$two_PA~bbdata$PTS)
bbdata.two_PA.lm

#Call:
#lm(formula = bbdata$PTS ~ bbdata$two_PA)

#Coefficients:
#  (Intercept)  bbdata$two_PA  
#        1.620          1.612

joint.fam <- 2 #number of joint betas we want.
alpha <- 0.05
joint.level <- alpha/joint.fam
bonf.int <- confint(bbdata.two_PA.lm, level = 1-joint.level)
bonf.int

#               1.25 %  98.75 %
#(Intercept)   1.237432 2.001636
#bbdata$two_PA 1.544166 1.680832

# b)
# FT
bbdata.FT.lm <- lm(bbdata$FT~bbdata$PTS) 
bbdata.FT.lm

#Call:
#lm(formula = bbdata$PTS ~ bbdata$FT)
#
#Coefficients:
#(Intercept)    bbdata$FT  
#      3.179        4.133 

joint.fam <- 2 #number of joint betas we want.
alpha <- 0.05
joint.level <- alpha/joint.fam
bonf.int <- confint(bbdata.FT.lm, level = 1-joint.level)
bonf.int

#             1.25 %  98.75 %
#(Intercept) 2.827850 3.530344
#bbdata$FT   3.945936 4.320537

#4.7

# FG
b.not<--0.01875
b.1<-2.72863
x<-2.6
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]7.075688
x.bar<-mean(bbdata$FG)
x.h<-2.6
MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$FG))^2)/628)

sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$FG-x.bar)^2))
# [1]0.01333719

7.075688-(2.33*0.01333719)
# [1]7.044612
7.075688+(2.33*0.01333719)
# [1]7.106764

x<-5.6
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]15.26158
15.26158-(2.33*0.04395231)
# [1]15.15917
15.26158+(2.33*0.04395231)
# [1]15.3639

#two_PA
b.not<-1.620
b.1<-1.612
x<-3.5
Y.hat<-b.not+b.1*(x)
x.h<-3.5
MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$two_PA))^2)/628)

x.bar<-mean(bbdata$two_PA)
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$two_PA-x.bar)^2))
# [1]0.01328276

Y.hat-(2.33*0.01328276)
# [1]7.231051
Y.hat+(2.33*0.01328276)
# [1]7.292949

x<-7.6
x.h<-7.6
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]13.8712
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$two_PA-x.bar)^2))
# [1]0.03751894

Y.hat-(2.33*0.03751894)
# [1]13.78378
Y.hat+(2.33*0.03751894)
# [1]13.95862


#FT
b.not<- 3.179
b.1<- 4.133
x<-1
x.h<-1
Y.hat<-b.not+b.1*(x)
Y.hat
# [1]7.312
x.bar<-mean(bbdata$FT)
MSE<-((sum(bbdata$PTS-(b.not+b.1*bbdata$FT))^2)/628)
sqrt(MSE*(1/n)+((x.h-x.bar)^2)/sum((bbdata$FT-x.bar)^2))
# [1]0.01295623

Y.hat+(2.33*0.01295623)
# [1]7.342188
Y.hat-(2.33*0.01295623)
# [1]7.281812

FG.PTS<-lm(bdata$FG ~ bdata$PTS)
summary(FG.PTS)

#Call:
#lm(formula = bdata$FG ~ bdata$PTS)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2.25594 -0.14530 -0.01556  0.13269  1.26231 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.077724   0.019668   3.952 8.54e-05 ***
#bdata$PTS   0.359507   0.001927 186.601  < 2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.2986 on 706 degrees of freedom
#Multiple R-squared:  0.9801,    Adjusted R-squared:  0.9801 
#F-statistic: 3.482e+04 on 1 and 706 DF,  p-value: < 2.2e-16

FT.PTS<-lm(bdata$FT ~ bdata$PTS)
summary(FT.PTS)

#Call:
#lm(formula = bdata$FT ~ bdata$PTS)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-2.1106 -0.3050 -0.0257  0.2787  3.2482 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.278746   0.036703  -7.595 9.82e-14 ***
#bdata$PTS    0.190201   0.003595  52.903  < 2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.5572 on 706 degrees of freedom
#Multiple R-squared:  0.7986,    Adjusted R-squared:  0.7983 
#F-statistic:  2799 on 1 and 706 DF,  p-value: < 2.2e-16

FTA.PTS<-lm(bdata$FTA ~ bdata$PTS)
summary(FTA.PTS)

#Call:
#lm(formula = bdata$FTA ~ bdata$PTS)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-2.8024 -0.3825 -0.0451  0.3344  3.8952 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.222834   0.048109  -4.632 4.31e-06 ***
#bdata$PTS    0.233734   0.004713  49.598  < 2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.7303 on 706 degrees of freedom
#Multiple R-squared:  0.777,    Adjusted R-squared:  0.7767
#F-statistic:  2460 on 1 and 706 DF,  p-value: < 2.2e-16

TOV.PTS<-lm(bdata$TOV ~ bdata$PTS)
summary(TOV.PTS)

#Call:
#lm(formula = bdata$TOV ~ bdata$PTS)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.95591 -0.24238 -0.04608  0.20425  1.91242 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.135814   0.027274    4.98 8.01e-07 ***
#bdata$PTS   0.107064   0.002672   40.08  < 2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.414 on 706 degrees of freedom
#Multiple R-squared:  0.6946,    Adjusted R-squared:  0.6942 
#F-statistic:  1606 on 1 and 706 DF,  p-value: < 2.2e-16

twoPA.PTS<-lm(bdata$"2PA" ~ bdata$PTS)
summary(twoPA.PTS)

#Call:
#lm(formula = bdata$"2PA" ~ bdata$PTS)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-6.9819 -0.7294  0.0168  0.7372  6.3936 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.106933   0.088723   1.205    0.229    
#bdata$PTS   0.503460   0.008691  57.929   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.347 on 706 degrees of freedom
#Multiple R-squared:  0.8262,    Adjusted R-squared:  0.8259 
#F-statistic:  3356 on 1 and 706 DF,  p-value: < 2.2e-16

twoP.PTS<-lm(bdata$"2P" ~ bdata$PTS)
summary(twoP.PTS)

#Call:
#lm(formula = bdata$"2P" ~ bdata$PTS)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.6692 -0.4123 -0.0007  0.3735  2.8611 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.049211   0.052229  -0.942    0.346    
#bdata$PTS    0.269207   0.005116  52.620   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.7928 on 706 degrees of freedom
#Multiple R-squared:  0.7968,    Adjusted R-squared:  0.7965 
#F-statistic:  2769 on 1 and 706 DF,  p-value: < 2.2e-16

FGA.PTS<-lm(bdata$FGA ~ bdata$PTS)
summary(FGA.PTS)

#Call:
#lm(formula = bdata$FGA ~ bdata$PTS)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.8698 -0.4631 -0.0145  0.4874  3.6387 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.684481   0.057029    12.0   <2e-16 ***
#bdata$PTS   0.739541   0.005586   132.4   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.8657 on 706 degrees of freedom
#Multiple R-squared:  0.9613,    Adjusted R-squared:  0.9612 
#F-statistic: 1.753e+04 on 1 and 706 DF,  p-value: < 2.2e-16

#######################################################  AFTER CLEANED

FG.PTS<-lm(bbdata$FG ~ bbdata$PTS)
summary(FG.PTS)

# Call:
#   lm(formula = bbdata$FG ~ bbdata$PTS)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.22981 -0.15339 -0.01274  0.13042  1.27534 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.075473   0.022306   3.384  0.00076 ***
#   bbdata$PTS  0.358846   0.002091 171.630  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3019 on 627 degrees of freedom
# Multiple R-squared:  0.9792,    Adjusted R-squared:  0.9791 
# F-statistic: 2.946e+04 on 1 and 627 DF,  p-value: < 2.2e-16

twoPA.PTS<-lm(bbdata$two_PA ~ bbdata$PTS)
summary(twoPA.PTS)

# Call:
#   lm(formula = bbdata$two_PA ~ bbdata$PTS)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.0162 -0.7396  0.0208  0.7559  6.4377 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.011422   0.102029   0.112    0.911    
# bbdata$PTS  0.507057   0.009564  53.019   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.381 on 627 degrees of freedom
# Multiple R-squared:  0.8176,    Adjusted R-squared:  0.8173 
# F-statistic:  2811 on 1 and 627 DF,  p-value: < 2.2e-16

FT.PTS<-lm(bbdata$FT ~ bbdata$PTS)
summary(FT.PTS)

# Call:
#   lm(formula = bbdata$FT ~ bbdata$PTS)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1171 -0.2942 -0.0086  0.2878  3.2263 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.327555   0.041479  -7.897 1.28e-14 ***
#   bbdata$PTS   0.192773   0.003888  49.581  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5614 on 627 degrees of freedom
# Multiple R-squared:  0.7968,    Adjusted R-squared:  0.7965 
# F-statistic:  2458 on 1 and 627 DF,  p-value: < 2.2e-16

#############################################

##########################################################
#################### Model diagnostics  ##################
########## for Simple Linear Regression ##################
##########################################################

#Import Data
df <- read_excel("bball_final.xlsx")

#Omit missing values
df <- na.omit(df)  
summary(df)
attach(df)

### Simple Linear Regression on Variables with the highest ADJ R^2 (not in MRV)

#Linear Regresson on two_pt_A (adj. r^2= .8173)
df_linear9 = lm(PTS ~ two_pt_A)
summary(df_linear9)

#Linear Regresson on FT (adj. r^2= .7965)
df_linear10 = lm(PTS ~ FT)
summary(df_linear10)


########################################
# T-test to determine linear association 
########################################

#T-crit, alpha=.05, n-2 = 627
t_crit = qt(.975, df=627); t_crit

####For FG
#T-stat, H0: B1=0, Ha: B1 neq 0, 2 tail test
t_stat_FG = summary(df_linear7)$coefficients[2]/summary(df_linear7)$coefficients[4]; t_stat_FG

#Since t stat, t*=171.6297, is greater than t crit we can reject H0 and conclude with 95% confidence
# that there is a linear relation between FIELD GOALS and POINTS SCORED.

####For two_pt_A
#T-stat, H0: B1=0, Ha: B1 neq 0, 2 tail test
t_stat_two_pt_A = summary(df_linear9)$coefficients[2]/summary(df_linear9)$coefficients[4]; t_stat_two_pt_A

# Since t stat, t*=53.0192, is greater than t crit we can reject H0 and conclude with 95% confidence
# that there is a linear relation between TWO POINTERS ATTEMPTED and POINTS SCORED.


####For FT
#T-stat, H0: B1=0, Ha: B1 neq 0, 2 tail test
t_stat_FT = summary(df_linear10)$coefficients[2]/summary(df_linear10)$coefficients[4]; t_stat_FT

# Since t stat, t*=49.58094, is greater than t crit we can reject H0 and conclude with 95% confidence
# that there is a linear relation between FREE THROWS and POINTS SCORED.

########################################
# Model Diagnostics (3.4 a, d, e, & g)
########################################

#### Summary Stats for predictors
#histograms
par(mfrow=c(1,2))
hist(two_pt_A,col= "darkblue", main ="2PA", border="darkgrey")
hist(FT,col= "darkred", main ="FT", border="darkgrey")

#boxplots
dotPlot(two_pt_A, ylim=c(0,35), ylab="Frequency", 
        xlab="Two Point FG Attempts per Game", stacked = TRUE, pch =20, 
        main = "Two Point FG Attempted per Game", col= "darkblue")
dotPlot(FT, ylim=c(0,50), ylab="Frequency", xlab="Free Throws Per Game", 
        stacked = TRUE, pch =20, main = "Free Throws Per Game", col= "darkred")

#BOXPLOT OF RESIDUALS
par(mfrow=c(2,1))
#### boxplot of residuals 2PA
boxplot(df_linear9$residuals, pch=20, horizontal=TRUE, ylim=c(-10,10), 
        main="Boxplot of Residuals for 2PA", col="darkblue", border="darkgrey")
#### boxplot of residuals
boxplot(df_linear10$residuals, pch=20, horizontal=TRUE, ylim=c(-10,10), 
        main="Boxplot of Residuals for FT", col="darkred", border="darkgrey")

#RESIDUAL VS. PREDICTOR
par(mfrow=c(1,2))
#### Residual vs. Two Point FG Attempts Per Game For FG (to test constancy of error variance)
#megaphone type, shows departures from constancy of the error variance
plot(df_linear9$residuals ~ FG, main="Residuals vs. Two Pt FG Attempts Per Game", 
     xlab="Two Pt FG Attempts Per Game", ylab= "Residuals", pch= 20, col= "darkblue")
abline(h=mean(df_linear9$residuals), col= "sienna")
#### Residual vs. Field Goals Per Game For FG (to test constancy of error variance)
#megaphone type, shows departures from constancy of the error variance
plot(df_linear10$residuals ~ FT, main="Residuals vs. Free Throws per Game", 
     xlab="Free Throws per Game", ylab= "Residuals", pch= 20, col= "darkred")
abline(h=mean(df_linear10$residuals), col= "sienna")

#TEST FOR OUTLIERS
par(mfrow=c(1,2))
#For 2PA
df_linear9_studres = rstudent(df_linear9)    
plot(df_linear9_studres~ two_pt_A, xlab="2PA", ylab= "Semi-Studentized Residuals", 
     pch= 20, col= "darkblue", main="Semi-Studentized Residuals vs. Two Pt FG Attempts Per Game")
abline(c(0,0), col="sienna")  
#For 2PA
df_linear10_studres = rstudent(df_linear10)    
plot(df_linear10_studres~ FT, xlab="FT", ylab= "Semi-Studentized Residuals", 
     pch= 20, col= "darkred", main="Semi-Studentized Residuals vs. Free Throws Per Game")
abline(c(0,0), col="sienna")  

#NORMAL PROB PLOT OF RESIDUALS
par(mfrow=c(1,2))
#### Normal Probability Plot of Residuals For 2PA
#error distribution with heavy tails, suggests error distribution is not normal
plot(df_linear9,pch=20, which=c(2), col= "darkblue")
#### Normal Probability Plot of Residuals For FT
#error distribution with heavy tails, suggests error distribution is not normal
plot(df_linear10,pch=20, which=c(2), col= "darkred")

#### stemleaf of residuals
#2PA
stem(df_linear9$residuals, scale = 5)
summary(df_linear9$residuals)
#FT
stem(df_linear10$residuals, scale = 5)
summary(df_linear10$residuals)

#### Obtain r between the ordered residuals and their exp. values
#2PA
n=629 
sq_mse = summary(df_linear9)$sigma;sq_mse
expected = sapply(1:n, function(k) sq_mse * qnorm((k-.375)/(n+.25))); expected
r=cor(expected,sort(df_linear9$residuals)); r

#H0:residuals are normally distributed 
#alpha = .05
#r= (from B-6) .9976
#r*=.9649414
#r*<r therefore we reject H0 and conclude that the residuals are not normally distributed

#FT
sq_mse = summary(df_linear9)$sigma;sq_mse
expected = sapply(1:n, function(k) sq_mse * qnorm((k-.375)/(n+.25))); expected
r=cor(expected,sort(df_linear9$residuals)); r

#H0:residuals are normally distributed 
#alpha = .05
#r= (from B-6) .9976
#r*=.9649414
#r*<r therefore we reject H0 and conclude that the residuals are not normally distributed

#### Transformation for nonconstancy of error variance and nonnormality of error terms
# The nonnormality and unequal variances departures take the form of increasing skewness 
# and increasing variability of the distributions of the error terms as 
# the mean response E {Y} increases.

#2PA
df_linear9_sqrt <- lm(sqrt(PTS) ~two_pt_A)
plot(df_linear9_sqrt$residuals ~ FG, 
     main="Residuals vs. Two PT FG Attempts per Game", 
     xlab="Two PT FG Attempts per Game", ylab= "Residuals", 
     pch= 20, col= "darkgreen")
abline(h=mean(df_linear9_sqrt$residuals), col= "sienna")

plot(df_linear9_sqrt,pch=20, which=c(2), col= "darkgreen")

#FT
df_linear10_sqrt <- lm(sqrt(PTS) ~ FT)
summary(df_linear10_sqrt)

par(mfrow=c(2,1))
plot(df_linear10_sqrt$residuals ~ FT, 
     main="Sqrt Transformation Model: Residuals vs. Free Throws per Game", 
     xlab="Free Throws per Game", ylab= "Residuals", pch= 20, col= "darkred")
abline(h=mean(df_linear10_sqrt$residuals), col= "sienna")
plot(df_linear10_sqrt,pch=20, which=c(2), col= "darkred")

##################################################################################

### Simple Linear Regression on Variables from the Multiple Regression Model

#Linear Regresson on DRB (adj. r^2= .4922)
df_linear1 = lm(PTS ~ DRB)
summary(df_linear1)

#Linear Regresson on AST (adj. r^2= .4268)
df_linear2 = lm(PTS ~ AST)
summary(df_linear2)

#Linear Regresson on PF (adj. r^2= .3375)
df_linear3 = lm(PTS ~ PF)
summary(df_linear3)

#Linear Regresson on FT_perc (adj. r^2= .07255)
df_linear4 = lm(PTS ~ FT_perc)
summary(df_linear4)

#Linear Regresson on three_pt_perc (adj. r^2= .05449)
df_linear5 = lm(PTS ~ three_pt_perc)
summary(df_linear5)

#Linear Regresson on two_pt_perc (adj. r^2= .03802)
df_linear6 = lm(PTS ~ two_pt_perc)
summary(df_linear6)

###############################################################

### Simple Linear Regression on other Variables with best adj. R^2
#Linear Regresson on FG (adj. r^2= .9791)
df_linear7 = lm(PTS ~ FG)
summary(df_linear7)

#Linear Regresson on FGA (adj. r^2= .9637)
df_linear8 = lm(PTS ~ FGA)
summary(df_linear8)


#Linear Regresson on two_pt (adj. r^2= .7924)
df_linear11 = lm(PTS ~ two_pt)
summary(df_linear11)

#Linear Regresson on FTA (adj. r^2= .7804)
df_linear12 = lm(PTS ~ FTA)
summary(df_linear12)

#Linear Regresson on TOV (adj. r^2= .6852)
df_linear13 = lm(PTS ~ TOV)
summary(df_linear13)