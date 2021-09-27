library(ggplot2) 
library(MASS) 
library(corrplot) 
library(dplyr) 
library(plyr)
library(ISLR)
library(tidyverse)
library(tree)
library(reshape2)
library(boot)
library(readr)
library(psych)

library(caTools)
framingham <- read_csv("C:/Users/adeol/Downloads/My Thesis/222487_478477_bundle_archive (1)/framingham.csv")
dim(framingham)
colSums(is.na(framingham))
framingham =na.omit(framingham)
dim(framingham)
framingham$male <- as.factor(framingham$gender)
framingham$currentSmoker <-as.factor(framingham$currentSmoker)

#data visuation
boxplot(framingham$education)
boxplot(framingham$cigsPerDay)
boxplot(framingham$BPMeds)
boxplot(framingham$totChol)
boxplot(framingham$BMI)
boxplot(framingham$heartRate)
boxplot(framingham$glucose)
ggplot(data=framingham)+ geom_bar (mapping = aes (x  = gender))




# Descriptive Statistics
table(framingham$gender)
table(framingham$education)
summary(framingham$age); summary(framingham$education)
summary(framingham$cigsPerDay)
summary(framingham$totChol); summary(framingham$systolicBloodPressure); summary(framingham$diastolicBloodPressure)
summary(framingham$BMI); summary(framingham$heartRate); summary(framingham$glucose)

library(ggcorrplot)
corr <- cor(framingham[c(-1,-4)])
corr

ggcorrplot(corr,hc.order = TRUE, type = "lower",
           lab = TRUE)

corr [,"TenYearCHD"]

 describe(framingham)

#Visualization

plot(framingham$age, framingham$systolicBloodPressure)
points(framingham$age, framingham$systolicBloodPressure, col = ifelse(framingham$gender == 0, "blue", "red"), pch = 19)
abline(h = 190, col = "black")
legend("topleft", 
       inset = 0.05,
       legend = c("male", "female"),
       col = c("blue", "red"),
       bg = "gray",
       lwd = c(6, 6))

plot(framingham$age, framingham$diastolicBloodPressure)
points(framingham$age, framingham$diastolicBloodPressure, col = ifelse(framingham$gender == 0, "blue", "red"), pch = 19)
abline(h = 100, col = "black")
legend("topleft", 
       inset = 0.05,
       legend = c("male", "female"),
       col = c("blue", "red"),
       bg = "gray",
       lwd = c(6, 6))

#Splitting data
set.seed(30)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.75)
train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)
nrow(train)
nrow(test)

#LOGISTIC REGRESSION WITH MULTIPLE FEATURES
#full model
logistic_model <- glm(TenYearCHD~., data = train, family = "binomial")
summary(logistic_model)

#Reduced model
logistic_model2 <- glm(TenYearCHD~. -education-diabetes-diastolicBloodPressure -heartRate -currentSmoker-prevalentStroke -prevalentHyp-totChol-BMI
                      , data = train, family = "binomial")
summary(logistic_model2)

#Predicting TenYearCHD on test data
pred_model <- predict(logistic_model,newdata = test[,-16],type = "response")
summary(pred_model)

pred_test <- ifelse(pred_model>0.5,1,0)
summary(pred_test)

#Confusion Matrix
confusion_matrix <- table(pred_test,test$TenYearCHD)
confusion_matrix

#Overall Accuracy with threshold 0.5
acc <- (sum(diag(confusion_matrix))/sum(confusion_matrix))*100
paste("Overall accuracy with threshold 0.5 is",acc,"%")

#Accuracy for those who have 10 year risk of coronary heart disease with threshold 0.1
pred_test_data_lowthreshold <- ifelse(pred_model>0.1,1,0)

confusion_matrix <- table(pred_test_data_lowthreshold,test$TenYearCHD)
confusion_matrix


acc_for_1 <- (117/(117+22))*100
paste("Accuracy for those who have 10 year risk of coronary heart disease with threshold 0.1 is",acc_for_1,"%")

#spitting the data into train data and test data for validation set approach

set.seed(1)
train=sample(3656,914)
dat.train=framingham[train,]
dim(dat.train)

dat.test=framingham[-train,]
dim(dat.test)

head(dat.test)
# LDA
library(lda)
lda.fit=lda(TenYearCHD~.,data=dat.train)
lda.fit
plot(lda.fit)
framingham.test=framingham$TenYearCHD[-train]

# Predict in LDA

lda.pred=predict(lda.fit,dat.test)
lda.class=lda.pred$class

#.the confusion matrix

table(lda.pred$class,dat.test$TenYearCHD)

acc <- (sum(diag(confusion_matrix))/sum(confusion_matrix))*100
paste("Overall accuracy with threshold 0.5 is",acc,"%")


# Misclassification error (test error) rate
mcer_lda=1-mean(lda.class==framingham.test)
mcer_lda

###############################################

framingham$male <- as.numeric(framingham$gender)
framingham$currentSmoker <-as.numeric(framingham$currentSmoker)

##******
set.seed(2)
framingham["fram_f"]=as.factor(framingham$TenYearCHD)
framingham= framingham[,-15]
train=sample(3656,914) 
train.dat=framingham[train,]
test.dat = framingham[-train, ]
tree.fram = tree(fram_f~.,train.dat)
cv.framingham= cv.tree(tree.fram,FUN=prune.misclass)
plot(cv.framingham$size,cv.framingham$dev,type="b")

prune.fram=prune.misclass(tree.fram, best=6)
plot(prune.fram)
text(prune.fram, pretty = 0)
pruned.tree.pred= predict(prune.fram,test.dat,type="class")

table(pruned.tree.pred,(test.dat$fram_f))

1-mean(pruned.tree.pred==as.factor(test.dat$fram_f))


#Bagging
library(randomForest)
set.seed(3)
bag.framingham=randomForest(fram_f~., data= framingham,mtry=16, importance=TRUE)
bag.framingham

varImpPlot(bag.framingham)

##Random forest

library(randomForest)
set.seed(31)
randomforest.framingham= randomForest(fram_f~., data= framingham,mtry=2,important=TRUE)
randomforest.framingham

varImpPlot(randomforest.framingham)

# Boosting
library(C50)
boost.framingham<- C5.0(train.dat[,c(1:14)],train.dat[,15],trials=6)
pred<-predict(boost.framingham,test.dat[,c(1:14)])
table(pred,test.dat$fram_f)

1-mean(pred==test.dat$fram_f)











