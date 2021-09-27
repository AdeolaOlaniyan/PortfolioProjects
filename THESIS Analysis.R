library(ggplot2) 
library(MASS) 
library(corrplot) 
library(dplyr) 
library(plyr)
library(ISLR)
library(tidyverse)
library(tree)
library(reshape2)
library(readr)
framingham <- read_csv("C:/Users/adeol/Downloads/My Thesis/222487_478477_bundle_archive (1)/framingham.csv")
dim(framingham)
is.na(sum(framingham))
      framingham =na.omit(framingham)
dim(framingham)
#EDA

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = male)) + geom_boxplot()+ labs(title="Distribution of gender  by TenYearCHD", x="TenYearCHD", y="gender")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = age)) + geom_boxplot() + labs(title="Distribution of age by TenYearCHD", x="TenYearCHD", y="horsepower")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = education)) + geom_boxplot() + labs(title=" Distribution of education by TenYearCHD", x="TenYearCHD", y="education")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = currentSmoker)) + geom_boxplot() + labs(title=" Distribution of currentSmoker by TenYearCHD", x="TenYearCHD", y="currentSmoker")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = cigsPerDay)) + geom_boxplot()+ labs(title="Distribution of cigsPerDay  by TenYearCHD", x="TenYearCHD", y="cigsPerDay")


ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = prevalentHyp)) + geom_boxplot() + labs(title=" Distribution of prevalentHyp by TenYearCHD", x="TenYearCHD", y="prevalentHyp")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = diabetes)) + geom_boxplot()+ labs(title="Distribution of diabetes  by TenYearCHD", x="TenYearCHD", y="diabetes")


ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = sysBP)) + geom_boxplot() + labs(title=" Distribution of sysBP by TenYearCHD1", x="TenYearCHD", y="sysBP")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = diaBP)) + geom_boxplot() + labs(title=" Distribution of diaBP by TenYearCHD", x="TenYearCHD", y="diaBP")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = BMI)) + geom_boxplot() + labs(title="Distribution of BMI by TenYearCHD", x="TenYearCHD", y="BMI")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = heartRate)) + geom_boxplot() + labs(title=" Distribution of heartRate by TenYearCHD", x="TenYearCHD", y="heartRate")

ggplot(data = framingham, mapping = aes(x = as.character(TenYearCHD), y = glucose)) + geom_boxplot() + labs(title=" Distribution of glucose by TenYearCHD", x="TenYearCHD", y="glucose")


# Descriptive Statistics

summary(framingham$age); summary(framingham$education)
summary(framingham$cigsPerDay); summary(framingham$currentSmoker)
summary(framingham$totChol); summary(framingham$sysBP); summary(framingham$diaBP)
summary(framingham$BMI); summary(framingham$heartRate); summary(framingham$glucose)
                                               

p=ggplot(data = framingham,aes(x=variable,y=value))+
  geom_boxplot(aes(fill=as.factor(TenYearCHD)))
p+facet_wrap(~ variable,scales="free")+
  labs(title = "Distribution features by TenYearCHD",x="",y="")

#LOGISTIC REGRESSION WITH MULTIPLE FEATURES
#full model
fit1 = glm(TenYearCHD ~ male+age+education+currentSmoker+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+
             diabetes+totChol+sysBP+diaBP+BMI+heartRate+glucose, data = framingham, family = binomial)
summary(fit1)
#Reduced model
fit2 = glm(TenYearCHD ~ male+age+cigsPerDay+totChol+sysBP+glucose, data = framingham, family = binomial)
summary(fit2) 

#Validation set approach
#spitting the data into train data and test data

set.seed(1)
train=sample(3656,2438)
dat.train=framingham[train,]
dim(dat.train)

dat.test=framingham[-train,]
dim(dat.test)

head(dat.test)

# LDA

lda.fit=lda(TenYearCHD~.,data=dat.train)
lda.fit
plot(lda.fit)
TenYearCHD.test=framingham$TenYearCHD[-train]

# Predict in LDA

lda.pred=predict(lda.fit,dat.test)
lda.class=lda.pred$class

#.the confusion matrix

table(lda.pred$class,dat.test$TenYearCHD)

# Misclassification error (test error) rate

mcer_lda=1-mean(lda.class==TenYearCHD.test)
mcer_lda

#ROC CURVE

#Performing logistic regression

train=sample(3656,2438) 
train.dat=framingham[train,]
test.dat = framingham[-train, ]
fit = glm(TenYearCHD ~., data=train.dat)
test.error = mean(test.dat$TenYearCHD - predict(fit, newdata = test.dat))^2
test.error


#Construct a classification tree based on the training data
  
set.seed(2)
framingham["TenYearCHD_f"]=as.factor(framingham$TenYearCHD)
framingham= framingham[,-15]
train=sample(3656,2438) 
train.dat=framingham[train,]
test.dat = framingham[-train, ]
tree.framingham = tree(TenYearCHD_f~.,train.dat)
cv.framingham= cv.tree(tree.framingham,FUN=prune.misclass)
plot(cv.framingham$size,cv.framingham$dev,type="b")

prune.framingham=prune.misclass(tree.framingham, best=5)


#Predict based on the pruned classification tree

pruned.tree.pred= predict(prune.framingham,test.dat,type="class")
table(pruned.tree.pred,(test.dat$TenYearCHD_f))

1-mean(pruned.tree.pred==as.factor(test.dat$TenYearCHD_f))

#To address overfitting
##Pruning to optimal 5 is done to address over-fitting in classification tree.#

#STE3: Apply the bagging algorithm**
  
library(randomForest)
set.seed(3)
bag.framingham=randomForest(TenYearCHD_f~., data= framingham,mtry=8, importance=TRUE)
bag.framingham

varImpPlot(bag.framingham)


#Apply the Random Forest algorithm
  
library(randomForest)
set.seed(31)
rf.framingham= randomForest(TenYearCHD_f~., data= framingham,mtry=3,important=TRUE)
rf.framingham

varImpPlot(rf.framingham)


#Apply the boosting algorithm
  
library(C50)
boost.framingham<- C5.0(train.dat[,c(1:15)],train.dat[,16],trials=6)
pred<-predict(boost.framingham,test.dat[,c(1:15)])
table(pred,test.dat$TenYearCHD_f)

1-mean(pred==test.dat$TenYearCHD_f)

##The option trials = 5 indicates three boosting iterations are used. I ran several trials and found that 6 iternations or 6 weak learners provide the lowest misclassification error. For this data, random forests procedure performs better than bagging, boosting aggregating, and a single tree because it has the lowest prediction error.
