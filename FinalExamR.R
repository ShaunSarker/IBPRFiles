#Question 2
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(caTools)
library(dplyr)
library(ggplot2)
library(GGally)
mel = read.csv("mel_housing.csv")

View(mel)
mel$Type = as.factor(mel$Type)
mel$Regionname = as.factor(mel$Regionname)
mel$CouncilArea = as.factor(mel$CouncilArea)
summary(mel$CouncilArea)
str(mel)
ggcorr(mel)
mod1 = lm(Price ~.,data = mel)
summary(mod1)
mel.df = data.frame(mel)
mel.df = select(mel.df, -c(SoldYear, Postcode, Landsize, Propertycount, Regionname, Lattitude))
mel.df = select(mel.df, -Id)
View(mel.df)
str(mel.df)

#below line of code is from Qiqi Jiang 2021, Lecture 5
OSR2 <- function(predictions, test, train) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

set.seed(2014)
dist.train = sample(nrow(mel.df), 0.8*nrow(mel.df))
mel.train= mel.df[dist.train,]
mel.test = mel.df[-dist.train,]
set.seed(2014)
mel.rf = randomForest(Price ~ ., data = mel.train, mtry = 5, nodesize = 5, ntree = 501)
mel.pred.rf = predict(mel.rf, newdata = mel.test)
importance(mel.rf)
hist(mel.pred.rf)
b.mel.train = as.data.frame(model.matrix(Price ~ . + 0, data = mel.train))
b.mel.test = as.data.frame(model.matrix(Price ~ . + 0, data=mel.test)) 
set.seed(2014)
b.model.mel = randomForest(x = b.mel.train, y = mel.train$Price, mtry = 10, nodesize = 5, ntree = 501) #read that using odd numbers helps with finer results
b.pred.mel = predict(b.model.mel, newdata = b.mel.test)


#Question 3
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
hr = read.csv("HRDataset.csv")
hr$Gender = as.factor(hr$Gender)
View(hr)
str(hr)
#assume men and women have the same probabilities/ability to reach similar positions (no workplace sexism for promotions)
statInfo = function(x){
  y = c(min(x), max(x), mean(x), median(x))
}
max(hr$Salary)
min(hr$Salary)
median(hr$Salary)
mean(hr$Salary)
G.mod = lm(Salary ~ Gender + Engagement + Satisfaction + Motivation , data = hr,)
S.mod = lm(Satisfaction ~ Salary, data = hr )
summary(S.mod)
summary(G.mod)
male = hr[hr$Gender == 1,]
female = hr[hr$Gender == 0,]
m.statInfo = statInfo(male$Salary)
f.statInfo = statInfo(female$Salary)
hr.statInfo = statInfo(hr$Salary)
m.statInfo
f.statInfo
hr.statInfo


ggplot(data = hr) + 
  geom_point(mapping = aes(x = Gender, y = Salary))
ggplot(data = hr) + 
  geom_point(mapping = aes(x = Salary, y = Engagement), colour = 'red')
