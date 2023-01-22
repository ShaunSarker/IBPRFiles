library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(caTools)
library(ggplot2)
library(GGally)
install.packages("caTool")
library(caTools)
level = c("No", "Yes")
plevel = c("Negative", "Positive")
glevel = c("Female", "Male") #Setting Female is default and Male as additive
d = read.csv("dia.csv")
dia = as.data.frame(d)

View(dia)
is.na(dia)
summary(dia)
dia$Gender = factor(dia$Gender, levels = c("Female", "Male"))
str(dia)
#have to make all the Char data into factors since it is yes or no, for some reason with Gender it was creating 51 levels 
dia[sapply(dia, is.character)] = lapply(dia[sapply(dia, is.character)],
                                        as.factor)
str(dia)
dia[sapply(dia, is.factor)] = lapply(dia[sapply(dia, is.factor)],
                                        as.numeric)
str(dia)
cor(dia)
dia.p = dia[dia$class == 2,]
barplot(table(dia$Gender), names.arg=c('Female','Male'), 
        main='Distribution of gender', xlab='Gender', ylab='Count')
barplot(table(dia.p$Gender), names.arg=c('Female','Male'), 
        main='Distribution of gender (Positive case)', xlab='Gender', ylab='Count')
barplot(table(dia.p$Polyuria), names.arg=c('Negative','Positive'), 
        main='Polyuria relationship with Diabetes ', xlab='Subjects with and without Polyuria', ylab='Count')
barplot(table(dia.p$Polydipsia), names.arg=c('Negative','Positive'), 
        main='Polydipsia relationship with Diabetes ', xlab='Subjects with and without Polyuria', ylab='Count')


set.seed(2022)
shuffle = sample(nrow(dia))
dia = dia[shuffle,]
View(dia)
ggcorr(dia)
mean(dia$Gender)
sapply(dia, mean, na.rm = T)         
lapply(dia, mean, na.rm = T)


res = cor(dia)

set.seed(6161)

dist.train = sample.split(dia, SplitRatio = 0.7)
dia.train= subset(dia, dist.train == TRUE)
dia.test = subset(dia, dist.train == FALSE)


print(dim(dia.train))
print(dim(dia.test))


dia.model = glm(class ~ ., data = dia.train)
summary(dia.model)

prop.table(table(dia.train$class))

#baseline accuracy is 0.62 so 62% baseline accuracy

dia.predict = predict(dia.model, data = dia.train, type = "response")
table(dia.train$class, dia.predict >= 0.9)
(127+210-1)/nrow(dia.train)
#99.4% accuracy for the training data

dia.predict.test = predict(dia.model, data = dia.test, type = "response")

summary(dia.predict.test)

table(dia.test$class, dia.predict.test >= 0.5)

#below codeblock is taken directly from R-CodeCSII from MLDB

testingProb = ifelse(dia.predict.test>0.5,1,0)
mean(pre_48)
