#comments coming later
install.packages("factoextra")
library(factoextra)
library(moments)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(GGally)
library(ROCR)
library(dotwhisker)
c = read.csv("Credit.csv")
View(c)
credit = data.frame(c)
View(credit)
str(credit)
nrow(credit)
ncol(credit)
View(credit)

#variable preprocessing
credit$Own = as.factor(credit$Own)
credit$Student = as.factor(credit$Student)
credit$Married = as.factor(credit$Married)
credit$Region = as.factor(credit$Region)
credit$Own = as.integer(credit$Own)
credit$Student = as.integer(credit$Student)
credit$Married = as.integer(credit$Married)
credit$Region = as.integer(credit$Region)

#statistical information
credit %>% summarise_if(is.numeric, mean)
credit %>% summarise_if(is.numeric, sd)
credit %>% summarise_if(is.numeric, min)
credit %>% summarise_if(is.numeric, max)
credit %>% summarise_if(is.numeric, median)

















credit = scale(credit)
png(file = "output.png")
km = kmeans(credit, centers = 4, nstart = 25)


fviz_cluster(km, data = credit)


dev.off()

png(file = "output2.png")

km = kmeans(credit, centers = 5, nstart = 25)


fviz_cluster(km, data = credit)

dev.off()
str(credit)
ggcorr(credit)
