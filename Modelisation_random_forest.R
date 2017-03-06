#library
library(glmnet)
library(caret)


#importations des données
#########################

#variables explicatives
var_expli<-read.csv("Sortie/Jeuvarexplifinal.csv", sep=",")
var_expli$X<-NULL

#clustering à expliquer
cluster<-read.csv("Sortie/clustering_6_classes_mod7j.csv", sep=";")

data<-merge(cluster, var_expli)
data$number<-NULL
data$cluster<-as.factor(data$cluster)

#data<-data[,1:12]
# summary(data)
# sapply(data[,-1], FUN=as.double)


# Modélisation
#################
randomForest_test <- train(x = data[,c(-1,-4,-5)],y = data[,1], method = 'rf')


# lasso2<-cv.glmnet

caret_ridge <- train()