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
attributes(randomForest_test)
randomForest_test$method
randomForest_test$modelInfo
randomForest_test$modelType
randomForest_test$results
randomForest_test$pred
randomForest_test$bestTune
randomForest_test$call
randomForest_test$dots
randomForest_test$metric
randomForest_test$control
randomForest_test$finalModel
randomForest_test$preProcess
randomForest_test$trainingData
randomForest_test$resample
randomForest_test$resampledCM
randomForest_test$perfNames
randomForest_test$maximize
randomForest_test$yLimits
randomForest_test$times
randomForest_test$levels

