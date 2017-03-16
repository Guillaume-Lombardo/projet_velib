#library
library(glmnet)
library(caret)
library(e1071)
library(randomForest)
library(gbm)

k<-6
scale<-1

    #importations des données
    #########################
    
    #variables explicatives
    var_expli<-read.csv("Sortie/Jeuvarexplifinal.csv", sep=",")
    var_expli$X.1<-NULL
    var_expli$X<-NULL
    
    #clustering à expliquer
    url<-paste0("Sortie/clustering_",k,"_classes_mod7j.csv")
    cluster<-read.csv(url, sep=";")
    
    data<-merge(cluster, var_expli)
    data$number<-NULL
    data$lat<-NULL
    data$lon<-NULL
    data$cluster<-as.factor(data$cluster)
    Xapp<-as.matrix(data[,-1])
    if (scale){
      Xapp<-scale(Xapp)
    }
    Y<-data[,1]

    
#importations des données version alternative
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
which(tolower(substr(names(data),1,3)) %in% c('lon','lat'))

# Modélisation random forest
#################
randomForest_test <- randomForest(x = data[,c(-1,-which(tolower(substr(names(data),1,3)) %in% c('lon','lat')))],
                                  y = data[,1], method = 'rf',importance=TRUE)
# attributes(randomForest_test)
# randomForest_test$call
# randomForest_test$type
table_croisee <- table(randomForest_test$predicted,data[,1])
effectif_classe <- sapply(1:6, function(.x) sum(data[,1]==.x))
table_croisee_pct <- sapply(1:6, function(.x) round(table_croisee[,.x]*100/effectif_classe[.x],1))
table_croisee_pct
# randomForest_test$err.rate
# randomForest_test$confusion
# randomForest_test$votes
# randomForest_test$oob.times
# randomForest_test$classes
# randomForest_test$importance
# head(randomForest_test$importance[order(randomForest_test$importance[,7],decreasing = T),7],20)
round(100*randomForest_test$importance[order(randomForest_test$importance[,7],decreasing = T),7]/max(cumsum(randomForest_test$importance[order(randomForest_test$importance[,7],decreasing = T),7])),1)
barplot(head(round(100*randomForest_test$importance[order(randomForest_test$importance[,7],decreasing = T),7]/max(cumsum(randomForest_test$importance[order(randomForest_test$importance[,7],decreasing = T),7])),1),15))
# randomForest_test$ntree
# randomForest_test$mtry
# randomForest_test$y

# Modélisation SVM
#################

SVM_rad <- train(x = data[,c(-1,-which(tolower(substr(names(data),1,3)) %in% c('lon','lat')))],
                 y = data[,1], method = 'svmRadial')
# attributes(SVM_rad)
# SVM_rad$method
# SVM_rad$modelInfo
# SVM_rad$modelType
# SVM_rad$results
# SVM_rad$pred
bestTune <- SVM_rad$bestTune
# SVM_rad$call
# SVM_rad$dots
# SVM_rad$metric
# SVM_rad$control
# SVM_rad$finalModel
# SVM_rad$preProcess
# SVM_rad$trainingData
# SVM_rad$resample
# SVM_rad$resampledCM
# SVM_rad$perfNames
# SVM_rad$maximize
# SVM_rad$yLimits
# SVM_rad$times
# SVM_rad$levels

SVM_rad_opt <- ksvm(x =  as.matrix(data[,c(-1,-which(tolower(substr(names(data),1,3)) %in% c('lon','lat')))]),
                    y = data[,1], C=bestTune[1,2],sigma = bestTune[1,1])
# attributes(SVM_rad_opt)

table_croisee_svm <- table(SVM_rad_opt@fitted,data[,1])
table_croisee_svm_pct <- sapply(1:6, function(.x) round(table_croisee_svm[,.x]*100/effectif_classe[.x],1))
table_croisee_svm_pct


#SVM radial avec tune.svm
##########################
# SVM_rad<-tune.svm(x = Xapp, y = Y,kernel="radial",scale=FALSE,gamma=10^(-1:2),cost=10^(-3:1))
# SVM_rad_opt<-SVM_rad$best.model
# Yprev<-SVM_rad_opt@fitted
# confusionSVMrad<-as.data.frame.matrix(table(Y,Yprev))

#SVM radial avec carret mais une autre grille d'optimisation
##############################################################
# paramgrid <- data.frame(cbind(sigma=rep(10^(-2:2),times=5),C=rep(10^(-3:1),each=5)))
# 
# SVM_rad <- train(x = Xapp, y = Y, method = 'svmRadial',
#                  trControl=trainControl(method="cv",number=10,search="grid"),
#                  tuneGrid=paramgrid)
# bestTune <- SVM_rad$bestTune
# SVM_rad_opt <- svm(x = Xapp, y = Y, 
#                    cost=bestTune[1,2],gamma = bestTune[1,1])




