#library
library(glmnet)
library(caret)
library(e1071)
library(randomForest)

#nom de la variable à ne pas estimer
nomvar<-"P13_POP2064.x"
n<-3
coln<-rep(3,5)
coln[n]<-2

# boucle sur le nombre de cluster et le scaling 
# qui produit tous les modèles
# et qui génère la table des biens classés par modèles
k<-6

#est ce qu'on scale les variables ?
####################################
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


# Modélisation
#################
#################
Xapp<-as.matrix(data[,-1])
if (scale){
  Xapp<-scale(Xapp)
}
colmoins<-which(colnames(Xapp)==nomvar)
# Xapp<-Xapp[,-colmoins]
Y<-data[,1]


#LASSO
######
lasso<-cv.glmnet(Xapp,Y,family="multinomial",alpha=1)
Yprev<-predict(lasso, Xapp, type="class",s=lasso$lambda.1se)
boxplot(Xapp[,colmoins]~ Y + Yprev)

#RIDGE
######
ridge=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
Yprev<-predict(ridge, Xapp, type="class")
boxplot(Xapp[,colmoins]~ Y + Yprev)

#ELASTICNET
############
elasticnet=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
Yprev<-predict(elasticnet, Xapp, type="class")
boxplot(Xapp[,colmoins]~ Y + Yprev)

#RANDOMFOREST
#############
randomForest <- randomForest(x=Xapp, y=Y, method = 'rf',importance=TRUE)
Yprev<-randomForest$predicted
boxplot(Xapp[,colmoins]~ Y[] + Yprev)

par(mfrow=c(1,2)) 
boxplot(Xapp[,colmoins]~ Y, col=coln)
boxplot(Xapp[Y[]==n,colmoins]~ Yprev[Y[]==n], col=coln)

#SVM radial
#############    
paramgrid <- data.frame(cbind(sigma=rep(10^(-2:1),times=4),C=rep(10^(-3:1),each=5)))

SVM_rad <- train(x = Xapp, y = Y, method = 'svmRadial',
                 trControl=trainControl(method="cv",number=10,search="grid"),
                 tuneGrid=paramgrid)
bestTune <- SVM_rad$bestTune
SVM_rad_opt <- svm(x = Xapp, y = Y, 
                   cost=bestTune[1,2],gamma = bestTune[1,1])
Yprev<-SVM_rad_opt$fitted
boxplot(Xapp[,colmoins]~ Y + Yprev)

#SVM polynomial
#############  
paramgrid = data.frame(C=c(seq(0.01,1,by=0.05),1))

SVM_lin <- train(x = Xapp, y = Y, method = 'svmLinear',
                 trControl=trainControl(method="cv",number=10,search="grid"),
                 tuneGrid=paramgrid)
bestTune <- SVM_lin$bestTune
SVM_lin_opt <- svm(x = Xapp, y = Y, 
                   cost=bestTune[1,1])
Yprev<-SVM_lin_opt$fitted
boxplot(Xapp[,colmoins]~ Y + Yprev)


