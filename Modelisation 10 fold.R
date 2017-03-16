#library
library(glmnet)
library(caret)
library(e1071)
library(randomForest)

# boucle sur le nombre de cluster et le scaling 
# qui produit tous les modèles
# et qui génère la table des biens classés par modèles

K<-1
k<-6

#est ce qu'on scale les variables ?
####################################
scale<-1

biensclasses <- as.data.frame(matrix(0, nrow=2*6, ncol=9))
nomcol<- rep("clusters ", 9)
nomcol<- paste0(nomcol, 2:10)
colnames(biensclasses)<-nomcol
nomlignes=rep(c("Lasso", "Ridge", "Elasticnet", "RandomForest", "SVM-Radial", "SVM-Linear"), each=2)
nomlignes<- paste(nomlignes, c("non scalé", "scalé"))
rownames(biensclasses)<-nomlignes


set.seed(4242)
ECHANTILLON <- sample(rep(c(1:10),round(1199)/10+1))[1:1199]


for (k in 6:6)
{
  for (scale in 1:1)
  {
    Yprevlasso<-c()
    Yprevridge<-c()
    Yprevelasticnet<-c()
    Yprevrandomforest<-c()
    YprevSVM_rad<-c()
    YprevSVM_lin<-c()
    Y<-c()
    
    for (K in 1:10)
    {
      
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
      X<-as.matrix(data[,-1])
      # saveRDS(Xapp, file = "Modeles/Xnonscale.RDS")
      if (scale){
        X<-scale(X)
        # saveRDS(Xapp, file = "Modeles/Xscale.RDS")
      }
      Xapp<-X[ECHANTILLON!=K,]
      Xest<-X[ECHANTILLON==K,]
      Yapp<-data[ECHANTILLON!=K,1]
      Yest<-data[ECHANTILLON==K,1]
      Y<-c(Y,Yest)
      # url<-paste0("Modeles/Y",k,".RDS")
      # saveRDS(Y, file = url)
      
      #LASSO
      ######
      lasso<-cv.glmnet(Xapp,Yapp,family="multinomial",alpha=1)
      Yprevlasso<-c(Yprevlasso,predict(lasso, Xest, type="class",s=lasso$lambda.1se))
  
      #RIDGE
      ######
      ridge=cv.glmnet(Xapp,Yapp,family="multinomial",alpha=0)
      Yprevridge<-c(Yprevridge,predict(ridge, Xest, type="class"))
  
      
      #ELASTICNET
      ############
      elasticnet=cv.glmnet(Xapp,Yapp,family="multinomial",alpha=0)
      Yprevelasticnet<-c(Yprevelasticnet,predict(elasticnet, Xest, type="class"))
    
      #RANDOMFOREST
      #############
      randomForest <- randomForest(x=Xapp, y=Yapp, method = 'rf',importance=TRUE)   
      Yprevrandomforest<-c(Yprevrandomforest,predict(randomForest, Xest, type="class"))
 
      
      #SVM radial
      #############    
      paramgrid <- data.frame(cbind(sigma=rep(10^(-2:1),times=4),C=rep(10^(-3:1),each=5)))
    
      SVM_rad <- train(x = Xapp, y = Yapp, method = 'svmRadial',
                       trControl=trainControl(method="cv",number=10,search="grid"),
                       tuneGrid=paramgrid)
      bestTune <- SVM_rad$bestTune
      SVM_rad_opt <- svm(x = Xapp, y = Yapp, 
                         cost=bestTune[1,2],gamma = bestTune[1,1])
      YprevSVM_rad<-c(YprevSVM_rad,predict(SVM_rad_opt, Xest, type="class"))
   
      #SVM polynomial
      #############  
      paramgrid = data.frame(C=c(seq(0.01,1,by=0.05),1))
      
      SVM_lin <- train(x = Xapp, y = Yapp, method = 'svmLinear',
                       trControl=trainControl(method="cv",number=10,search="grid"),
                       tuneGrid=paramgrid)
      bestTune <- SVM_lin$bestTune
      SVM_lin_opt <- svm(x = Xapp, y = Yapp, 
                         cost=bestTune[1,1])
      
      YprevSVM_lin<-c(YprevSVM_lin,predict(SVM_lin_opt, Xest, type="class"))
    
      
    }
    confusionlasso<-as.data.frame.matrix(table(Y,Yprevlasso))
    biensclasses[1+scale,k-1]<-round(100*sum(diag(as.matrix(confusionlasso)))/sum(confusionlasso),digits=1)
    saveRDS(confusionlasso, file = paste0("Confusion/confusionLasso",k,scale,".RDS"))
    
    confusionridge<-as.data.frame.matrix(table(Y,Yprevridge))
    biensclasses[3+scale, k-1]<-round(100*sum(diag(as.matrix(confusionridge)))/sum(confusionridge),digits=1)
    saveRDS(confusionridge, file = paste0("Confusion/confusionRidge",k,scale,".RDS"))
    
    confusionelasticnet<-as.data.frame.matrix(table(Y,Yprevelasticnet))
    biensclasses[5+scale,k-1]<-round(100*sum(diag(as.matrix(confusionelasticnet)))/sum(confusionelasticnet),digits=1)
    saveRDS(confusionelasticnet, file = paste0("Confusion/confusionElasticnet",k,scale,".RDS"))
    
    confusionRF<-as.data.frame.matrix(table(Y,Yprevrandomforest))
    biensclasses[7+scale,k-1]<-round(100*sum(diag(as.matrix(confusionRF)))/sum(confusionRF),digits=1)
    saveRDS(confusionRF, file = paste0("Confusion/confusionRandomForest",k,scale,".RDS"))
    
    confusionSVMrad<-as.data.frame.matrix(table(Y,YprevSVM_rad))
    biensclasses[9+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMrad)))/sum(confusionSVMrad),digits=1)
    saveRDS(confusionSVMrad, file = paste0("Confusion/confusionSVMRadial",k,scale,".RDS"))
    
    confusionSVMlin<-as.data.frame.matrix(table(Y,YprevSVM_lin))
    biensclasses[11+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMlin)))/sum(confusionSVMlin),digits=1)
    saveRDS(confusionSVMlin, file = paste0("Confusion/confusionSVMLinear",k,scale,".RDS"))
  }
  
}



saveRDS(biensclasses,"Confusion/Biensclasses.RDS")
