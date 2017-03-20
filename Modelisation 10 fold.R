#library
library(glmnet)
library(caret)
library(e1071)
library(randomForest)


ACPcluster<-0
varexpliACP<-0

if (ACPcluster==0){
  if (varexpliACP == 0) {
    urlclusteretvarexpli<-"Confusion/ClusternoACPvenoACP"
  }
  else{
    urlclusteretvarexpli<-"Confusion/ClusternoACPveACP"   
  }
} else {
  if (varexpliACP == 0) {
    urlclusteretvarexpli<-"Confusion/ClusterACPvenoACP"  
  }
  else{
    urlclusteretvarexpli<-"Confusion/ClusterACPveACP"
  }  
}



# boucle sur le nombre de cluster et le scaling 
# qui produit tous les modèles
# et qui génère la table des biens classés par modèles

# K de la CV si hors de la boucle
K<-1

# nombre de cluster si hors de la boucle
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
      if (varexpliACP == 0) {
        var_expli<-read.csv("Sortie/Jeuvarexplifinal.csv", sep=",")
      } else {
        var_expli<-read.csv("Sortie/JeuvarexplifinalACP.csv", sep=",")
      }
      var_expli$X<-NULL
      
      #clustering à expliquer
      if (ACPcluster==0){
        url<-paste0("Sortie/clustering_",k,"_classes_mod7j.csv") 
      } else {
        url<-paste0("Sortie/clustering_ACP_",k,"_classes_mod7j.csv") 
      }
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
      if (scale){
        X<-scale(X)
      }
      Xapp<-X[ECHANTILLON!=K,]
      Xest<-X[ECHANTILLON==K,]
      Yapp<-data[ECHANTILLON!=K,1]
      Yest<-data[ECHANTILLON==K,1]
      Y<-c(Y,Yest)

      
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
      paramgrid <- data.frame(cbind(sigma=rep(10^(-2:1),times=5),C=rep(10^(-3:1),each=4)))
      
      SVM_rad <- train(x = Xapp, y = Yapp, method = 'svmRadial',
                       trControl=trainControl(method="cv",number=10,search="grid"),
                       tuneGrid=paramgrid)
      bestTune <- SVM_rad$bestTune
      SVM_rad_opt <- svm(x = Xapp, y = Yapp,
                         cost=bestTune[1,2],gamma = bestTune[1,1])
      YprevSVM_rad<-c(YprevSVM_rad,predict(SVM_rad_opt, Xest, type="class"))
   
      #SVM linear
      #############
      # paramgrid = data.frame(C=c(seq(0.01,0.1,by=0.01),seq(0.2,1,by=0.1)))
      # 
      # SVM_lin <- train(x = Xapp, y = Yapp, method = 'svmLinear',
      #                  trControl=trainControl(method="cv",number=10,search="grid"),
      #                  tuneGrid=paramgrid)
      # bestTune <- SVM_lin$bestTune
      # SVM_lin_opt <- svm(x = Xapp, y = Yapp,
      #                    cost=bestTune[1,1])
      # 
      # YprevSVM_lin<-c(YprevSVM_lin,predict(SVM_lin_opt, Xest, type="class"))

      
    }
    confusionlasso<-as.data.frame.matrix(table(Y,Yprevlasso))[,paste0(1:k)]
    biensclasses[1+scale,k-1]<-round(100*sum(diag(as.matrix(confusionlasso)))/sum(confusionlasso),digits=1)
    saveRDS(confusionlasso, file = paste0(urlclusteretvarexpli,"/confusionLasso",k,scale,".RDS"))

    confusionridge<-as.data.frame.matrix(table(Y,Yprevridge))[,paste0(1:k)]
    biensclasses[3+scale, k-1]<-round(100*sum(diag(as.matrix(confusionridge)))/sum(confusionridge),digits=1)
    saveRDS(confusionridge, file = paste0(urlclusteretvarexpli,"/confusionRidge",k,scale,".RDS"))

    confusionelasticnet<-as.data.frame.matrix(table(Y,Yprevelasticnet))[,paste0(1:k)]
    biensclasses[5+scale,k-1]<-round(100*sum(diag(as.matrix(confusionelasticnet)))/sum(confusionelasticnet),digits=1)
    saveRDS(confusionelasticnet, file = paste0(urlclusteretvarexpli,"/confusionElasticnet",k,scale,".RDS"))

    confusionRF<-as.data.frame.matrix(table(Y,Yprevrandomforest))[,paste0(1:k)]
    biensclasses[7+scale,k-1]<-round(100*sum(diag(as.matrix(confusionRF)))/sum(confusionRF),digits=1)
    saveRDS(confusionRF, file = paste0(urlclusteretvarexpli,"/confusionRandomForest",k,scale,".RDS"))

    confusionSVMrad<-as.data.frame.matrix(table(Y,YprevSVM_rad))[,paste0(1:k)]
    biensclasses[9+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMrad)))/sum(confusionSVMrad),digits=1)
    saveRDS(confusionSVMrad, file = paste0(urlclusteretvarexpli,"/confusionSVMRadial",k,scale,".RDS"))

    # confusionSVMlin<-as.data.frame.matrix(table(Y,YprevSVM_lin))[,paste0(1:k)]
    # biensclasses[11+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMlin)))/sum(confusionSVMlin),digits=1)
    # saveRDS(confusionSVMlin, file = paste0(urlclusteretvarexpli,"/confusionSVMLinear",k,scale,".RDS"))
  }
  
}



saveRDS(biensclasses,paste0(urlclusteretvarexpli,"/Biensclasses.RDS"))
