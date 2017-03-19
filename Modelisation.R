#library
library(glmnet)
library(caret)
library(e1071)
library(randomForest)

# boucle sur le nombre de cluster et le scaling 
# qui produit tous les modèles
# et qui génère la table des biens classés par modèles

ACPcluster<-1
varexpliACP<-0

if (ACPcluster==0){
  if (varexpliACP == 0) {
    urlclusteretvarexpli<-"Modeles/ClusternoACPvenoACP"
  }
  else{
    urlclusteretvarexpli<-"Modeles/ClusternoACPveACP"   
  }
} else {
  if (varexpliACP == 0) {
    urlclusteretvarexpli<-"Modeles/ClusterACPvenoACP"  
  }
  else{
    urlclusteretvarexpli<-"Modeles/ClusterACPveACP"
  }  
}

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

for (k in 2:10)
{
  for (scale in 0:1)
  {
  
    #importations des données
    #########################
    
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
    Xapp<-as.matrix(data[,-1])
    saveRDS(Xapp, file = paste0(urlclusteretvarexpli,"/Xnonscale.RDS"))
    if (scale){
      Xapp<-scale(Xapp)
      saveRDS(Xapp, file = paste0(urlclusteretvarexpli,"/Xscale.RDS"))
    }
    Y<-data[,1]
    url<-paste0(urlclusteretvarexpli,"/Y",k,".RDS")
    saveRDS(Y, file = url)
    
    #LASSO
    ######
    lasso<-cv.glmnet(Xapp,Y,family="multinomial",alpha=1)
    url<-paste0(urlclusteretvarexpli,"/Lasso",k,scale,".RDS")
    saveRDS(lasso,url)
    
    #table de confusion
    Yprev<-predict(lasso, Xapp, type="class",s=lasso$lambda.1se)
    confusionlasso<-as.data.frame.matrix(table(Y,Yprev))
    biensclasses[1+scale,k-1]<-round(100*sum(diag(as.matrix(confusionlasso)))/sum(confusionlasso),digits=1)
    
    #RIDGE
    ######
    ridge=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
    url<-paste0(urlclusteretvarexpli,"/Ridge",k,scale,".RDS")
    saveRDS(ridge,url)  
    Yprev<-predict(ridge, Xapp, type="class")
    confusionridge<-as.data.frame.matrix(table(Y,Yprev))
    biensclasses[3+scale, k-1]<-round(100*sum(diag(as.matrix(confusionridge)))/sum(confusionridge),digits=1)
    
    
    #ELASTICNET
    ############
    elasticnet=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
    url<-paste0(urlclusteretvarexpli,"/Elasticnet",k,scale,".RDS")
    saveRDS(elasticnet,url)
    Yprev<-predict(elasticnet, Xapp, type="class")
    confusionelasticnet<-as.data.frame.matrix(table(Y,Yprev))
    biensclasses[5+scale,k-1]<-round(100*sum(diag(as.matrix(confusionelasticnet)))/sum(confusionelasticnet),digits=1)
    
    #RANDOMFOREST
    #############
    randomForest <- randomForest(x=Xapp, y=Y, method = 'rf',importance=TRUE)   
    url<-paste0(urlclusteretvarexpli,"/RandomForest",k,scale,".RDS")
    saveRDS(randomForest,url)
    Yprev<-randomForest$predicted
    confusionRF<-as.data.frame.matrix(table(Y,Yprev))
    biensclasses[7+scale,k-1]<-round(100*sum(diag(as.matrix(confusionRF)))/sum(confusionRF),digits=1)
    
    
    #SVM radial
    #############    
    # paramgrid <- data.frame(cbind(sigma=rep(10^(-2:1),times=4),C=rep(10^(-3:1),each=5)))
    # 
    # SVM_rad <- train(x = Xapp, y = Y, method = 'svmRadial',
    #                  trControl=trainControl(method="cv",number=10,search="grid"),
    #                  tuneGrid=paramgrid)
    # bestTune <- SVM_rad$bestTune
    # SVM_rad_opt <- svm(x = Xapp, y = Y, 
    #                     cost=bestTune[1,2],gamma = bestTune[1,1])
    # url<-paste0(urlclusteretvarexpli,"/SVMRadial",k,scale,".RDS")
    # saveRDS(SVM_rad_opt,url)
    # Yprev<-SVM_rad_opt$fitted
    # confusionSVMrad<-as.data.frame.matrix(table(Y,Yprev))
    # biensclasses[9+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMrad)))/sum(confusionSVMrad),digits=1)
    # 
    # 
    # #SVM polynomial
    # #############  
    # paramgrid = data.frame(C=c(seq(0.01,1,by=0.05),1))
    # 
    # SVM_lin <- train(x = Xapp, y = Y, method = 'svmLinear',
    #                  trControl=trainControl(method="cv",number=10,search="grid"),
    #                  tuneGrid=paramgrid)
    # bestTune <- SVM_lin$bestTune
    # SVM_lin_opt <- svm(x = Xapp, y = Y, 
    #                    cost=bestTune[1,1])
    # 
    # # SVM_lin<-tune.svm(x = Xapp, y = Y,kernel="linear",scale=FALSE,cost=10^(-3:1))
    # # SVM_lin_opt<-SVM_lin$best.model
    # url<-paste0(urlclusteretvarexpli,"/SVMLinear",k,scale,".RDS")
    # saveRDS(SVM_lin_opt,url)
    # Yprev<-SVM_lin_opt$fitted
    # confusionSVMlin<-as.data.frame.matrix(table(Y,Yprev))
    # biensclasses[11+scale,k-1]<-round(100*sum(diag(as.matrix(confusionSVMlin)))/sum(confusionSVMlin),digits=1)
    # 
    
  }
}

saveRDS(biensclasses,paste0(urlclusteretvarexpli,"/Biensclasses.RDS"))
