#library
library(glmnet)

#le nombre de clusters retenus
#############################
k<-6

#est ce qu'on scale les variables ?
####################################
scale<-1

for (k in 2:10)
{
  for (scale in 0:1)
  {
    
    
    #importations des données
    #########################
    
    #variables explicatives
    var_expli<-read.csv("Sortie/Jeuvarexplifinal.csv", sep=",")
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
    
    #lasso
    ########
    Xapp<-as.matrix(data[,-1])
    if (scale){
      #saveRDS(Xapp, file = "Modeles/Xnonscale.RDS")
      Xapp<-scale(Xapp)
    }
    #saveRDS(Xapp2, file = "Modeles/Xscale.RDS")
    Y<-data[,1]
    url<-paste0("Modeles/Y",k,".RDS")
    saveRDS(Y, file = url)
    
    #on fait tourner le lasso
    lasso=cv.glmnet(Xapp,Y,family="multinomial",alpha=1)
    url<-paste0("Modeles/Lasso",k,scale,".RDS")
    saveRDS(lasso,url)
    
    lambdachoisi=lasso$lambda.1se
    
    # #représentation des coefficients les plus importants
    # out2<-coef(lasso)
    # out22<-abs(out2[[1]])
    # for (i in 2:6)
    # {
    #   out22<-out22+abs(out2[[i]])
    #   
    # }
    # outend<-as.data.frame(as.matrix(out22))
    # outend$X2<-row.names(outend)
    # outend<-outend[order(outend[,1], decreasing=T),]
    # barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])
    # amBarplot(x="X2", y="1", data=outend[1:10,])
    # 
    # 
    # #table de confusion
    # Yprev<-predict(lasso, Xapp, type="class",s=lambdachoisi)
    # 
    # confusionlasso<-as.data.frame.matrix(table(Y,Yprev))
    # sommel<-apply(confusionlasso, 1, sum)
    # confusionlasso2<-round(100*apply(confusionlasso, MARGIN=2, sommel, FUN="/"),digits=0)
    # confusionlasso2
    # # sum(diag(confusionlasso))/sum(confusionlasso)
    
    #ridge
    ########
    
    #on fait tourner le lasso
    ridge=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
    url<-paste0("Modeles/Ridge",k,scale,".RDS")
    saveRDS(ridge,url)
    
    # lambdachoisi=ridge$lambda.1se
    # 
    # #représentation des coefficients les plus importants
    # out2<-coef(ridge)
    # out22<-abs(out2[[1]])
    # for (i in 2:6)
    # {
    #   out22<-out22+abs(out2[[i]])
    #   
    # }
    # outend<-as.data.frame(as.matrix(out22))
    # outend$X2<-1
    # outend<-outend[order(outend[,1], decreasing=T),]
    # barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])
    # 
    # 
    # #table de confusion
    # Yprev<-predict(ridge, Xapp, type="class",s=lambdachoisi)
    # confusionridge<-table(Y,Yprev)
    # sommel<-apply(confusionridge, 1, sum)
    # confusionridge2<-round(100*apply(confusionridge, MARGIN=2, sommel, FUN="/"),digits=0)
    # confusionridge2
    # # sum(diag(confusionridge))/sum(confusionridge)
    
    #elasticnet
    ############
    
    #on fait tourner le lasso
    elasticnet=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
    url<-paste0("Modeles/Elasticnet",k,scale,".RDS")
    saveRDS(elasticnet,url)
    
    lambdachoisi=elasticnet$lambda.1se
    
    # #représentation des coefficients les plus importants
    # out2<-coef(elasticnet)
    # out22<-abs(out2[[1]])
    # for (i in 2:6)
    # {
    #   out22<-out22+abs(out2[[i]])
    #   
    # }
    # outend<-as.data.frame(as.matrix(out22))
    # outend$X2<-1
    # outend<-outend[order(outend[,1], decreasing=T),]
    # barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])
    # 
    # 
    # #table de confusion
    # Yprev<-predict(elasticnet, Xapp, type="class",s=lambdachoisi)
    # confusionelasticnet<-table(Y,Yprev)
    # sommel<-apply(confusion, 1, sum)
    # confusionelasticnet2<-round(100*apply(confusionelasticnet, MARGIN=2, sommel, FUN="/"),digits=0)
    # confusionelasticnet2
    # # sum(diag(confusionelasticnet))/sum(confusionelasticnet)
    
  }
}
