#library
library(glmnet)


#importations des données
#########################

#variables explicatives
var_expli<-read.csv("Sortie/Jeuvarexplifinal.csv", sep=",")
var_expli$X<-NULL

#clustering à expliquer
cluster<-read.csv("Sortie/clustering_6_classes_mod7j.csv", sep=";")

data<-merge(cluster, var_expli)
data$number<-NULL
data$lat<-NULL
data$lon<-NULL
data$cluster<-as.factor(data$cluster)


# data<-data[,1:12]
# summary(data)
# sapply(data[,-1], FUN=as.double)


# Modélisation
#################
#################

#lasso
########
Xapp<-as.matrix(data[,-1])
#saveRDS(Xapp, file = "Modeles/Xnonscale.RDS")
Xapp2<-scale(Xapp)
#saveRDS(Xapp2, file = "Modeles/Xscale.RDS")
Y<-data[,1]

#on fait tourner le lasso
lasso=cv.glmnet(Xapp2,Y,family="multinomial",alpha=1)
lambdachoisi=lasso$lambda.1se


#représentation des coefficients les plus importants
out2<-coef(lasso)
out22<-abs(out2[[1]])
for (i in 2:6)
{
  out22<-out22+abs(out2[[i]])
  
}
outend<-as.data.frame(as.matrix(out22))
outend$X2<-row.names(outend)
outend<-outend[order(outend[,1], decreasing=T),]
barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])

amBarplot(x="X2", y="1", data=outend[1:10,])


#table de confusion
Yprev<-predict(lasso, Xapp2, type="class",s=lambdachoisi)

confusionlasso<-as.data.frame.matrix(table(Y,Yprev))
sommel<-apply(confusionlasso, 1, sum)
confusionlasso2<-round(100*apply(confusionlasso, MARGIN=2, sommel, FUN="/"),digits=0)
confusionlasso2
# sum(diag(confusionlasso))/sum(confusionlasso)

#ridge
########

#on fait tourner le lasso
ridge=cv.glmnet(Xapp2,Y,family="multinomial",alpha=0)
lambdachoisi=ridge$lambda.1se

#représentation des coefficients les plus importants
out2<-coef(ridge)
out22<-abs(out2[[1]])
for (i in 2:6)
{
  out22<-out22+abs(out2[[i]])

}
outend<-as.data.frame(as.matrix(out22))
outend$X2<-1
outend<-outend[order(outend[,1], decreasing=T),]
barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])


#table de confusion
Yprev<-predict(ridge, Xapp2, type="class",s=lambdachoisi)
confusionridge<-table(Y,Yprev)
sommel<-apply(confusionridge, 1, sum)
confusionridge2<-round(100*apply(confusionridge, MARGIN=2, sommel, FUN="/"),digits=0)
confusionridge2
# sum(diag(confusionridge))/sum(confusionridge)

#elasticnet
############

#on fait tourner le lasso
elasticnet=cv.glmnet(Xapp2,Y,family="multinomial",alpha=0)
lambdachoisi=elasticnet$lambda.1se

#représentation des coefficients les plus importants
out2<-coef(elasticnet)
out22<-abs(out2[[1]])
for (i in 2:6)
{
  out22<-out22+abs(out2[[i]])
  
}
outend<-as.data.frame(as.matrix(out22))
outend$X2<-1
outend<-outend[order(outend[,1], decreasing=T),]
barplot(outend[1:10,1], names.arg=row.names(outend)[1:10])


#table de confusion
Yprev<-predict(elasticnet, Xapp2, type="class",s=lambdachoisi)
confusionelasticnet<-table(Y,Yprev)
sommel<-apply(confusion, 1, sum)
confusionelasticnet2<-round(100*apply(confusionelasticnet, MARGIN=2, sommel, FUN="/"),digits=0)
confusionelasticnet2
# sum(diag(confusionelasticnet))/sum(confusionelasticnet)


