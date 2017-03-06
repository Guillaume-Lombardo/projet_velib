#library
library(glmnet)


#importations des données
#########################
boxplot(alt~bonus,data = data[,c('alt','bonus')])
class(data$bonus)
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
Y<-data[,1]

#on fait tourner le lasso
lasso=cv.glmnet(Xapp,Y,family="multinomial",alpha=1)
lambdachoisi=lasso$lambda.1se

#représentation des coefficients les plus importants
out2<-coef(lasso)
out22<-abs(out2[[1]])
for (i in 2:6)
{
  out22<-out22+abs(out2[[i]])
  
}
outend<-as.data.frame(as.matrix(out22))
outend$X2<-1
outend<-outend[order(outend[,1], decreasing=T),]
barplot(outend[2:11,1], names.arg=row.names(outend)[2:11])


#table de confusion
Yprev<-predict(lasso, Xapp, type="class",s=lambdachoisi)
confusionlasso<-table(Yprev,Y)
sommel<-apply(confusionlasso, 2, sum)
confusionlasso<-round(100*apply(confusionlasso, MARGIN=1, sommel, FUN="/"),digits=0)
confusionlasso


#ridge
########

#on fait tourner le lasso
ridge=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
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
barplot(outend[2:11,1], names.arg=row.names(outend)[2:11])


#table de confusion
Yprev<-predict(ridge, Xapp, type="class",s=lambdachoisi)
confusionridge<-table(Yprev,Y)
sommel<-apply(confusionridge, 2, sum)
confusionridge<-round(100*apply(confusionridge, MARGIN=1, sommel, FUN="/"),digits=0)
confusionridge

#elasticnet
############

#on fait tourner le lasso
elasticnet=cv.glmnet(Xapp,Y,family="multinomial",alpha=0)
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
barplot(outend[2:11,1], names.arg=row.names(outend)[2:11])


#table de confusion
Yprev<-predict(elasticnet, Xapp, type="class",s=lambdachoisi)
confusionelasticnet<-table(Yprev,Y)
sommel<-apply(confusion, 2, sum)
confusionelasticnet<-round(100*apply(confusionelasticnet, MARGIN=1, sommel, FUN="/"),digits=0)
confusionelasticnet


