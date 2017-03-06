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
data$cluster<-as.factor(data$cluster)

# summary(data)
# sapply(data[,-1], FUN=as.double)


# Modélisation
#################
lasso<-glmnet(as.matrix(data[,-1]), data[,1], family="multinomial", alpha=1, lambda=0.5)

summary(lasso)
lasso
plot(lasso)
attributes(lasso)
lasso$beta
# lasso2<-cv.glmnet