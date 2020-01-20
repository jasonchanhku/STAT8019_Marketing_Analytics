#install.packages("psych")
library(psych)
library(reshape2)
library(ggplot2)

public<-read.csv("public.csv", header = TRUE)
public[,3:10]<-scale(public[,3:10])

#k-means method, 5-cluster solution
set.seed(12345)
#kmeans() K-means clustering
#x= data set
#centers= number of clusters/centers of clusters
#algorithm= K-means algorithm
# MacQueen's method the traditional method
fit1<-kmeans(x=public[3:10],centers=5,algorithm="MacQueen")
fit1

#BSS/ESS
fit1$betweenss/fit1$tot.withinss

#cluster solution
fit1$cluster

#rerun K-means with seeds from the Ward's method
dist<-dist(public[,3:10],method="euclidean")^2
fit2 <- hclust(dist, method="ward.D")
ward.sol<-cutree(fit2,k=5)
tb<-aggregate(public[,3:10],by=list(ward=ward.sol),FUN=mean)
#use the centers as seeds
fit1<-kmeans(x=public[3:10],centers=tb[,2:9],algorithm="MacQueen")
fit1

#cluster centers
tb<-fit1$centers

tb<-data.frame(cbind(tb,cluster=1:5))
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")

#combine data 
sol<-cbind(kmeans=fit1$cluster,ward=ward.sol,public)
sol[order(sol[1]),c("name","id","kmeans","ward")]

