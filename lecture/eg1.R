#install.packages("psych")
#install.packages("ggplot2")
library(psych)
library(ggplot2)
library(reshape2)

setwd('D:\\chap2\\r')
#read data
exp_eg<-read.table("exp_eg.csv", sep = ",",header = TRUE)
head(exp_eg)


#summary
describe(exp_eg[,1:9],skew=F,ranges=F)

#standardization
exp<-scale(exp_eg[,1:9])
head(exp)
#round(exp[1:2,1:9],3)

#Squared Euclidean distance
dist<-dist(exp,method="euclidean")^2
dist
#round(dist,2)

#Single linkage
#method includes: "ward.D", "sin", "com", "ave", "cen"
fit <- hclust(dist, method="single")
#clustering histroy
history<-cbind(fit$merge,fit$height)
history

#distance plot
ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

#dendrogram
par(mar=c(1,4,1,1))
plot(fit,labels=exp_eg$case,hang=-1,main="")
axis(side = 2, at = seq(0, 16, 2))

#4-cluster solution
cutree(fit, k=4) #cluster index
sol <- data.frame(cluster=cutree(fit, k=4),id=exp_eg$case)
sol

#Ward's method
#dist<-dist(exp,method="euclidean")^2
fit <- hclust(dist, method="ward.D")
history<-cbind(fit$merge,fit$height)
history

ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

plot(fit,labels=exp_eg$case,hang=-1,sub="",xlab="",main="")
axis(side = 2, at = seq(0, 100, 20))

#clustering
cluster<-cutree(fit,k=4)
case<-exp_eg$case
sol <- data.frame(cluster,exp,case)
sol[ order(sol$cluster),c(1,11) ]

#cluster means
tb<-aggregate(x=sol[,2:10], by=list(cluster=sol$cluster),FUN=mean)
print(tb,digits=2)

#profile plot
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")
