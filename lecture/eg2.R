#install.packages("psych")
#install.packages("ggplot2")
library(psych)
library(ggplot2)

faces<-read.table("faces.csv", sep = ",",header = TRUE)
head(faces)

#Squared Euclidean distance = number of misatched
#number of  mismatched
dist<-dist(faces[,2:6],method="euclidean")^2
dist

#complete linkage
fit <- hclust(dist, method="com")
history<-cbind(fit$merge,fit$height)
history

#plot of distances
ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

#dengrogram
par(mar=c(2,5,1,1))
plot(fit,labels=faces$id,hang=-1,sub="",xlab="",main="")
axis(side = 2, at = seq(0, 0.8, 0.2))

#3-cluster solution
#cutreee() cut a tree into clusters
#tree= tree produced by hclust()
#k= number of clusters
sol <- data.frame(cluster=cutree(tree=fit, k=3),faces)
sol[order(sol[,1]),1:2]
table(sol[,1])

#crosstab
#i=3,4,5,6,7
i<-3
colnames(sol)[i]
t<-table(sol$cluster,sol[,i])
prop.table(t,1)

#i='sex', 'glasses', 'moustache', 'smile', 'hat'
i<-'hat'
t<-table(sol$cluster,sol[,i])
prop.table(t,1)
######################
#for (i in 3:7) {
#  print(colnames(sol)[i])
#  t<-table(sol$cluster,sol[,i])
#  print(prop.table(t,1))
#}
########################
