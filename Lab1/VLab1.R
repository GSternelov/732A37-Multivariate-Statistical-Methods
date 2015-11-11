mydat<-"C:/Users/vuong/Desktop/Multivariate/T1-9.dat"
data<-read.delim(mydat,header=FALSE)
colnames(data)<-c("Country","100m(s)","200m(s)","400m(s)","800m(min)","1500m(min)","3000m(min)","Marathon(min)")

#Assignment 1

#1a
lapply(data[,2:8],mean)
lapply(data[,2:8],sd)
lapply(data[,2:8],max)
lapply(data[,2:8],min)
lapply(data[,2:8],median)
#1b
par(mfrow=c(2, 4))
for(i in 2:8){
  #hist(data[,i],main=colnames(data[i]),xlab="observation")
  qqnorm(data[,i],main=colnames(data[i]))
  qqline(data[,i])
}
bxpdat <- boxplot(data[,2:8],col=rainbow(length(data[,2:8])),main="Boxplot")
text(bxpdat$group+0.2,                                           # the x locations 
     bxpdat$out,                                                 # the y values
     rownames(data)[which( data == bxpdat$out, arr.ind=TRUE)[, 1]])

# Simple Dotplot
for(i in 2:8){
  hej<-dotchart(data[,i],labels=row.names(data),cex=.7,main=colnames(data[i]))
}
#Assigment 2

#2a
par(mfrow=c(1, 1))
covM<-cov(data[,2:8])
corrM<-cor(data[,2:8])

#2b
plot(data[2:8])
par(mfrow=c(1, 1))
for(j in 2:7){
  for(i in j:8){
    plot(data[,j], data[,i],main= "Scatter plot",xlab=colnames(data[j]),ylab= colnames(data[i]),col= "blue")
    
    text(data[,j], data[,i], labels=data[,1],cex=1,pos=1)
  }
}
par(mfrow=c(1, 1))
#2c
#install.packages("rgl")
library(rgl)
#make more
plot3d(data[,2],data[,3],data[,8],xlab="100m",ylab="200m",zlab="Marathon")
text(data[,2], data[,3],data[,8], labels=data[,1],cex=1,pos=1)
plot3d(data[,2],data[,5],data[,8],xlab="100m",ylab="800m",zlab="Marathon")
plot3d(data[,2],data[,3],data[,4],xlab="100m",ylab="200m",zlab="400")
#3

#a

#b



XmeanV<-(data[,2:8])
XmeanV<-(scale(XmeanV,center = TRUE,scale = FALSE))


sqdistance<-(XmeanV)%*%t(XmeanV)
sqDiagdistance<-(diag(sqdistance))
result<-data.frame(data[,1],sqDiagdistance)
slutresult<-result[order(-result[,2]),]
slutresult
#c
Xtest<-scale(data[,2:8])
distancenew<-(Xtest)%*%t(Xtest)
distancenewSqDistance<-diag(distancenew)
resulttest<-data.frame(data[,1],distancenewSqDistance)
slutresulttest<-resulttest[order(-resulttest[,2]),]
slutresulttest

VarV<-diag(cov)
VarM<-as.vector(VarV)*diag(length(VarV))
newSqDistance<-(XmeanV)%*%solve(VarM)%*%t(XmeanV)
newSqDistance<-diag(newSqDistance)
#newSqDistance<-as.data.frame(t(newSqDistance))
result2<-data.frame(data[,1],newSqDistance)
slutresult2<-result2[order(-result2[,2]),]
slutresult2
#d
mahatest<-mahalanobis(data[,2:8],center = colMeans(data[,2:8]), cov=covM, inverted = FALSE)
resultmaha<-data.frame(data[,1],mahatest)
slutresultmaha<-resultmaha[order(-resultmaha[,2]),]
slutresultmaha


MahanabisD<-(XmeanV)%*%solve(covM)%*%t(XmeanV)
MahanabisD<-diag(MahanabisD)
result3<-data.frame(data[,1],MahanabisD)
slutresult3<-result3[order(-result3[,2]),]
slutresult3


