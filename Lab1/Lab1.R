
# Reads in data
data <- "C:/Users/Gustav/Documents/732A37-Multivariate-Statistical-Methods/Lab 1/data.dat"
data <- read.delim(data, header=FALSE)
names(data) <- c("Country", "100", "200", "400", "800", "1500", "3000", "Marathon")

# 1 
# a)
means <- lapply(data,  FUN=mean)[2:8]
sds <- lapply(data,  FUN=sd)[2:8]
vars <-  lapply(data,  FUN=var)[2:8]

# b)
boxplot(data[, 2:8], col="blue")
plot(data[, 2:8], col="blue")


par(mfrow=c(3, 3))
for (i in 2:8){
  dotchart(data[, i], row.names(data),col="blue", bg="black", main=colnames(data)[i])
}
par(mfrow=c(1, 1))

library(ggplot2)
require(gridExtra)

plot1 <- ggplot(data, aes(x=data[, 2], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[2] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot2 <- ggplot(data, aes(x=data[, 3], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[3] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot3 <- ggplot(data, aes(x=data[, 4], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[4] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot4 <- ggplot(data, aes(x=data[, 5], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[5] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot5 <- ggplot(data, aes(x=data[, 6], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[6] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot6 <- ggplot(data, aes(x=data[, 7], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[7] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)
plot7 <- ggplot(data, aes(x=data[, 8], y=row.names(data))) + geom_point()+
  labs(list(title =colnames(data)[8] , x = "Time")) + 
  geom_text(aes(label=data[, 1]), vjust=1.2, hjust=0.5)

grid.arrange(plot1, plot2,plot3,plot4, ncol=2)
grid.arrange(plot5, plot6,plot7, ncol=2)


# 2
# a)
covDat <- cov(data[,2:8])
corrDat <- cor(data[,2:8])

# b)
plot(data[, 2:8], col="blue")
text(data, labels=data$Country)
require(GGally)
ggpairs(data=data, # data.frame with variables
        columns=2:8) # columns to plot, default to all.

ggpairs(data=data, diag=list(continuous="density"), columns=5:7, 
        axisLabels="show")
library(corrgram)
corrgram(data, upper.panel=panel.pts, lower.panel=panel.ellipse, diag.panel=panel.density)
# c)
#2c
#install.packages("rgl")
library(rgl)
#make more
plot3d(data[,2],data[,3],data[,8],xlab="100m",ylab="400m",zlab="Marathon")
text(data[,2], data[,3],data[,8], labels=data[,1],cex=1,pos=1)
plot3d(data[,2],data[,5],data[,8],xlab="100m",ylab="800m",zlab="Marathon")
plot3d(data[,2],data[,3],data[,4],xlab="100m",ylab="200m",zlab="400")

# 3

# b)
XmeanV<-(data[,2:8])
XmeanV<-(scale(XmeanV,center = TRUE,scale = FALSE))
sqdistance<-(XmeanV)%*%t(XmeanV)
sqDiagdistance<-(diag(sqdistance))
result<-data.frame(data[,1],sqDiagdistance)
slutresult<-result[order(-result[,2]),]
head(slutresult, n=3)


# not sure but..here we go
dat_mat <- t(data[ ,2:8])
dat_mat <- t(scale(dat_mat, center = TRUE, scale = FALSE))
rowMeans(dat_mat)

testar <- ((dat_mat) %*% t(dat_mat))
testat <- diag(testar)
tesst <- data.frame(data$Country, testat)

my_new <- tesst[order(-testat),] 

# c)

# supposed to scale data?
dat_matC <- t(data[ ,2:8])
dat_matC <- t(scale(dat_matC, center = TRUE, scale = FALSE))

VarC <- diag(covDat)
VarMat <- as.vector(VarC)*diag(length(VarC))

testarC <- (dat_matC) %*% solve(VarMat) %*% t(dat_matC)
testatC <- diag(testarC)
tesstC <- data.frame(data$Country, testatC)

my_newC <- tesstC[order(-testatC),] 

# d)
dat_matD <- t(data[ ,2:8])
dat_matD <- t(scale(dat_matD, center = TRUE, scale = FALSE))


testarD <- (dat_matD) %*% solve(covDat) %*% t(dat_matD)
testatD <- diag(testarD)
tesstD <- data.frame(data$Country, testatD)

my_newD <- tesstD[order(-testatD),] 


