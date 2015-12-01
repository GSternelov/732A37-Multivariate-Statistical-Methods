
# Reads in data
data <- "C:/Users/Gustav/Documents/732A37-Multivariate-Statistical-Methods/Lab1/data.dat"
data <- read.delim(data, header=FALSE)
names(data) <- c("Country", "100", "200", "400", "800", "1500", "3000", "Marathon")

##### Assignment 1

### a)
corrMat <- cor(data[,2:8])
eigenVal <- eigen(corrMat)[1]
eigenVec <- eigen(corrMat)[2]

### b)
# principal components
dataSc <- data.frame(scale(data[,2:8], center=TRUE, scale=TRUE))
prComp <- prcomp(dataSc, center=FALSE, scale=FALSE)
# correlation between first two principal components and the standardized variable
# correlation with what? The scores? loadings? 
cor(data.frame(prComp$x[,1], prComp$x[,2]), dataSc)
# variation explained by PC1 and PC2
lambda=prComp$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)[1:2]

### c)
# data frame with scores and country
pcScores <- data.frame(PC1=prComp$x[,1], PC2=prComp$x[,2], country=data[,1])
library(ggplot2)
ggplot(pcScores, aes(x=PC1, y=PC2)) + geom_point() + geom_text(aes(label=country))

### d)
# rank the nations based on their score for PC1
TopNat <- pcScores[order(-pcScores$PC1),]
head(TopNat[,c(1,3)])

##### Assignment 2
corFA <- cor(data[, 2:8])
covFA <- cov(data[, 2:8])

# With covariance matric
FAcov <- factanal(data=(data[,2:8]),covmat =covFA, factors = 3, scores = "regression")

# With correlation matrix
FAcor <- factanal(x=data[,2:8], factors = 3, scores = "regression")
FAcor$scores


