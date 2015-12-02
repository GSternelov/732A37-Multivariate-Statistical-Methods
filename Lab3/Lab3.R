
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
prComp$rotation[,1:2]
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
FAcov <- factanal(data=(data[,2:8]),covmat =covFA, factors = 3, scores = "Bartlett")

# With correlation matrix
FAcor <- factanal(x=data[,2:8], factors = 2, scores = "Bartlett")
FAcor$
plot(FAcor$scores)



## Principal component solution with S
covEigenV <- as.matrix(data.frame(eigen(cov(data[, 2:8]))[1]))
covEigenVec <- as.matrix(data.frame(eigen(cov(data[, 2:8]))[2]))

L <- matrix(ncol=7, nrow=7) 
for(i in 1:7){
  L[,i] <- sqrt(covEigenV[i,]) * covEigenVec[,i]
}
TestPsi <- diag(x=diag(covFA - L%*%t(L)))
#Sii <- diag(covFA)
#Psi <- Sii -  colSums(L%*%t(L))
#PsiHat <- diag(x=Psi)

diag(L%*%t(L) + TestPsi)
diag(covFA)
residualMat <- covFA - (L%*%t(L) + TestPsi)
# Proportion of total sample variance explained
covEigenV[1,] / sum(diag(covFA))
covEigenV[2,] / sum(diag(covFA))

factorLoadCov <- L[,1]
# Factor scores
dataCtrd <- as.matrix(scale(data[,2:8], center=TRUE, scale=FALSE))
ScoresCov <- t(solve(t(L)%*%L) %*% t(L) %*% t(dataCtrd))
plot(ScoresCov[,1], ScoresCov[,2])

## Principal component solution with R
corEigenV <- as.matrix(data.frame(eigen(cor(data[, 2:8]))[1]))
corEigenVec <- as.matrix(data.frame(eigen(cor(data[, 2:8]))[2]))

Lcor <- matrix(ncol=7, nrow=7) 
for(i in 1:7){
  Lcor[,i] <- sqrt(corEigenV[i,]) * corEigenVec[,i]
}
TestPsi <- diag(x=diag(corFA - Lcor%*%t(Lcor)))

residualMat <- corFA - (Lcor%*%t(Lcor) + TestPsi)
# Proportion of total sample variance explained
corEigenV[1,] / 7
corEigenV[2,] / 7

factorLoadCor <- Lcor[,1:2]
# Factor scores
dataCtrd <- as.matrix(scale(data[,2:8], center=TRUE, scale=FALSE))
ScoresCor <- t(solve(t(Lcor)%*%Lcor) %*% t(Lcor) %*% t(dataCtrd))

plot(ScoresCor[,1], ScoresCor[,2])

# The ML method, common factors F and specific factors epsilon
# assumed to be normally dsitributed
