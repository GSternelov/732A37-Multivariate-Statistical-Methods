
data <- "C:/Users/Gustav/Documents/732A37-Multivariate-Statistical-Methods/Lab2/data.dat"
data <- read.delim(data, header=FALSE)
names(data) <- c("Country", "100", "200", "400", "800", "1500", "3000", "Marathon")

# 1
# a)

XmeanV<-(data[,2:8])
XmeanV<-(scale(XmeanV,center = TRUE,scale = FALSE))
covDat <- cov(data[,2:8])

MahanabisD<-(XmeanV)%*%solve(covDat)%*%t(XmeanV)

test <- ((54-7) / (7*(54-1)) )*  diag(MahanabisD)
valus <- data.frame(data[,1],test)
head(valus[order(-valus[,2]),])

# 2
data2 <- "C:/Users/Gustav/Documents/732A37-Multivariate-Statistical-Methods/Lab2/T5-12.dat"
data2 <- read.delim(data2, header=FALSE, sep="")

# a
library(ellipse)
my_centre <- unlist(lapply(data2, mean)) - c(190, 275)
diffs <- data2 -  unlist(lapply(data2, mean)) 

mean_mat <- matrix(ncol=2, nrow=45)
for (i in 1:2){
  mean_mat[, i] <- data2[, i] - my_mean[i]
}

radius <- mean_mat %*% cov(data2) %*% t(mean_mat)

plot(ellipse(x=cov(data2), centre=c(190, 275)), type="l")
points(ellipse(x=cov(data2), centre=my_mean),type="l", col="red")
points(data2)
# Rather plausible values since the ellipse covers zero for both the tail and wing length.

library(car)
my_mean <-  unlist(lapply(data2, mean))
plot(data2)
car::ellipse(center=my_mean, shape=cov(data2), radius=0.365, col="blue")
car::ellipse(center=c(190, 275), shape=cov(data2), radius=0.365, col="brown")

# Eigenval
eigenVal <- eigen(cov(data2))$values
halfLeng1 <- sqrt(eigenVal[1]) * sqrt(((2*44)/(45*43)) * 3.21)
halfLeng2 <- sqrt(eigenVal[2]) * sqrt(((2*44)/(45*43)) * 3.21)

plot(data2)
points(ellipse::ellipse(x=cor(data2), centre=my_mean), type="l")
points(ellipse::ellipse(x=cor(data2), centre=c(190, 275)), type="l", col="blue")

# b
my_mean <-  unlist(lapply(data2, mean))
# Hotellings conf ints
a <- matrix(c(1,0))
c1 <- t(a) %*% cov(data2) %*% a
a2 <- matrix(c(0,1))
c2 <- t(a2) %*% cov(data2) %*% a2

conf1_Low <- my_mean[1] - sqrt(((2*44) / (45-2) * 1.65 / 45 ) * c1)
conf1_High <- my_mean[1] + sqrt(((2*44) / (45-2) * 1.65 / 45 ) * c1)
conf2_Low <- my_mean[2] - sqrt(((2*44) / (45-2) * 1.65 / 45 ) * c2)
conf2_High <- my_mean[2] + sqrt(((2*44) / (45-2) * 1.65 / 45 ) * c2)

# Bonferroni
Bonf1_Low <- my_mean[1] - 2.32 * sqrt(cov(data2)[1,1] / 45)
Bonf1_High <- my_mean[1] + 2.32 * sqrt(cov(data2)[1,1] / 45)

Bonf2_Low <- my_mean[2] - 2.32 * sqrt(cov(data2)[2,2] / 45)
Bonf2_High <- my_mean[2] +  2.32 * sqrt(cov(data2)[2,2] / 45)

# c
qqnorm(data2[,1]); qqline(data2[,1], col = 2)
qqnorm(data2[,2]); qqline(data2[,2], col = 2)
library(ggplot2)
ggplot(data2, aes(x=V1, y=V2)) + geom_point()

# 3
data3 <- "C:/Users/Gustav/Documents/732A37-Multivariate-Statistical-Methods/Lab2/T6-13.dat"
data3 <- read.delim(data3, header=FALSE, sep="")
# 
skull_manova <- manova(cbind(V1, V2, V3, V4) ~ as.factor(V5), data=data3)
summary(skull_manova)
summary.aov(skull_manova)

year1 <- subset(data3, data3$V5 == 1)
year2 <- subset(data3, data3$V5 == 2)
year3 <- subset(data3, data3$V5 == 3)

plot(data3[,1:4], col=data3$V5)

cov(year1)
cov(year2)
cov(year3)

# Medelvärden
meanY1 <- 0
meanY2 <- 0
meanY3 <- 0
for (i in 1:4){
  meanY1[i] <- mean(year1[,i])
  meanY2[i] <- mean(year2[,i])
  meanY3[i] <- mean(year3[,i])
}
# Differenser
DiffV1 <- c(meanY1[1] - meanY2[1], meanY1[1] - meanY3[1], meanY2[1] - meanY3[1])
DiffV2 <- c(meanY1[2] - meanY2[2], meanY1[2] - meanY3[2], meanY2[2] - meanY3[2])
DiffV3 <- c(meanY1[3] - meanY2[3], meanY1[3] - meanY3[3], meanY2[3] - meanY3[3])
DiffV4 <- c(meanY1[4] - meanY2[4], meanY1[4] - meanY3[4], meanY2[4] - meanY3[4])

# Undersöker för vilka diffar intervallen ej täcker 0
for (i in 1:3){
  if(abs(DiffV1[i]) > 3.44){
    print("Difference")
  }else{
    print("No difference")}
}
for (i in 1:3){
  if(abs(DiffV2[i]) > 3.57){
    print("Difference")
  }else{
    print("No difference")}
}
for (i in 1:3){
  if(abs(DiffV3[i]) > 4.03){
    print("Difference")
  }else{
    print("No difference")}
}
for (i in 1:3){
  if(abs(DiffV4[i]) > 2.36){
    print("Difference")
  }else{
    print("No difference")}
}