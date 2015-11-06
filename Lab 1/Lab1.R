
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


# 3

# b)

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



