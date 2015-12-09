

covMat <- matrix(c(1106, 396.7, 108.4, 0.787, 26.23, 396.7, 2382, 1143, -0.214, -23.96,
         108.4, 1143, 2136, 2.189, -20.84, 0.787, -0.214, 2.189, 0.016, 0.216,
         26.23, -23.96, -20.84, 0.216, 70.56), nrow=5, ncol=5)
corMat <- cov2cor(covMat)

s11 <- covMat[1:3, 1:3]
s12 <- covMat[1:3, 4:5]
s21 <- covMat[4:5, 1:3]
s22 <- covMat[4:5, 4:5]
r11 <- corMat[1:3, 1:3]
r12 <- corMat[1:3, 4:5]
r21 <- corMat[4:5, 1:3]
r22 <- corMat[4:5, 4:5]

expM <- function(X,e) { v <- La.svd(X); v$u %*% diag(v$d^e) %*% v$vt } 

EigVal1 <- eigen(expM(s11, -0.5) %*% s12 %*% solve(s22) %*% s21 %*% expM(s11, -0.5))[1]
Pstar1 <- sqrt(EigVal1[[1]])[1]
EigVal2 <- eigen(expM(s22, -0.5) %*% s21 %*% solve(s11)%*% s12 %*% expM(s22, -0.5))
Pstar2 <- sqrt(EigVal2[[1]])[2]

EigVec1 <- eigen(expM(s11, -0.5) %*% s12 %*% solve(s22) %*% s21 %*% expM(s11, -0.5))[2]
EigVec2 <- eigen(expM(s22, -0.5) %*% s21 %*% solve(s11)%*% s12 %*% expM(s22, -0.5))[2]

# Standardized canonical coefficients
EigVec1 <- eigen(expM(r11, -0.5) %*% r12 %*% solve(r22) %*% r21 %*% expM(r11, -0.5))[2]
EigVec2 <- eigen(expM(r22, -0.5) %*% r21 %*% solve(r11) %*% r12 %*% expM(r22, -0.5))[2]

U1 <- t(as.matrix(EigVec1[[1]][,1])) %*% expM(r11, -0.5) 
V1 <- t(as.matrix(EigVec2[[1]][,1])) %*% expM(r22, -0.5) 

corrU1 <- as.vector(U1 %*% r11)
names(corrU1) <- c("Glucose", "Insulin", "Insulres")
corrV1 <- as.vector(V1 %*% r22)
names(corrV1) <-c("Weight", "Fasting")
kable(corrV1, format = "latex")
U1Tab <- xtable(U1 %*% r11)
print(U1Tab)

`r kable(corrU1, format = "latex", caption="Correlation between U1 and the primary set", align="c")`
`r kable(corrV1, format = "latex", caption="Correlation between V1 and the secondary set")`

### a) 
# Testa om det finns något samband mellan första och andra gruppen av variabler
# För den första kanoniska korrelationen
-(46-1-0.5*(3+2+1)) * log((1-Pstar1^2) * (1-Pstar2^2))
-46 * (log((1-Pstar1^2) * (1-Pstar2^2)))
chi_sq_val <- qchisq(1-0.05, df=6) 

# För den andra kanoniska korrelationen
-(46-1-0.5*(3+2+1)) * log(1-Pstar2^2)
chi_sq_val2 <- qchisq(1-0.05, df=2) 

# Bara första paret är signifikant

# Beräkna U1 och V1


