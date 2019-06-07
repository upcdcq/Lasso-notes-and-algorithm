remove(list = ls())
#Set data
#Example 1 
set.seed(12345)
N = 500
X1 = rnorm(N,-4,1.5^2)
X2 = rexp(N,4)
X3 = rgamma(N,5)
X4 = rt(N,2)
X = cbind(X1,X2,X3,X4)
b = c(1,2,3,0.001)
eps = rnorm(N,0,0.5^2)
Y = X%*%b + eps
lm(Y~X)
# fit model
lambda = 0.1
iter = 1000
eps1 = 0.01
ls_cd(X,Y,lambda,iter,eps1)
#Compute the test MSE
M = 100
X1_new = rnorm(M,-4,1.5^2)
X2_new = rexp(M,4)
X3_new = rgamma(M,5)
X4_new = rt(M,2)
X_new = cbind(X1_new,X2_new,X3_new,X4_new)
#X_new = scale(X_new)
eps_new = rnorm(M,0,0.5^2)
Y_new = X_new%*%b + eps_new
M= 30
Lambda = seq(0,0.2,length.out = M)
Loss = NULL
for (i in 1:M) {
  Be = ls_cd(X,Y,Lambda[i],iter,eps1)
  Loss[i] = loss(X_new,Y_new,Be)
}

#plot_lasso(Lambda,Beta)
plot(Lambda,Loss,type = 'l',col = 'orange',main = 'Test MSE')
sss = which.min(Loss)
abline(v =Lambda[sss],lty = 2)
#_profile
S= 20
i=seq(-5,3,length.out = S)
Lambda = exp(i)
Beta = matrix(0,nrow = S,ncol = 4)
for (i in 1:S) {
  Beta[i,] = ls_cd(X,Y,Lambda[i])[-1]
}
plot_lasso(log(Lambda),Beta)
fit1 = glmnet(X, Y)
plot(fit1)
#______________________
#Using cross validation
M= 30
Lambda = seq(0,0.2,length.out = M)
cv_result = cv_cd(X,Y,Lambda,10)
par(mfrow = c(1,2))
plot(Lambda,cv_result[,1],type = 'l',ylim = c(-0.1,0.7),
     main = c('CV'))
lines(Lambda,cv_result[,3],col = 'red')
lines(Lambda,cv_result[,2],col = 'blue')
loca = which.min(cv_result[,1])
abline(v =Lambda[loca],lty = 2)

#______plot the profile___
#Beta = matrix(0,nrow = M,ncol = 10)


## EXAMPLE by lars data
library(lars)
library(glmnet)
data(diabetes)
myX = diabetes$x
myY = diabetes$y
fit1 = cv.glmnet(myX, myY)
plot(fit1)
#myX = scale(myX)
S= 35

i=seq(-3,3.5,length.out = S)
Lambda = exp(i)
mean_x = apply(X,2,mean)
cv_result = cv_cd(scale(myX),myY,Lambda)
plot(log(Lambda),cv_result[,1],type = 'l',col = 'orange',
     ylim = c(2000,6000),main = 'My CV',lwd = 2)
lines(log(Lambda),cv_result[,2], lty = 2,lwd = 2,col = 'blue')
lines(log(Lambda),cv_result[,3],lty = 2,lwd = 2,col = 'red')
lab_min =  which.min(cv_result[,1])
abline(v = log(Lambda[lab_min]) ,lty = 2,lwd = 2)


S= 50
i=seq(-6,1,length.out = S)
Lambda = exp(i)
Beta = matrix(0,nrow = S,ncol = p)
for (i in 1:S) {
  Beta[i,] = ls_cd(myX,myY,Lambda[i])[-1]
}
plot_lasso(log(Lambda),Beta)
fit_glmnet = glmnet(myX, myY)
plot(fit_glmnet,lwd = 3)
### High dimensinal data test
library(glmnet)
set.seed(19875)  # Set seed for reproducibility
n <- 200  # Number of observations
p <- 10  # Number of predictors included in model
real_p <- 5  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]
##use glmnet
cvfit = cv.glmnet(x, y)
plot(cvfit)
# use my function
S= 30
i=seq(-4,0.5,length.out = S)
Lambda = exp(i)
cv_result = cv_cd(x,y,Lambda)
plot(log(Lambda),cv_result[,1],type = 'l',main = 'My CV',
     col = 'orange',ylim = c(-0.1,7),lwd = 2)
lines(log(Lambda),cv_result[,2], lty = 2,col = 'blue',lwd = 2)
lines(log(Lambda),cv_result[,3],lty = 2,col = 'red',lwd = 2)
lab_min =  which.min(cv_result[,1])
abline(v = log(Lambda[lab_min]) ,lty = 2,lwd = 2)
# plot the profile for high dimensinal data
S= 40
i=seq(-6,0.5,length.out = S)
Lambda = exp(i)
Beta = matrix(0,nrow = S,ncol = p)
for (i in 1:S) {
  Beta[i,] = ls_cd(x,y,Lambda[i])[-1]
}
plot_lasso(log(Lambda),Beta)

fit_2= glmnet(x, y)
plot(fit_2)
##Hight
## p = 2n,real_p = 5
set.seed(1287787)  # Set seed for reproducibility
n <- 100  # Number of observations
p <- 200  # Number of predictors included in model
real_p <- 5  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]
##use glmnet
cvfit = cv.glmnet(x, y)
plot(cvfit)
# use my function
S= 30
i=seq(-4,0,length.out = S)
Lambda = exp(i)
cv_result = cv_cd(x,y,Lambda)
plot(log(Lambda),cv_result[,1],type = 'l',
     col = 'orange',ylim = c(-1,6),lwd = 2,main = 'My CV')
lines(log(Lambda),cv_result[,2], lty = 2,lwd = 2,col = 'blue')
lines(log(Lambda),cv_result[,3],lty = 2,lwd = 2,col = 'red')
lab_min =  which.min(cv_result[,1])
abline(v = log(Lambda[lab_min]) ,lty = 2,lwd = 2)
# plot the profile for high dimensinal data
S= 40
i=seq(-4,0.1,length.out = S)
Lambda = exp(i)
Beta = matrix(0,nrow = S,ncol = p)
for (i in 1:S) {
  Beta[i,] = ls_cd(x,y,Lambda[i])[-1]
}
plot_lasso(log(Lambda),Beta)
fit_glmnet = glmnet(x, y)

plot(fit_glmnet)
fit_lars = lars(x.train,y.train, type = "lasso")
plot(fit_lars)

##Real data
library(MASS)
library (ISLR)
Hitters =na.omit(Hitters) 
x=Hitters[,-c(14,15,19,20)]
x = as.matrix(x)
y=Hitters$Salary
p = dim(x)[2]
S = 40
i=seq(-20,5,length.out = S)
Lambda = exp(i)
Beta = matrix(0,nrow = S,ncol = p)
Beta[1,] = profile_cd(x,y,Lambda[1],rnorm(p))[-1]
for (i in 2:S) {
  Beta[i,] = profile_cd(x,y,Lambda[i],Lambda[i-1])[-1]
}
fit_glmnet = glmnet(as.matrix(x), y,lambda = Lambda)
plot(fit_glmnet)
plot_lasso(Lambda,Beta)

cv_11 = cv.glmnet(as.matrix(x), y,lambda = Lambda)
plot(cv_11)
S= 40
i=seq(-3,6,length.out = 25)
Lambda = exp(i)
cv_result = cv_cd(x,y,Lambda,K = 16)
plot(log(Lambda),cv_result[,1],type = 'l',
     col = 'orange',ylim = c(1e4,3e5))
lines(log(Lambda),cv_result[,2], lty = 2,col = 'blue')
lines(log(Lambda),cv_result[,3],lty = 2,col = 'red')
lab_min =  which.min(cv_result[,1])
abline(v = log(Lambda[lab_min]) ,lty = 2)
# K = 6
# N = 100
# for (i in 1:K) {
#   begin = ceiling((i-1)*N/K+1)
#   over = min(ceiling(i*N/K) ,N)
#   print(c(begin,over))
# }