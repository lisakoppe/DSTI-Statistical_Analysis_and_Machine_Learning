#ASML - session 2___________________________________________________________________________________________

##simple linear regression

X = runif(50,-7,7)
Y = 5-6*X+rnorm(50, 0, 15)
plot(X,Y)
L = lm(Y~X)
abline(L,col='red')

help(predict.lm)

new = data.frame(X=seq(-3,3,0.1))
predict(L,new)
pred.w.plim <- predict(L, new, interval = "prediction")
pred.w.clim <- predict(L, new, interval = "confidence")
matplot(new$X, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

plot(X,Y)
matlines(new$X, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

##multiple linear regression

A = matrix(0, nrow=50, ncol=50)
A[,1] = rexp(50, 0.4)
A[,2] = rnorm(50, 3, 0.5)
A[,3] = rpois(50, 0.8)
A[,4] = runif(50, -5, 3)
A[,5:58]=matrix(rnorm(44*50), ncol=44)

Y = 3+2*A[,1]-5*A[,2]+7*A[,4]+rnorm(50)

L = lm(Y~.,data=as.data.frame(A))
L
summary(L)

X = cbind(rep(1, 50), A)
X

install.packages('Matrix')
library(Matrix)
rankMatrix(X)

L1 = lm(Y~.,data=as.data.frame(A[,1:48]))
L1
summary(L1)

sigmahat2 = sum((Y-predict(L))^2/(50-5))
sigmahat2
sqrt(sigmahat2)
plot(L)
hist(L$residuals, freq=FALSE)
