#ASML - session 1___________________________________________________________________________________________

##simple linear regression

X = runif(100, -3, 3)
Y = 3 - 2*X + rnorm(100)
plot(X, Y)
L = lm(Y~X)

summary(L)
names(L)
L$coefficients
L$residuals
L$fitted.values
L$rank

sigmahatn2 = sum(L$residuals^2)/98
sqrt(sigmahatn2)
summary(L)

R = L$residuals
hist(R, freq=FALSE)
plot(L)

rst = rstudent(L)
rst
resm = Y-L$fitted.values
resm
L$residuals-resm

rstam = rstandard(L)
rstam

plot(rst)
abline(h=2, col='red')
abline(h=-2, col='red')

y = 3-2*X+rexp(100,2)
L=lm(Y~X)
summary(L)
hist(L$residuals)

y = 3-2*X+rexp(100,0.2)
L=lm(Y~X)
summary(L)
hist(L$residuals, freq=FALSE)
plot(X,y)
