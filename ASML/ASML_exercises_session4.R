#ASML - session 4___________________________________________________________________________________________

install.packages('glmnet')
library(glmnet)

help(glmnet)

x = matrix(rnorm(100*20),100,20)
y = rnorm(100)
fit1 = glmnet(x,y,alpha=0)
plot(fit1)

x = matrix(rnorm(100*10),ncol=10)
fit1 = glmnet(x,y,alpha=0)
plot(fit1)
fit2=glmnet(x,y,alpha=1)
plot(fit2)
print(fit2)

help(cv.glmnet)
fit2cv = glmnet(x,y)
plot(fit2cv)
names(fit2cv)

fit2cv$lambda.min
fit2cv$lambda.1se
coefficients(fit2)
X = matrix(data=0,ncol=5,nrow=100)
X[,1]=runif(100,-4,3)
X[,2]=rexp(100,4)
X[,3]=rexp(100,0.02)
X[,4]=rpois(100,10)
X[,5]=rf(100,2,5)
Y=-2+3*X[,2]-5*X[,1]+X[,4]+rnorm(100)
fitcv=cv.glmnet(X,Y)
plot(fitcv)

fit = glmnet(X,Y,alpha=1, lambda=fitcv$lambda.1se)
coefficients(fit)
cor(X)

reg = lm(Y~.,data=as.data.frame(X[,1:4]))
reg

#Exercise

X = matrix(data=c(rep(1,100),
           c(rep(1,20),rep(0,80)),
           c(rep(0,20),rep(1,25),rep(0,55)),
           c(rep(0,45),rep(1,20),rep(0,35)),
           c(rep(0,65),rep(1,10),rep(0,25)),
           c(rep(0,75),rep(1,25))),ncol=6)

B = matrix(data=c(4,0.5,0.4,0.6,0.2,0.1),ncol=1)
Y = X%*%B+matrix(data=rnorm(100),ncol=1)
Xf = factor(c(rep('A',20),
            rep('B',25),
            rep('C',20),
            rep('D',10),
            rep('E',25)))
boxplot(Y)
boxplot(Y~Xf)

mod = lm(Y~Xf)
mod

(Xf=='A')
n1 = sum(Xf=='A')
muhat = 1/n1*sum(Y*(Xf=='A'))
mod
n2 = sum(Xf=='B')
1/n2*sum(Y*(Xf=='B'))-muhat
summary(mod)
anova(mod)

#Exercise

X = matrix(data=c(rep(1,10),
                  c(rep(1,3),rep(0,7)),
                  c(rep(0,3),rep(1,3),rep(0,4)),
                  c(rep(0,6),rep(1,4)),
                  c(1,1,0,1,0,0,1,1,0,0),
                  c(0,0,1,0,1,1,0,0,1,1),
                  c(1,1,0,0,0,0,0,0,0,0),
                  c(0,0,1,0,0,0,0,0,0,0),
                  c(0,0,0,1,0,0,0,0,0,0),
                  c(0,0,0,0,1,1,0,0,0,0),
                  c(0,0,0,0,0,0,1,1,0,0),
                  c(0,0,0,0,0,0,0,0,1,1)),
                  ncol=12)

B = matrix(data=c(1,2,3,4,2,2,1,2,1,3,1,2),ncol=1)
Y = X%*%B+rnorm(10)
F1=factor(c(rep(1,3),rep(2,3),rep(3,4)))
F1
F2=factor(c(1,1,2,1,2,2,1,1,2,2))
F2
mod2=lm(Y~F1+F2)
mod2

mod1=lm(Y~F1+F2+F1*F2)
mod1

anova(mod1)
anova(mod2)

FF1 = as.numeric(F1)
FF1
FF2 = as.numeric(F2)
FF2
modb2=lm(Y~FF1+FF2)
modb2
