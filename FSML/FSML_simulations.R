#FSML2 - session 1___________________________________________________________________________________________

A = runif(10000)
H = hist(A, breaks=c(0,0.2,0.4,0.6,0.8,1), main="Title", freq=FALSE)
H$counts
H$breaks


fluc <- function(n,lambda,K)
{
  L = c()
  for (i in 1:K)
  {
    A = rexp(n,lambda)
    L = c(L,1/mean(A))
  }
  fluc = L
}
Est = fluc(100,4,50)
boxplot(Est)

Est = fluc(10000,4,50)
X11()
boxplot(Est)

Est = fluc(1000,4,50)
boxplot(Est)


#FSML2 - session 2___________________________________________________________________________________________

#read data1
A = read.table('data1.txt')
table(A)
sum(table(A))

#read data2
B = read.table('data2.txt')
table(B)
help(var)
v = var(B) * 999/1000
v
m = mean(as.matrix(B))
m
1-v/m
phat = 1-v/m
nhat = m/phat
nhat
nhat = 11
phat = m/nhat
phat


#Exercise 1
x = seq(-1, 5, 0.01)
fx = 0 * (x<0) + x^2/8 * (x>=0) * (x<2) + (-x^2/8+x-1) * (x>=2) * (x<4) + 1 * (x>=4)
plot(x, fx, type='l', col='red')
u = runif(5000) #5000 simulations of a uniform on [O,1]
Y = sqrt(8*u)*(u<=1/2)+(4-sqrt(8-8*u))*(u>1/2)
min(Y)
max(Y)
hist(Y, freq=FALSE, breaks=30)


#Exercise 2
M = matrix(data = runif(5000*100, 0, 3.5), nrow = 5000)
A = matrix(1:10, ncol=2)
sum(A)
apply(A, 1, sum)
Xbar = apply(M, 1, mean)
hist(Xbar, freq=FALSE, breaks = 20)


#FSML2 - session 3___________________________________________________________________________________________

#Example of Student's distribution
t = seq(-10, 10, 0.05)
y1 = dt(t, 1)
y5 = dt(t, 5)
y10 = dt(t, 10)
y50 = dt(t, 50)
ym = max(y1, y5, y10, y50)
plot(t, y1, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='red')
par(new=TRUE)
plot(t, y5, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='blue')
par(new=TRUE)
plot(t, y10, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='green')
par(new=TRUE)
plot(t, y50, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='yellow')
yn = dnorm(t, 0, 1)
par(new=TRUE)
plot(t, yn, xlim=c(-10, 10), ylim=c(0, ym), type='l', col='black')


#FSML2 - session 4___________________________________________________________________________________________

#Exercise on Student's distribution tables
qt(0.2,12) #quantile function is the inverse
qt(0.9,12)
qt(0.505,12)
#evaluation of the distribution function at a point
pt(0.259,12)
1-pt(1.083,12)
1-pt(-2.179,12)


#Exercise confidence interval
mu = 2
sig2 = 1.7
alpha = 0.05

#question 1
n = 500
A = rnorm(n, mu, sqrt(sig2)) #we create the 500 observations from a Gaussian with expectation 2 and variance 1.7

lowerbound = mean(A)-sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)
upperbound = mean(A)+sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)
c(lowerbound,upperbound)

#question 2
visua <- function(k, n, mu, sig2) #to compute several confidence interval and say when we are outside
{
M = matrix(data=0, ncol=2, nrow=k)
for (i in 1:k)
  {
    A = rnorm(n, mu, sqrt(sig2)) #dataset
    lowerbound = mean(A) - sqrt(var(A))/sqrt(n) * qt(1-alpha/2,n-1) #lowerbound with unknown variance in gaussian setting
    upperbound = mean(A) + sqrt(var(A))/sqrt(n) * qt(1-alpha/2,n-1) #upperbound with unknown variance in gaussian setting
    M[i,1] = lowerbound
    M[i,2] = upperbound
  }
N = sum((M[,2]<2) + (M[,1]>2))
visua = list(interval=M, count=N)
}
Z = visua(50, 500, 2, 1.7)
Z$interval
Z$count


#correction exercise from session1
M = matrix(data=rpois(200*1000,5), ncol=200) #matrix with 200 columns and 1000 rows which elements are observations from a Poisson with parameter 5
N = apply(M, 1, mean) #we compute row by row the empirical mean. N is composed of 1000 observations of lambdahat_{n,2}
hist(N, freq=FALSE) #it looks like a gaussian. Obvious with central limit theorem

#what are the parameters of the gaussian?
#idea: 5, 5/n.      #n is the number of observations used to compute an empirical mean
#here n=200

H = hist(N, plot=FALSE)
limits = H$breaks #values that determine the classes
xmin = min(limits) #smallest limit of classes
xmax = max(limits) #biggest limit of classes
x = seq(xmin, xmax, 0.01)
y = dnorm(x, 5, sqrt(5/200))
ymax = max(y, H$density)
hist(N, freq=FALSE, xlim=c(xmin, xmax), ylim=c(0, ymax))
par(new=TRUE) #superposition
plot(x, y, type='l', col='red', xlim=c(xmin, xmax), ylim=c(0, ymax))


#simulation exercises

#simulation 1
dataset1 = scan("simu1.txt")
hist(dataset1, freq=FALSE)
#we guess an exponential distribution
#estimation of the parameter of the exponential
lambdahat = 1/mean(dataset1)
lambdahat
ks.test(dataset1, "pexp", lambdahat) #Kolmogorov-Smirnov test

#construct confidence interval for lambda
lowerbound = 1/mean(dataset1)-1/(sqrt(length(dataset1))*mean(dataset1))*qnorm(0.975)
upperbound = 1/mean(dataset1)+1/(sqrt(length(dataset1))*mean(dataset1))*qnorm(0.975)
c(lowerbound, upperbound)

#simulation 2
dataset2 = scan("simu2.txt")
B1 = sqrt(-log(u1))*cos(2*pi*u2)
hist(B1,freq=FALSE)

#simulation 3
dataset3 = scan("simu3.txt")


#FSML2 - session 5___________________________________________________________________________________________

#simulation using Fischer
confvar <- function(n, m, sig1, sig2, alpha)
{
  X = rnorm(n, 0, sqrt(sig1))
  Y = rnorm(n, 0, sqrt(sig2))
  sighat1 = var(X) #empirical variance computed on the sample X
  sighat2 = var(Y) #empirical variance computed on the sample Y
  f1 = qf(alpha/2, n-1, m-1)
  f2 = qf(1-alpha/2, n-1, m-1)
  confvar = c(sighat1/sighat2*1/f2, sighat1/sighat2*1/f1)
}
A = confvar(20, 25, 1, 1.5, 0.05)
A
B = confvar(500, 500, 1, 1.5, 0.05)
B
C = confvar(50, 50, 1, 1.5, 0.05)
C