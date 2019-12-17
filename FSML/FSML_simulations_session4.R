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
