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
