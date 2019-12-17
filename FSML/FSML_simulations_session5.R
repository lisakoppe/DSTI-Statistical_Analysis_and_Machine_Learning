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
