#ASML - session 0___________________________________________________________________________________________

X = rexp(50, 0.4)
Y = 1+1/X
plot(X, Y)

obs <- function(n)
{
  u = c()
  l = 0
  while(l<n)
  {
    a = rexp(1, 0.4)
    if(a>2)
    {
      u = c(u, a)
      l = length(u)
    }

  }
  obs = u
}

A = obs(500)

Y = l + 1/X
plot(X, Y)

Y = l + 1/A
plot(A, Y)

L = lm(Y~A)
L
summary(L)
