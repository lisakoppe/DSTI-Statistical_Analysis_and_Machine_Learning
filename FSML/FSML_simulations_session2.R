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
