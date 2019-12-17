#ASML - session 3___________________________________________________________________________________________

A = matrix(0, ncol=3, nrow=50)
A[,1] = rexp(50, 0.2)
A[,2] = rexp(50, 7)
A[,3] = runif(50, -5, 6)
Y = 5-3*A[,1] + 2*A[,2] - A[,3] + rnorm(50)
L = lm(Y~., data=as.data.frame(A))
summary(L)

Ab = runif(50, 2, 8)
A = cbind(A, Ab)
Y = 5-3*A[,1] + 2*A[,2] + rnorm(50)
dim(A)
L = lm(Y~., data=as.data.frame(A))
summary(L)

Y = 5-3*A[,1] + 2*A[,3] + rnorm(50)
L1 = lm(Y~., data=as.data.frame(A[,1]))
summary(L1)

L2 = lm(Y~., data=as.data.frame(A[,1:2]))
summary(L2)

L3 = lm(Y~., data=as.data.frame(A[,1:3]))
summary(L3)

L4 = lm(Y~., data=as.data.frame(A[,1:4]))
summary(L4)


#Exercises with datasets

S = rep(1:4, times=3)
S
S = rep(1:4, each=3) #quantitative variable
S
summary(S)
T = factor(S) #qualitative variable
T
summary(T)
R = factor(rep(c('A', 'B', 'C'), 4))
R
summary(R)
W = cbind(S,T)
is.matrix(W)
W[1,1]+W[1,2]
Wb = data.frame(X1=S, X2=T)
Wb
Wb$X1
Wb$X2
write(t(Wb), file = 'test.txt', ncolumns=2)
Wbb = read.table('test.txt')
Wbb$V1
Wbb$V2
#here we showed that creating a .txt file is not the right way to same the data generated
save(Wb, file = 'test.RData')
Xwb = load('test.RData')
Xwb
Wb
Wb$X1
Wb$X2


getwd()
setwd('C:/Users/Lisa/Datasets_ASML')
dir()

Y1 = read.table('Y1.txt')
head(Y1)
#if a header is already integrated into the file, it can be skipped by:
#Y1 = read.table('Y1.txt', header=TRUE)

A1 = read.table('A1.txt')
head(A1)

is.matrix(Y1)
is.data.frame(Y1)
Y1 = as.matrix(Y1)
Y1[2,1]+Y1[3,1]
hist(Y1)
L1 = lm(Y1~., data=A1)
L1
summary(L1)
#linear model doesn't seem to be the right approach because the R-squared are close to 0

Y2 = read.table('Y2.txt')
head(Y2)
L2 = lm(as.matrix(Y2)~.,data=A1)
summary(L2)
plot(L2)

cor(A1)

Y3 = read.table('Y3.txt')
head(Y3)
A3 = read.table('A3.txt')
head(A3)
Y3 = as.matrix(Y3)
L3 = lm(Y3~.,data=A3)
summary(L3)
plot(L3)
cor(A3)

Y4 = read.table('Y4.txt')
head(Y4)
A4 = read.table('A4.txt')
head
dim(A4)
Y4 = as.matrix(Y4)
L4 = lm(Y4~.,data=A4)
L4
summary(L4)
plot(L4)
hist(L4$residuals)

L1 = lm(as.matrix(Y2)~A1[,1])
L1
summary(L1)
L1 = lm(as.matrix(Y2)~A1[,2])
L1
summary(L1)
L1 = lm(as.matrix(Y2)~A1[,3])
L1
summary(L1)
L1 = lm(as.matrix(Y2)~A1[,4])
L1
summary(L1)
L1 = lm(as.matrix(Y2)~A1[,5])
L1
summary(L1)
L2 = lm(as.matrix(Y2)~., data = A1[,c(5,1)])
summary(L2)
L2 = lm(as.matrix(Y2)~., data = A1[,c(5,2)])
summary(L2)
L2 = lm(as.matrix(Y2)~., data = A1[,c(5,3)])
summary(L2)
L2 = lm(as.matrix(Y2)~., data = A1[,c(5,4)])
summary(L2)
L3 = lm(as.matrix(Y2)~., data = A1[,c(5,1,2)])
summary(L3)
L3 = lm(as.matrix(Y2)~., data = A1[,c(5,1,3)])
summary(L3)
L3 = lm(as.matrix(Y2)~., data = A1[,c(5,1,4)])
summary(L3)
