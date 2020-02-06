#ASML - session 6___________________________________________________________________________________________

library(rpart)
data(iris)
Tree1 = rpart(iris$Species~., data=iris[,-5])
Tree1
Tree2 = rpart(iris$Species~., data=iris[,-5], control=rpart.control(cp=10^-9, minsplit=2))
printcp(Tree2)
A = printcp(Tree2)
alpha = A[,1]
K = length(alpha)
for (i in 1:(K-1))
  {
  T = prune(Tree2, cp=alpha[K+1-i])
  plot(T)
  #text(T)
  }

TT1 = rpart(iris$Species~., data=iris[,-5], control=rpart.control(cp=10^-9,minsplit=2))
TT2 = rpart(iris$Species~., data=iris[,-5], control=rpart.control(cp=10^-9,minsplit=2))
TT3 = rpart(iris$Species~., data=iris[,-5], control=rpart.control(cp=10^-9,minsplit=2))

u = sample(1:150,120)
learning = iris[u,]
test = iris[-u,]
Tree = rpart(learning[,5]~., data=learning[,-5], cp=0.02, minsplit=2)
predict(Tree)
predict(Tree, type='class')
Tree
predict(Tree)
