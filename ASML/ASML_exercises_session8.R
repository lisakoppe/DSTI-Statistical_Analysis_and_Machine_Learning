#ASML - session 8___________________________________________________________________________________________

ozone = read.table("Dataset_ozone.txt", header=TRUE, sep=';', dec=",")

#we need the library ggplot2
ggplot(ozone, aes(x=T12, y=maxO3)) +
  geom_point() +
  xlab('T12') +
  ylab('maxo3')

reg_simp = lm(maxO3~T12, data=ozone)
reg_simp
summary(reg_simp)
plot(reg_simp)

ggplot(ozone, aes(x=T12, y=maxO3)) +
  geom_point() +
  stat_smooth(method="lm", se=FALSE)
xlab('T12') + ylab('maxo3')

maxO3_ajusted <- reg_simp$fitted.values
ggplot(ozone, aes(x=maxO3, y=maxO3_ajusted)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, color='red') +
  xlab('max03') +
  ylab('max03_adjusted')


#multiple linear model only just the quantitative explanatory variables
reg_multi = lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=ozone)
reg_multi


#PCA
ozonepca = ozone[,3:12]
pcaozone = princomp(ozonepca)  #non normalized PCA
pcanozone = princomp(ozonepca, cor=TRUE)   # normalized PCA


bager <- function(ozonel, ozonet)
{
  err = c()
  for (k in 1:500)
  {
    A = bagging(maxO3~., data=ozonel, nbag=k)
    bagt = predict(A, newdata=ozonet)
    e = 1 / nrow(ozonet) * sum((bagt-ozonet$maxO3)^2)
    err = c(err, e)
  }
  bager = err
}

plot(err, type='l')
