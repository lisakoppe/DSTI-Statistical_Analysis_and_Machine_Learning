#ASML - session 5___________________________________________________________________________________________

getwd()
setwd('C:/Users/Lisa/Datasets_ASML')
dir()

ozone <- read.table('ozone.txt', header=TRUE)
head(ozone)
dim(ozone)

#how to see if there is an influence of the wind on the concentration is ozone (maxO3)
attach(ozone)
vent
summary(ozone[,c("maxO3","vent")])

#boxplot of the variable maxO3 with respect to the labels of wind
plot(maxO3~vent,data=ozone,pch=15,cex=0.5,col='green')
model <- aov(maxO3~vent)
model

#before analyzing the outputs of tests, we need to verify assumptions on the noise:
#- independency: by looking the experimentation
#- gaussianity: normal qq-plot, Kolmogorov test, shapiro test
#- homoscedasticity (same variances): bartlett test, levene test

#equality of the variances
Var1 = var(maxO3[vent=='Est'])
Var2 = var(maxO3[vent=='Nord'])
Var3 = var(maxO3[vent=='Ouest'])
Var4 = var(maxO3[vent=='Sud'])
Var1
Var2
Var3
Var4
summary(vent)

#levene test:
summary(aov(abs(model$res)~vent))

#bartlett test:
bartlett.test(model$res~vent)
#here the conclusion is that we can accept the equality of the variances

#for gaussianity:
select.est <- ozone[,"vent"]=="Est"
select.est

#We test the normality of the sample associated to the label "Est"
shapiro.test(ozone[select.est, "maxO3"])
#here we are on one label, this explains why we can work directly on the response variable and not on the residuals
#if we work on the whole dataset, we have to consider the residuals because the responses for all the individuals are not identically distributed (not the same expectation)

#we have to do this for each label and for the whole
qqnorm(ozone[select.est, "maxO3"])
#conclusion: the assumptions on the noise seem to be satisfied

ozone.aov <- aov(maxO3~vent)
ozone.aov
summary(ozone.aov)

res.ozone <- rstudent(ozone.aov)
plot(res.ozone~vent)
dim(ozone)
summary(lm(maxO3~vent,data=ozone)) #constraint with reference to label 1
summary(lm(maxO3~C(vent,base=2),data=ozone)) #constraint with reference cell on label 2
summary(lm(maxO3~C(vent,sum),data=ozone)) #constraint on the sum of the mu_i =0

#Exercise

X = data.frame(T1=c(5,8,7,7,10,8),
               T2=c(4,6,6,3,5,6),
               T3=c(6,4,4,5,4,3),
               T4=c(7,4,6,6,3,5),
               T5=c(9,3,5,7,7,6))

delay <- stack(X)$values #transpose of Y (the big vector)
delay
treatment <- factor(rep(c('T1','T2','T3','T4','T5'),each=6)) #the response
treatment
# OR paste('T',1:5,sep='')
plot(delay~treatment,col='green')
myaov=aov(delay~treatment)
myaov
summary(myaov)
model <- lm(delay~treatment)
model
summary(model)

install.packages('gmodels')
library(gmodels)
cmat <- rbind(" : 2 versus 3"=c(0,1,-1,0,0))
cmat
fit.contrast(myaov,treatment,cmat)
pairwise.t.test(delay,treatment,p.adjust="bonf")
TukeyHSD(myaov)
par(las=1)
plot(TukeyHSD(myaov))


#dataset: arbre
A = load('arbre.RData')
A
arbre
arbre$hetraie
arbre$hauteur
L = lm(hauteur~hetraie,data=arbre)
anova(L)
summary(L)

length(arbre$hauteur)
