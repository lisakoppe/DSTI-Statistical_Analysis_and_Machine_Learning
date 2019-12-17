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
