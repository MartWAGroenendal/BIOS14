set.seed(85)
x=rnorm(n=200,mean=10, sd=2)
y=0.4*x+rnorm(200,0,1)
plot(x,y,las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

m = lm(y~x)
cf = summary(m)$coef
predvals = cf[1,1] + cf[2,1]*x
par(mfrow=c(1,2))
plot(x, y, las=1)
abline(m)
segments(x, y, x, predvals)
hist(residuals(m), las=1)

