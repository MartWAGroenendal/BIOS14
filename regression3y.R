rm(list=ls())
x=rnorm(n=200,mean=10, sd=2)
y=0.4*x+rnorm(200,0,1)
mm=lm(y~x)
coefs<-summary(mm)$coef
rangestd=seq(0,2,0.005)
slope_error=NULL
K=NULL
for(i in 1:length(rangestd)){
  yreal=rnorm(200,mean=y,sd=rangestd[i])
  m=lm(yreal~x)
  cf_error<-summary(m)$coef
  slope_error[i]<-cf_error[2,1]
  K[i]=1-var(yreal-y)/var(yreal)
}
yandy=data.frame(y,yreal)
plot(yandy$y,yandy$xreal)
##plot
newx = seq(min(x), max(x), length.out=200)
predy = coefs[1,1] + coefs[2,1]*newx
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)
plot(rangestd, slope_error, las=1,
     xlab="Error std in x",
     ylab="Estimated slope", type='b')
slope_corr=slope_error/K

slopes=data.frame(slope_error, K, slope_corr)
lines(rangestd, slope_corr, las=1, col='red')
