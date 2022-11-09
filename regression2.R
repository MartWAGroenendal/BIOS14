rm(list=ls())
par(mfrow=c(1,1))
set.seed(85)
x=rnorm(n=200,mean=10, sd=2)
y=0.4*x+rnorm(200,0,1)
mm=lm(y~x)
coefs<-summary(mm)$coef
slope = NULL
dataframe=data.frame(x,y)
for(i in 1:1000){
  dat<-dataframe[sample(nrow(dataframe), replace=T),]
  m=lm(dat$y~dat$x)
  cf = summary(m)$coef
  slope[i]=cf[2,1]
}
hist(slope, las=1)

stdout=sd(slope)
reg=cov(y,x)/var(x)
#for ychange, use the coefficients of the original data set, not the bootstrapped ones
ychange=(coefs[2,1]*(mean(x) + sd(x))) - (coefs[2,1]*mean(x))
rsquared=cor(x,y)^2
y_hat = coefs[1,1] + coefs[2,1]*x
r2=var(y_hat)/var(y)
y_hat_diff=coefs[2,1]ˆ2*var(x)
lm()