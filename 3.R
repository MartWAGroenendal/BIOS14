rm(list=ls())

set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))
out = NULL
CV= NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
  CV[i]=sd(sample)/mean(sample)
}
hist(out, las=1, main="")
sd<-sd(out)


conf<-quantile(CV, c(0.025, 0.975))

