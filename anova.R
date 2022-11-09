set.seed(100)
groups = as.factor(rep(c("Small", "Medium", "Large"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))
plot(groups, x, las=1, xlab="")
for (i in 1:length(levels(groups))){
     points(aa[aa==i]-0.5,x[aa==i])
     }
m=lm(x~groups-1)
a<-anova(m)
SS_T=sum(a$`Sum Sq`[1:2])
SS_Tcorr=SS_T/(sum(a$Df[]))
varx=var(x)
F=a$`Mean Sq`[1]/a$`Mean Sq`[2]

