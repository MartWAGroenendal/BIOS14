x = rnorm(50, 10, 2)
mat=matrix(nrow=1000,ncol=2)
for(i in 1:1000){
sample<-sample(x, replace=TRUE)
CV<-sd(sample)/mean(sample)
mat[i,1]<-CV
xnew=log(sample)
SDlog<-sd(xnew)
mat[i,2]<-SDlog
}
plot(mat[1:1000,1],mat[1:1000,2] )

