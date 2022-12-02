#Start with a clean slate
rm(list=ls())

# #install and load required packages
# install.packages("lavaan", dependencies = TRUE) #used to draw a path diagram
# library("psych") #used for variable relationships exploration
# library("dplyr") #used for data cleaning with pipe operators
# library("lavaan") #used to draw a path diagram
# library(ggplot2)
# library(MASS)
# library(ggtext)
# library(MuMIn)

#import the data
# ******* SET YOUR WORKING DIRECTORY HERE USING setwd("desiredworkingdirectory") **********
dat<-read.csv("Eulaema.csv")

#what are our variables?
# We have the response variable (Eulaema_nigrita), two methodological variables (method and effort), and seven environmental variables (altitude, MAT, MAP, Tseason, Pseason, forest, lu_het)
#data exploration, do we see interesting relationships?
pairs.panels(dat)

#To ease referring to variables, I here extract and rename the variables from the data set and give them new names
count=dat$Eulaema_nigrita
effort=dat$effort
alt=dat$altitude
forest=dat$forest.
MAP=dat$MAP
MAT=dat$MAT
Pseason=dat$Pseason
Tseason=dat$Tseason
meth=dat$method

#Correct count for effort
cc=count/exp(effort) #This will indeed make cc non-integer, but R uses a certain continuous Poisson process, so the model still works, while maintaining data accuracy and integrity.

#Model fitting
m=glm(cc~MAP, family="poisson") # fit a poisson model, as we have count data that is not normally distributed
#residuals are too great for the degrees of freedom, i.e. we have overdispersion, hence we need another parameter theta, and we fit a negative binomial model
m = glm.nb(cc~MAP, link=log) #Fit a negative binomial model using log as link function
#Predicted data according to the model, creates the values for your regression line
xx = seq(min(MAP), max(MAP), length.out= 200)
y_hat = predict(m, newdata=list(MAP=xx),type="response", se.fit = T) #using type = response makes that we do not need to backtransform the data ourselves anymore
y_hatfit<-unlist(y_hat$fit)
y_hatfit=as.numeric(y_hatfit)
# Creates the borders for the confidence intervals
y_min=qnorm(0.005, y_hat$fit, y_hat$se.fit)
y_max=qnorm(0.995,y_hat$fit, y_hat$se.fit)
y_min2=qnorm(0.025, y_hat$fit, y_hat$se.fit)
y_max2=qnorm(0.975,y_hat$fit, y_hat$se.fit)
y_min3=qnorm(0.05, y_hat$fit, y_hat$se.fit)
y_max3=qnorm(0.95,y_hat$fit, y_hat$se.fit)

#Combine all that is to be plotted into one dataframe
pred=cbind(cc, MAP, xx, y_hatfit, y_min,y_max, y_min2, y_max2, y_min3, y_max3)
predi=as.data.frame(pred)

#Plot the data 
myplot<-ggplot(data=predi, aes(x=MAP, y=cc))+
  geom_point()+
  geom_ribbon(data=predi, aes(ymin=y_min3, ymax=y_max3, x=xx, fill="90", alpha="90"))+
  geom_ribbon(data=predi, aes(ymin=y_min2, ymax=y_min3, x=xx, fill="95", alpha="95"))+
  geom_ribbon(data=predi, aes(ymin=y_max3, ymax=y_max2, x=xx, fill="95",alpha="95"))+
  geom_ribbon(data=predi, aes(ymin=y_min, ymax=y_min2, x=xx, fill="99", alpha="99"))+
  geom_ribbon(data=predi, aes(ymin=y_max2, ymax=y_max, x=xx, fill="99", alpha="99"))+
  geom_line(data=predi, aes(x=xx, y=y_hatfit))+
  scale_fill_manual(values = c("90" = "red", "95" = "red", "99" = "red"), guide=NULL)+ 
  scale_alpha_manual(values = c("90" = 0.5, "95" = 0.3, "99" = 0.1), labels = c("90" = "90% confidence interval","95" = "95% confidence interval", "99" = "99% confidence interval" ))+ 
  guides(alpha = guide_legend(title="Confidence intervals", override.aes= list(fill = "red")))+
  labs(x="Mean Annual Precipitation (mm)",
       y=expression(paste("Number of ", italic("Eulaema nigrita"), " caught per hour (1/h)")))+
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0, size=12))
myplot

#Now fit a model using precipitation seasonality as additional predictor variable
c=corr.test(MAP, Pseason)
if (abs(c$r)<0.6){
  mps = glm.nb(cc~MAP + Pseason, link=log) #model fit is done in the log world!
mpsco=mps$coefficients
xx = seq(min(MAP), max(MAP), length.out= 200)
y_hatmean = mpsco[1]+mpsco[2]*xx + mpsco[3]*mean(Pseason)
y_hatpsd=mpsco[1]+mpsco[2]*xx + mpsco[3]*(mean(Pseason)+sd(Pseason))
y_hatmsd=mpsco[1]+mpsco[2]*xx + mpsco[3]*(mean(Pseason)-sd(Pseason))
y_hatp2sd=mpsco[1]+mpsco[2]*xx + mpsco[3]*(mean(Pseason)+2*sd(Pseason))
y_hatm2sd=mpsco[1]+mpsco[2]*xx + mpsco[3]*(mean(Pseason)-2*sd(Pseason))
y_hats=cbind(y_hatmean, y_hatpsd, y_hatmsd, y_hatp2sd, y_hatm2sd)
y_hats=exp(y_hats) #backtransform data so that it represents the real world values and we can plot it

#combine all to be plotted into a single dataframe
actdata=cbind(cc, MAP)
simdata=cbind(xx,y_hats)
actdata=as.data.frame(actdata)
simdata=as.data.frame(simdata)

#plot
myplot2<-ggplot(data=actdata, aes(x=MAP, y=cc))+
  geom_point()+
  geom_line(data=simdata, aes(x=xx, y=y_hatfit, linetype="solid"))+
  geom_line(data=simdata, aes(x=xx, y=y_hatpsd), linetype="dashed")+
  geom_line(data=simdata, aes(x=xx, y=y_hatmsd), linetype="dashed")+
  geom_line(data=simdata, aes(x=xx, y=y_hatp2sd), linetype="dotted")+
  geom_line(data=simdata, aes(x=xx, y=y_hatm2sd), linetype="dotted")+
  # scale_fill_manual(values = c("90" = "red", "95" = "red", "99" = "red"), guide=NULL)+ 
  # scale_alpha_manual(values = c("90" = 0.5, "95" = 0.3, "99" = 0.1), labels = c("90" = "90% confidence interval","95" = "95% confidence interval", "99" = "99% confidence interval" ))+ 
  # guides(alpha = guide_legend(title="Confidence intervals", override.aes= list(fill = "red")))+
  labs(x="Mean Annual Precipitation (mm)",
       y=expression(paste("Number of ", italic("Eulaema nigrita"), " caught per hour (1/h)")) ) +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(linetype= FALSE)
myplot2
}

# Obtain summary statistics of the dataset
sum<-describe(dat)
write.csv(sum, file = 'sum.csv')