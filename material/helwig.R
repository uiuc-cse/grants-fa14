############***############   CSE Big Data Workshop   ############***############
# Nathaniel E. Helwig      
# Department of Statistics 
# University of Illinois   
# May 29, 2014



#########***#########   load big packages and data   #########***#########

# library function loads package
library(bigmemory)      # ver 4.4.6
library(biganalytics)   # ver 1.1.1
library(biglm)          # ver 0.9-1
library(bigsplines)     # ver 1.0-1

# load data as class "big.matrix" with "short" data structure
mypath="/Users/Nate/Desktop/CSE_2014/Rcode/data/"
flights<-read.big.matrix(filename=paste(mypath,"flights.csv",sep=""),
                         col.names=c("Year","Month","DepHour","DepDelay","ArrDelay"),
                         type="short",backingfile="flights.bin",backingpath=mypath,
                         descriptorfile="flights.desc")
class(flights)
flights[1:4,]

# can reload data using "attach.big.matrix" function:
library(bigmemory)
mypath="/Users/Nate/Desktop/CSE_2014/Rcode/data/"
flights<-attach.big.matrix("flights.desc",backingfile="flights.bin",backingpath=mypath)
flights[1:4,]



#########***#########   look at descriptive statistics   #########***#########

# print data dimensions and column names
dim(flights)
colnames(flights)

# look at variable ranges
apply(flights,2,range)

# look at variable means
apply(flights,2,mean)

# look at correlation between DepDelay and ArrDelay
cor(flights[,4],flights[,5])



#########***#########   simple linear regression (biglm.big.matrix)   #########***#########

# fit big linear regression model (using big.matrix interface)
linmod=biglm.big.matrix(ArrDelay~DepDelay,data=flights)
linsum=summary(linmod)
linsum
linsum$rsq

# create prediction function
newdata=data.frame(DepDelay=seq(1,120,length=200),ArrDelay=rep(0,200))
linpred=predict(linmod,newdata,se.fit=TRUE,make.function=TRUE)
yhat=linpred[[1]](newdata$DepDelay)
yhatse=sqrt(diag(linpred[[2]](newdata$DepDelay)))

# plot regression line with pointwise confidence intervals
x11(width=6,height=6)
plot(newdata$DepDelay,yhat,type="l",
     xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"linmod.pdf",sep=""))



#########***#########   simple linear regression (biglm)   #########***#########

# get subset of data
set.seed(123)
subidx=sample.int(nrow(flights),10^6)
flightsub=as.data.frame(flights[subidx,])

# fit big linear regression model (using biglm)
linmods=biglm(ArrDelay~DepDelay,data=flightsub)
linsums=summary(linmods)
linsum
linsums

# create prediction function
linpred=predict(linmods,newdata,se.fit=TRUE,make.function=TRUE)
yhat=linpred[[1]](newdata$DepDelay)
yhatse=sqrt(diag(linpred[[2]](newdata$DepDelay)))

# plot regression line with pointwise confidence intervals
x11(width=6,height=6)
plot(newdata$DepDelay,yhat,type="l",
     xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"linmods.pdf",sep=""))



#########***#########   multiple linear regression (biglm.big.matrix)   #########***#########

# fit big linear regression model (using big.matrix interface)
mlrmod=biglm.big.matrix(ArrDelay~Year+Month+DepHour+DepDelay,data=flights)
mlrsum=summary(mlrmod)
mlrsum
mlrsum$rsq

# create prediction function
newdata=data.frame(Year=seq(2003,2008,length=200),
                   Month=seq(1,12,length=200),
                   DepHour=seq(1,24,length=200),
                   DepDelay=seq(1,120,length=200),
                   ArrDelay=rep(0,200))
mlrpred=predict(mlrmod,newdata,se.fit=TRUE,make.function=TRUE)

# set up plot
mfs=apply(flights,2,mean)
x11(width=12,height=6)
par(mfrow=c(2,2))

# plot line and 95% pointwise CI for Year
yhat=mlrpred[[1]](cbind(newdata[,1],mfs[2],mfs[3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(newdata[,1],mfs[2],mfs[3],mfs[4]))))
plot(newdata$Year,yhat,type="l",
     xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for Month
yhat=mlrpred[[1]](cbind(mfs[1],newdata[,2],mfs[3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],newdata[,2],mfs[3],mfs[4]))))
plot(newdata$Month,yhat,type="l",
     xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for DepHour
yhat=mlrpred[[1]](cbind(mfs[1],mfs[2],newdata[,3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],mfs[2],newdata[,3],mfs[4]))))
plot(newdata$DepHour,yhat,type="l",
     xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for DepDelay
yhat=mlrpred[[1]](cbind(mfs[1],mfs[2],mfs[3],newdata[,4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],mfs[2],mfs[3],newdata[,4]))))
plot(newdata$DepDelay,yhat,type="l",
     xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"mlrmod.pdf",sep=""))



#########***#########   multiple linear regression (biglm)   #########***#########

# fit big linear regression model (using biglm)
mlrmods=biglm(ArrDelay~Year+Month+DepHour+DepDelay,data=flightsub)
mlrsums=summary(mlrmods)
mlrsum
mlrsums

# create prediction function
newdata=data.frame(Year=seq(2003,2008,length=200),
                   Month=seq(1,12,length=200),
                   DepHour=seq(1,24,length=200),
                   DepDelay=seq(1,120,length=200),
                   ArrDelay=rep(0,200))
mlrpred=predict(mlrmods,newdata,se.fit=TRUE,make.function=TRUE)

# set up plot
mfs=apply(flightsub,2,mean)
x11(width=12,height=6)
par(mfrow=c(2,2))

# plot line and 95% pointwise CI for Year
yhat=mlrpred[[1]](cbind(newdata[,1],mfs[2],mfs[3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(newdata[,1],mfs[2],mfs[3],mfs[4]))))
plot(newdata$Year,yhat,type="l",
     xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for Month
yhat=mlrpred[[1]](cbind(mfs[1],newdata[,2],mfs[3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],newdata[,2],mfs[3],mfs[4]))))
plot(newdata$Month,yhat,type="l",ylim=c(27.2,27.6),
     xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for DepHour
yhat=mlrpred[[1]](cbind(mfs[1],mfs[2],newdata[,3],mfs[4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],mfs[2],newdata[,3],mfs[4]))))
plot(newdata$DepHour,yhat,type="l",
     xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% pointwise CI for DepDelay
yhat=mlrpred[[1]](cbind(mfs[1],mfs[2],mfs[3],newdata[,4]))
yhatse=sqrt(diag(mlrpred[[2]](cbind(mfs[1],mfs[2],mfs[3],newdata[,4]))))
plot(newdata$DepDelay,yhat,type="l",
     xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"mlrmods.pdf",sep=""))



#########***#########   nonparametric regression (full data)   #########***#########

# set up plot
x11(width=12,height=6)
par(mfrow=c(2,2))

# Year vs. ArrDelay using cubic spline with 4 knots
smod=bigspline(flights[,1],flights[,5],nknots=4)
newdata=data.frame(Year=seq(2003,2008,length=50))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Year,yhat,type="l",xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")


# Month vs. ArrDelay using periodic cubic spline with 6 knots
smod=bigspline(flights[,2],flights[,5],type="per",nknots=6)
newdata=data.frame(Month=seq(1,12,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Month,yhat,type="l",xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# DepHour vs. ArrDelay using periodic cubic spline with 12 knots
smod=bigspline(flights[,3],flights[,5],type="per",nknots=12)
newdata=data.frame(DepHour=seq(1,24,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepHour,yhat,type="l",xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# DepDelay vs. ArrDelay using cubic spline with 10 knots
smod=bigspline(flights[,4],flights[,5],type="cub",nknots=10)
newdata=data.frame(DepDelay=seq(1,120,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepDelay,yhat,type="l",xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"nonmod.pdf",sep=""))



#########***#########   nonparametric regression (subset data)   #########***#########

# set up plot
x11(width=12,height=6)
par(mfrow=c(2,2))

# Year vs. ArrDelay using cubic spline with 4 knots
smod=bigspline(flightsub[,1],flightsub[,5],nknots=4)
newdata=data.frame(Year=seq(2003,2008,length=50))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Year,yhat,type="l",xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")
title(bquote(R^2==.(round(smod$info[2],4))))


# Month vs. ArrDelay using periodic cubic spline with 6 knots
smod=bigspline(flightsub[,2],flightsub[,5],type="per",nknots=6)
newdata=data.frame(Month=seq(1,12,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Month,yhat,type="l",xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# DepHour vs. ArrDelay using periodic cubic spline with 12 knots
smod=bigspline(flightsub[,3],flightsub[,5],type="per",nknots=12)
newdata=data.frame(DepHour=seq(1,24,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepHour,yhat,type="l",xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# DepDelay vs. ArrDelay using cubic spline with 10 knots
smod=bigspline(flightsub[,4],flightsub[,5],type="cub",nknots=10)
newdata=data.frame(DepDelay=seq(1,120,length=200))
spred=predict(smod,newdata,se.fit=TRUE)
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepDelay,yhat,type="l",xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)",main=bquote(R^2==.(round(smod$info[2],4))))
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"nonmods.pdf",sep=""))



#########***#########   semiparametric regression   #########***#########

# fit semiparametric model
smod=bigssp(ArrDelay~Year+Month+DepHour+DepDelay,data=flightsub,
            type=list(Year="cub",Month="per",DepHour="per",DepDelay="prm"),nknots=30,
            rparm=list(Year=0.01,Month=0.01,DepHour=0.01,DepDelay=5),skip.iter=FALSE)
smod

# set up plot
x11(width=12,height=6)
par(mfrow=c(2,2))

# plot line and 95% Bayesian CI for Year
newdata=data.frame(Year=seq(2003,2008,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="Year")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Year,yhat,type="l",ylim=c(-.8,.4),xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)",main="Year effect")
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for Month
newdata=data.frame(Month=seq(1,12,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="Month")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Month,yhat,type="l",ylim=c(-.5,.5),xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)",main="Month effect")
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for DepHour
newdata=data.frame(DepHour=seq(1,24,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="DepHour")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepHour,yhat,type="l",xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)",main="DepHour effect")
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for DepDelay
newdata=data.frame(DepDelay=seq(1,120,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="DepDelay")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepDelay,yhat,type="l",xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)",main="DepDelay effect")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"semimod.pdf",sep=""))



#########***#########   nonparametric regression (all predictors)   #########***#########

# fit nonparametric model
smod=bigssp(ArrDelay~Year+Month+DepHour+DepDelay,data=flightsub,
            type=list(Year="cub",Month="per",DepHour="per",DepDelay="cub"),nknots=30,
            rparm=list(Year=0.02,Month=0.01,DepHour=0.01,DepDelay=0.02),skip.iter=FALSE)
smod

# set up plot
x11(width=12,height=6)
par(mfrow=c(2,2))

# plot line and 95% Bayesian CI for Year
newdata=data.frame(Year=seq(2003,2008,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="Year")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Year,yhat,type="l",ylim=c(-.8,.4),xlab="Departure Year",
     ylab="Exp. Arrival Delay (min)",main="Year effect")
lines(newdata$Year,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Year,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for Month
newdata=data.frame(Month=seq(1,12,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="Month")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$Month,yhat,type="l",ylim=c(-.5,.5),xlab="Departure Month",
     ylab="Exp. Arrival Delay (min)",main="Month effect")
lines(newdata$Month,yhat+2*yhatse,lty=2,col="red")
lines(newdata$Month,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for DepHour
newdata=data.frame(DepHour=seq(1,24,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="DepHour")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepHour,yhat,type="l",xlab="Departure Time (24 hr)",
     ylab="Exp. Arrival Delay (min)",main="DepHour effect")
lines(newdata$DepHour,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepHour,yhat-2*yhatse,lty=2,col="red")

# plot line and 95% Bayesian CI for DepDelay
newdata=data.frame(DepDelay=seq(1,120,length=50))
spred=predict(smod,newdata,se.fit=TRUE,include="DepDelay")
yhat=spred[[1]]
yhatse=spred[[2]]
plot(newdata$DepDelay,yhat,type="l",xlab="Departure Delay (min)",
     ylab="Exp. Arrival Delay (min)",main="DepDelay effect")
lines(newdata$DepDelay,yhat+2*yhatse,lty=2,col="red")
lines(newdata$DepDelay,yhat-2*yhatse,lty=2,col="red")
#dev.copy2pdf(file=paste(mypath,"semimods.pdf",sep=""))


