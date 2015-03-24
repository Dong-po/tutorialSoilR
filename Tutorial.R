###############################################################################
### This is a tutorial for the SoilR package
################################################## by Carlos A. Sierra
### 29.10.2013
###############################################################################

#First, install the package if you have not done it.
install.packages("SoilR")

# Now load SoilR to your session
library(SoilR)

#Explore the help
help(package="SoilR")

#Explore the help of function OnepModel
?OnepModel

#Copy and paste the code from the example in function OnepModel
t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep)
t=seq(0,10,by=0.2)
k=0.8 # 1/time
C0=100 # mass
In = 30 # mass/time

Ex=OnepModel(t,k,C0,In)
Ct=getC(Ex)
Rt=getReleaseFlux(Ex)
Rc=getAccumulatedRelease(Ex)

plot(t, Ct, type="l", ylab="Carbon stocks (arbitrary units)",
  xlab="Time (arbitrary units)", lwd=2) 

plot(t, Rt, type="l", ylab="Carbon released (arbitrary units)",
  xlab="Time (arbitrary units)",lwd=2) 

plot(t, Rc, type="l",ylab="Cummulative carbon released (arbitrary units)",
  xlab="Time (arbitrary units)",lwd=2) 

#Let's now make some changes. For example, let's assume we have random inputs

randomInputs=data.frame(time=t,In=rnorm(n=length(t),mean=30,sd=5))
plot(randomInputs,type="l")

Ex2=OnepModel(t,k,C0,In=randomInputs)
Ct2=getC(Ex2)
Rt2=getReleaseFlux(Ex2)

plot(t,Ct2,type="l",col=2,ylab="Carbon stock")
lines(t,Ct)
legend("topright",c("Constant inputs","Random inputs"),col=c(1,2),lty=1,bty="n")

plot(t,Rt2,type="l",col=2,ylab="Carbon release")
lines(t,Rt)
legend("topright",c("Constant inputs","Random inputs"),col=c(1,2),lty=1,bty="n")

#Let's now assume that we have soil temperature data
Temp=rnorm(n=length(t),mean=10,sd=2)

#We can calculate the effects of temperature on decomposition rates
xi=fT.Q10(Temp,Q10=1.4)

plot(t,xi,type="l")
abline(h=1,lty=2)

Xi=data.frame(time=t,xi=xi)

Ex3=OnepModel(t,k,C0,In=30,xi=Xi)
Ct3=getC(Ex3)
Rt3=getReleaseFlux(Ex3)

plot(t,Ct3,type="l",col=4,ylab="Carbon stock")
lines(t,Ct2,col=2)
lines(t,Ct)
legend("topright",c("Constant inputs","Random inputs","Constant inputs, variable temperature"),col=c(1,2,4),lty=1,bty="n")

plot(t,Rt3,type="l",col=4,ylab="Carbon release")
lines(t,Rt2,col=2)
lines(t,Rt)
legend("topright",c("Constant inputs","Random inputs"),col=c(1,2),lty=1,bty="n")

################################################################################
# Example with more complicated model.
# Assume you have ten years of litter inputs and temperatures

Litter=data.frame(year=c(1:10),Litter=rnorm(10,10,2))
plot(Litter,type="l")

years=seq(1,10,by=1/365)
TempData=data.frame(years,Temp=15+sin(2*pi*years)+rnorm(length(years),0,1))
plot(TempData,type="l")

dim(Litter)
dim(TempData)

#Temperature effects
TempEffect=fT.LandT(TempData[,2])

TempEffects=data.frame(years,TempEffect)
plot(TempEffects,type="l")

#Run a three pool model with connections in feedback
?ThreepFeedbackModel

C0=c(10,40,30)

ks=c(1/2,1/5,1/8)
a21=ks[1]*0.3
a12=ks[2]*0.1
a32=ks[2]*0.2
a23=ks[3]*0.1


Ex4=ThreepFeedbackModel(years,ks,a21,a12,a32,a23,C0,In=Litter,xi=TempEffects)
Ct4=getC(Ex4)
Rt4=getReleaseFlux(Ex4)

head(Ct4)
dim(Ct4)

plot(years,rowSums(Ct4),type="l")

plotCPool(years,Ct4,col=1:3,ylim=c(0,max(Ct4)),ylab="Carbon stocks")
legend("topright",c("Pool 1","Pool 2","Pool 3"),lty=1,col=1:3)

plotCPool(years,Rt4,col=1:3,ylim=c(0,max(Rt4)),ylab="Carbon release flux",xlab="Time")
legend("topright",c("Pool 1","Pool 2","Pool 3"),lty=1,col=1:3,bty="n")
