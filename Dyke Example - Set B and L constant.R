install.packages("ExtDist")
library(ExtDist)
install.packages("VGAM")
library(VGAM)
install.packages("sensitivity")
library(sensitivity)
install.packages("calibrate")
library(calibrate)

### Function for Cp ###

CostDyke <- function(Zv,Q,Ks,Hd,Cb) {
  a = 300 * Ks * ((55-Zv)/5000)^0.5
  S = Zv + (Q/a)^0.6 - Hd - Cb
  p = ifelse(S>0,1,0)
  t = ifelse(Hd>8,1,0)
  u = ifelse(Hd<=8,1,0)
  z = ifelse(S<=0,1,0)
  w = 0.2 + 0.8*(1-exp(-1000/S^4))
  v = (1/20)*(Hd*t+8*u)
  Cp = p + w*z + v
  return(Cp)
}

DykeApply <- function (my.data) {
  return(mapply(CostDyke, my.data[,1], my.data[,2], my.data[,3],my.data[,4],my.data[,5]))
}


### Function for S ###

SDyke <- function(Zv,Q,Ks,Hd,Cb) {
  a = 300* Ks * ((55-Zv)/5000)^0.5
  S = Zv + (Q/a)^0.6 - Hd - Cb
  return(S)
}

SDykeApply <- function (my.data) {
  return(mapply(SDyke, my.data[,1], my.data[,2], my.data[,3],my.data[,4],my.data[,5]))
}


### Creating Samples ###

rtnorm <- function(n,mean,sd,a,b){
    qnorm(runif(n, pnorm(a,mean,sd), pnorm(b,mean,sd)),mean,sd)
}
rtgumb <- function(n,loc,scale,a,b){
    qgumbel(runif(n, pgumbel(a, loc, scale), pgumbel(b, loc, scale)), loc, scale)
}


Zv <- c(rTriangular(2000000,49,51)) #1 Zv
Q <- c(rtgumb(2000000,1013,558,500,3000)) #2 Q
Ks <- c(rtnorm(2000000,30,8,15,999)) #3 Ks  
Hd <- c(runif(2000000,7,9)) #5 Hd
Cb <- c(rTriangular(2000000,55,56)) #6 Cb

X = cbind(Zv,Q,Ks,Hd,Cb)

s1 = X[1:1000000,]
s2 = X[1000001:2000000,]


### Applying Sobol ###

SobSDyke <- sobol2002(model=SDykeApply,s1,s2,nboot=100)
print(SobSDyke)

SobCostDyke <- sobol2002(model=DykeApply,s1,s2,nboot=100)
print(SobCostDyke)

dev.off()
par(mfrow=c(1,2))
plot(SobSDyke)
title(main="S Output, Main and Total Effects",xlab="Parameter")
plot(SobCostDyke)
title(main="Cp Output, Main and Total Effects",xlab="Parameter")


### Applying Morris ###

Smodyke <- morris(model=SDykeApply, factors=6, r=12,design = list(type = "oat",
			 levels = 10, grid.jump = 5), binf=c(49,500,15,54,7,55),bsup=c(51,3000,999,56,9,56))
print(Smodyke)
plot(Smodyke)

Codyke <- morris(model=DykeApply, factors=6, r=12, design = list(type = "oat",
			 levels = 10, grid.jump = 5), binf=c(49,500,15,54,7,55),bsup=c(51,3000,999,56,9,56))
print(Codyke)
plot(Codyke)


par(mfrow=c(2,1))
plot(Smodyke)
title(main="S Output")
plot(Codyke)
title(main="Cp Output")


#### Comparing ####

SobCost <- sobol(model=DykeApply,s1,s2,order=2,nboot=100)
print(SobCost)
plot(SobCost)

