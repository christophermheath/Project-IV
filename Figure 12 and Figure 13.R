install.packages("ExtDist")
library(ExtDist)
install.packages("VGAM")
library(VGAM)
install.packages("sensitivity")
library(sensitivity)
install.packages("calibrate")
library(calibrate)

### Function for Cp ###

CostDyke <- function(Zv,Q,B,Ks,Zm,L,Hd,Cb) {
  a = B* Ks * ((Zm-Zv)/L)^0.5
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
  return(mapply(CostDyke, my.data[,1], my.data[,2], my.data[,3],my.data[,4],my.data[,5],my.data[,6],
                my.data[,7],my.data[,8]))
}


### Function for S ###

SDyke <- function(Zv,Q,B,Ks,Zm,L,Hd,Cb) {
  a = B* Ks * ((Zm-Zv)/L)^0.5
  S = Zv + (Q/a)^0.6 - Hd - Cb
  return(S)
}

SDykeApply <- function (my.data) {
  return(mapply(SDyke, my.data[,1], my.data[,2], my.data[,3],my.data[,4],my.data[,5],my.data[,6],
                my.data[,7],my.data[,8]))
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
B <- c(rTriangular(2000000,295,300)) #3 B
Ks <- c(rtnorm(2000000,30,sqrt(8),15,999)) #4 Ks  
Zm <- c(rTriangular(2000000,54,56)) #5 Zm
L <- c(rTriangular(2000000,4990,5010)) #6 L
Hd <- c(runif(2000000,7,9)) #7 Hd
Cb <- c(rTriangular(2000000,55,56)) #8 Cb

X = cbind(Zv,Q,B,Ks,Zm,L,Hd,Cb)

s1 = X[1:1000000,]
s2 = X[1000001:2000000,]


### Applying Sobol ###

SobSDyke <- sobol2002(model=SDykeApply,s1,s2,nboot=100)
print(SobSDyke)

SobCostDyke <- sobol2002(model=DykeApply,s1,s2,nboot=100)
print(SobCostDyke)

plot(SobSDyke,ylim=c(0,0.6))
title(main="S Output, Main and Total Effects",xlab="Parameter")

plot(SobCostDyke,ylim=c(0,0.6))
title(main="Cp Output, Main and Total Effects",xlab="Parameter")


### Applying Morris ###

Smodyke <- morris(model=SDykeApply, factors=8, r=16,design = list(type = "oat",
			 levels = 10, grid.jump = 5), binf=c(49,500,295,15,54,4990,7,55),bsup=c(51,3000,300,999,56,5010,9,56))
print(Smodyke)
plot(Smodyke)

Codyke <- morris(model=DykeApply, factors=8, r=16, design = list(type = "oat",
			 levels = 1000, grid.jump = 550),binf=c(49,500,295,15,54,4990,7,55),bsup=c(51,3000,300,999,56,5010,9,56))
print(Codyke)
plot(Codyke)


plot(Smodyke,identify=TRUE)
title(main="S Output")

plot(Codyke,identify=TRUE)
title(main="Cp Output")


