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


#### Comparing ####

SobCost <- sobol(model=DykeApply,s1,s2,order=4,nboot=100)
print(SobCost)
plot(SobCost)



####

samples <- function(n){
  Zv <- c(rTriangular(2*n,49,51)) #1 Zv
  Q <- c(rtgumb(2*n,1013,558,500,3000)) #2 Q
  B <- c(rTriangular(2*n,295,300)) #3 B
  Ks <- c(rtnorm(2*n,30,8,15,999)) #4 Ks  
  Zm <- c(rTriangular(2*n,54,56)) #5 Zm
  L <- c(rTriangular(2*n,4990,5010)) #6 L
  Hd <- c(runif(2*n,7,9)) #7 Hd
  Cb <- c(rTriangular(2*n,55,56)) #8 Cb
  X = cbind(Zv,Q,B,Ks,Zm,L,Hd,Cb)
  s1 = X[1:n,] 
  s2 = X[(n+1):(2*n),]
  a = sobol(model=DykeApply,s1,s2,order=1)
  one = a$S[,1]
  return(one)
}

limiting <- function(n){
  i <- 1
  prices = 0
  repeat {
    prices <- samples(n) + prices
    i <- i+1
    if(i > 100)
      break
  }
  return(prices/100)
}


one = limiting(10)
two = limiting(50)
three = limiting(100)
four = limiting(500)
five = limiting(1000)
six = limiting(2000)
seven = limiting(5000)
eight = limiting(8000)
nine = limiting(10000)


## Combining ##

N = c(10,50,100,500,1000,2000,5000,8000,10000)
S1 = c(one[1],two[1],three[1],four[1],five[1],six[1],seven[1],eight[1],nine[1])
S2 = c(one[2],two[2],three[2],four[2],five[2],six[2],seven[2],eight[2],nine[2])
S3 = c(one[3],two[3],three[3],four[3],five[3],six[3],seven[3],eight[3],nine[3])
S4 = c(one[4],two[4],three[4],four[4],five[4],six[4],seven[4],eight[4],nine[4])
S5 = c(one[5],two[5],three[5],four[5],five[5],six[5],seven[5],eight[5],nine[5])
S6 = c(one[6],two[6],three[6],four[6],five[6],six[6],seven[6],eight[6],nine[6])
S7 = c(one[7],two[7],three[7],four[7],five[7],six[7],seven[7],eight[7],nine[7])
S8 = c(one[8],two[8],three[8],four[8],five[8],six[8],seven[8],eight[8],nine[8])
s = c(S1[9],S2[9],S3[9],S4[9],S5[9],S6[9],S7[9],S8[9])

plot(N,S2)

