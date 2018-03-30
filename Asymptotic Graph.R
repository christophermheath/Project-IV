install.packages("sensitivity")
library(sensitivity)

## Taking Samples ##

x <- c(runif(10000,min=-pi,max=pi))
y <- c(runif(10000,min=-pi,max=pi))
z <- c(runif(10000,min=-pi,max=pi))

X = cbind(x,y,z)

s1 = X[1:5000,]
s2 = X[5001:10000,]

Ishfun <- function (a,b,c) {
  Y = sin(a) + 7*sin(b)^2 + (c^4)/10 * sin(a)
  return (Y)
}

Ishapply <- function (my.data) {
  return(mapply(Ishfun, my.data[,1], my.data[,2], my.data[,3]))
}

t1 <- sobol(model=Ishapply,s1,s2,order=3,nboot=100)
print(t1)
plot(t1)

## Samples Recieved ##

samples <- function(n){
  x <- c(runif(2*n,min=-pi,max=pi))
  y <- c(runif(2*n,min=-pi,max=pi))
  z <- c(runif(2*n,min=-pi,max=pi))
  X = cbind(x,y,z)
  s1 = X[1:n,]
  s2 = X[(n+1):(2*n),]
  a = sobol(model=Ishapply,s1,s2,order=3,nboot=100)
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

## Plotting ## 

par(mar=c(4,4,2,2))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
plot(N,S1)
abline(h = 0.3139051911)
plot(N,S2)
abline(h = 0.4424111448)
par(mar=c(2,14,2,14))
plot(N,S3)
abline(h = 0)


#####


aa = samples(200)
ab = samples(200)
ac = samples(200)
ad = samples(200)
ae = samples(200)
af = samples(200)
ag = samples(200)
ah = samples(200)
ai = samples(200)
aj = samples(200)
ak = samples(200)
al = samples(200)
am = samples(200)
an = samples(200)
ao = samples(200)
ap = samples(200)
aq = samples(200)
ar = samples(200)
as = samples(200)
at = samples(200)
au = samples(200)
av = samples(200)
aw = samples(200)
ax = samples(200)
ay = samples(200)
az = samples(200)
aaa = samples(200)
aab = samples(200)
aac = samples(200)
aad = samples(200)
aae = samples(200)
aaf = samples(200)
aag = samples(200)
aah = samples(200)
aai = samples(200)
aaj = samples(200)
aak = samples(200)
aal = samples(200)
aam = samples(200)
aan = samples(200)
aao = samples(200)
aap = samples(200)
aaq = samples(200)
aar = samples(200)
aas = samples(200)
aat = samples(200)
aau = samples(200)
aav = samples(200)
aaw = samples(200)
aax = samples(200)

bf = cbind(aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,aaa,aab,aac,aad,aae,aaf,aag,aah,
       aai,aaj,aak,aal,aam,aan,aao,aap,aaq,aar,aas,aat,aau,aav,aaw,aax)
mubb200 = mapply(mean,bf[,1],bf[,2],bf[,3],bf[,4],bf[,5],bf[,6],bf[,7])

plot(n,S1,ylim=c(-0.75,1))
points(10,mubb[1],col=2,bg=2,pch=21)
points(50,mubb50[1],col=2,bg=2,pch=21)
points(100,mubb100[1],col=2,bg=2,pch=21)
points(200,mubb200[1],col=2,bg=2,pch=21)
points(500,mubb500[1],col=2,bg=2,pch=21)
abline(h = 0.3139051911,col="blue")

n = c(rep(10,50),rep(50,50),rep(100,50),rep(200,50),rep(500,50))
S1 = c(bb[1,],bc[1,],bd[1,],bf[1,],be[1,])
