Gi.om11 <- function (a) {
  b = 0.5 + (1/pi)*asin(sin(11*a))
  return(b)
}

plot(Gi.om11,xlim=c(-pi,pi),xlab="s",ylab="G")


# Builds our sample

Gi.om3 <- function (a) {
  b = 0.5 + (1/pi)*asin(sin(3*a))
  return(b)
}

Gi.om7 <- function (a) {
  b = 0.5 + (1/pi)*asin(sin(7*a))
  return(b)
}
s1 <- runif(2000,-pi,pi)
t1 = Gi.om3(s1)
s2 <- runif(2000,-pi,pi)
t2 = Gi.om7(s2)
v = cbind(t1,t2)
plot(v,xlim=c(0,1),ylim=c(0,1),xlab="x1",ylab="x2")

par(mfrow=c(1,1))

