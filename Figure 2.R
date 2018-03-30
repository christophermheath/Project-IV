alpha <- function(s){
  num = pi^(s/2)*0.5^s
  denom = gamma(s/2 + 1)
  y = num/denom
  return(y)
}

c = c(1,2,3,4,5,6,7,8,9,10)
d = alpha(c)

plot(100*d/1,xlab="Number of Model Paramters",ylab="Percentage of Space Filled",type="b",col=4)
axis(side=1, at=c(0:15))
