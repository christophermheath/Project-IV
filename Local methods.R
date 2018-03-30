library(durham)
data(engine)

CO = c(engine$CO)
HC = c(engine$HC)

z <- function(CO,HC){
  NOX = 1.54354 - 0.08877*CO + 0.89128*HC
  return(NOX)
}

zapply <- function(m){
  Y = mapply(z,m[,1],m[,2])
  return(Y)
}

library(sensitivity)

thanks <- morris(zapply, factors=2, r=8, design=list(type="oat",levels=20,grid.jump=10))
plot(thanks)
