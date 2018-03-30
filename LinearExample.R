# Own Example

library(durham)
data(cofe)
library(sensitivity)

cofe
?cofe

G = c(cofe$Giving)
E = c(cofe$Employment)
R = c(cofe$Roll)
A = c(cofe$Attendance)

giving.lm <- lm(Giving~Employment+Roll+Attendance, data=cofe)
giving.lm

# Assumption of a well-fitting model
# Our regression line is G = -60.646 + 1.441E - 3.381R - 1.607A

Givfun <- function (E,R,A) {
  G = -60.646 + 1.441*E - 3.381*R - 1.607*A
  return (G)
}

Givapply <- function (my.data) {
  return(mapply(Givfun, my.data[,1], my.data[,2], my.data[,3]))
}

GM <- morris(model = Givapply, factors = 3, r = 6,
             design = list(type = "oat", levels = 10, grid.jump = 5))
print(GM)
plot(GM)


x <- c(rnorm(2000000,2.7,0.76))
y <- c(rnorm(2000000,4.12,1.6694))
z <- c(rnorm(2000000,87,2.8))

X = cbind(x,y,z)

s1 = X[1:1000000,]
s2 = X[1000001:2000000,]

t1 <- sobol(model=Givapply,s1,s2,order = 3)
print(t1)
plot(t1)


