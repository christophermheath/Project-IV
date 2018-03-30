install.packages("sensitivity")
library(sensitivity)
install.packages("ggplot2")
library(ggplot2)

x1 <- c(runif(100000,min=-pi,max=pi))
x2 <- c(runif(100000,min=-pi,max=pi))
x3 <- c(runif(100000,min=-pi,max=pi))

X = cbind(x1,x2,x3)

s1 = X[1:50000,]
s2 = X[50001:100000,]

Ishfun <- function (a,b,c) {
  Y = sin(a) + 7*sin(b)^2 + (c^4)/10 * sin(a)
  return (Y)
}

Ishapply <- function (my.data) {
  return(mapply(Ishfun, my.data[,1], my.data[,2], my.data[,3]))
}

t1 <- sobol(model=Ishapply,s1,s2,order=3)
print(t1)
plot(t1)



#Pie Chart
percentage <- c(50, 20, 10, 10, 2, 2, 2)


a = c("Sx","Sy","Sz","Sxy","Sxz","Syz","Sxyz")
b = factor(a, as.character(a))
S=c(0.31487563,0.44480353,0.017,0.011,0.20664722,0.01,0.01)
Sexact=c(0.3139,0.4424,0,0,0.2437,0,0)
t = data.frame(SobolIndices=as.factor(b),S)
plot(t)
theme_set(theme_classic())
library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
bp<- ggplot(t, aes(x="", y=S, fill=SobolIndices))+
  geom_bar(width = 0.75, stat = "identity") + theme(legend.text=element_text(size=12)) +
  coord_flip() + scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))+
  labs(x="",y="")

bp


pie <- bp + coord_polar("y", start=0,direction=1)
pie +  theme(axis.text.x=element_blank()) + blank_theme +theme(axis.text.x=element_blank()) + theme(legend.text=element_text(size=12))
  
bp<- ggplot(t, aes(x="", y=Sexact, fill=SobolIndices))+
  geom_bar(width = 0.75, stat = "identity") + theme(legend.text=element_text(size=12)) +
  coord_flip() + scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))+
  labs(x="",y="")
bp
