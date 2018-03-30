install.packages("sensitivity")
library(sensitivity)
install.packages("ggplot2")
library(ggplot2)


a = c("Sx","Sy","Sz","Sxy","Sxz","Syz","Sxyz")
b = factor(a, as.character(a))
Sexact=c(0.3139,0.4424,0,0,0.2437,0,0)
t = data.frame(SobolIndices=as.factor(b),Sexact)
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
bp<- ggplot(t, aes(x="", y=Sexact, fill=SobolIndices))+
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
