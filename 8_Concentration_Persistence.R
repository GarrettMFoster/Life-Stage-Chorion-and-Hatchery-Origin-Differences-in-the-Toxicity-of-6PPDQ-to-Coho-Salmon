require(drc)
require(ggplot2)
require(reshape2)
require(tidyverse)
require(car)
require(gridExtra)

loss<-read.csv("data/Concentration_Persistence.csv")
valoss<-loss[loss$Group == "Voights_Alevin",]
fryloss<-loss[loss$Group == "Voights_Fry",]
parrloss<-loss[loss$Group == "Voights_Parr",]
adultloss<-loss[loss$Group == "Clark_Adult",]
juvenileloss<-rbind(valoss,fryloss,parrloss)

#juvenile exposures Q persistence
juvpersistence<-ggplot(juvenileloss, aes(x = Exposure_Hour, y = C_C0)) +
  geom_smooth(data=juvenileloss, linewidth=1.1, method="gam", 
              formula = y~x*I(x^(1/2)), color="#D55E00", fill="#D55E00", 
              alpha=0.2,level=0.95)+
  geom_point(color="#D55E00", size=1, shape=19, stroke=1.4,alpha=1) +
  xlab("Hours")+
  ylab(bquote("[6PPDQ]/[6PPDQ]"[50]))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.5))+
  scale_x_continuous(breaks = seq(0, 24, by = 12))+
  coord_cartesian(ylim=c(0,1.1))+
  theme_classic(20)
final<-(juvenileloss[juvenileloss$Exposure_Hour == 24,])
mean(final$C_C0)
sd(final$C_C0)

grid.arrange(juvpersistence,juvplot,ncol=2)
loss<-geom_smooth(data=juvenileloss, linewidth=1.1, method="gam", 
                       formula = y~x*I(x^(1/2)), color="#D55E00", fill="#D55E00", 
                       alpha=0.2,level=0.95)
auc(loss,0,24)

#adult exposures Q persistence
adultpersistence<-ggplot(adultloss, aes(x = Exposure_Hour, y = C_C0)) +
  geom_smooth(data=adultloss, linewidth=1.1, method="gam", 
              formula = y~x*I(x^(1/2)), color="#D55E00", fill="#D55E00", 
              alpha=0.2)+
  geom_point(color="#D55E00", size=1, shape=19, stroke=1.4,alpha=1) +
  xlab("Hours")+
  ylab(bquote("[6PPDQ]/[6PPDQ] "[0]))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.5))+
  scale_x_continuous(breaks = seq(0, 24, by = 12))+
  coord_cartesian(ylim=c(0,1), expand=FALSE)+
  annotate("text",x=1,y=0.95,label="B)",size=6,hjust=0, size=8)+
  theme_classic(20)

grid.arrange(adult_plot,adultpersistence,ncol=2)
