require(ggplot2)
require(lme4)
library(dplyr)
library(gridExtra)

light<-read.csv("data/Episodic_Exposure_Light_Test.csv")
lightc<-light[light$Treatment=="Control",]
lightc51<-lightc%>%
  filter(Days_Post_Fertilization == 51)
lightc55<-lightc%>%
  filter(Days_Post_Fertilization == 55)
lightcglm<-glm(lightc$N_in_Light~lightc$Minutes_Post_Switch)
lightc51glm<-glm(lightc51$N_in_Light~lightc51$Minutes_Post_Switch)
lightc55glm<-glm(lightc55$N_in_Light~lightc55$Minutes_Post_Switch)

lightq<-light[light$Treatment=="6PPDQ",]
lightq51<-lightq%>%
  filter(Days_Post_Fertilization == 51)
lightq55<-lightq%>%
  filter(Days_Post_Fertilization == 55)
lightqglm<-glm(lightq$N_in_Light~lightq$Minutes_Post_Switch)
lightq51glm<-glm(lightq51$N_in_Light~lightq51$Minutes_Post_Switch)
lightq55glm<-glm(lightq55$N_in_Light~lightq55$Minutes_Post_Switch)

plot51<-ggplot(lightq51, aes(x = Minutes_Post_Switch, y = N_in_Light)) +
  geom_smooth(data=lightq51, linewidth=1.1, method=lm, formula = y~x, 
              color="#F8766D", fill="#F8766D", alpha=0.2)+
  geom_point(color="#F8766D", size=3, shape=2, stroke=1.4,alpha=1)+
  geom_smooth(data=lightc51, linewidth=1.1, method=lm, formula = y~x, 
              color="#00bfc4", fill="#00bfc4", alpha=0.2)+
  geom_point(data=lightc51, color="#00bfc4", size=3, shape=1, stroke=1.4,
             alpha=1)+
  labs(x="Minutes", y="51 Days_Post_Fertilization alevin in the light")+
  coord_cartesian(ylim=c(0,20),xlim=c(1,10),expand=TRUE)+
  scale_y_continuous(breaks=seq(0,20,by=10))+
  scale_x_continuous(breaks=seq(0,10,by=5))+
  theme(legend.position=c(0,0))+
  annotate("text",x=1,y=19,label="A)",size=6,hjust=0, size=8)+
  theme_classic(20)

plot55<-ggplot(lightq55, aes(x = Minutes_Post_Switch, y = N_in_Light)) +
  geom_smooth(data=lightq55, linewidth=1.1, method=lm, formula = y~x, 
              color="#F8766D", fill="#F8766D", alpha=0.2)+
  geom_point(color="#F8766D", size=3, shape=2, stroke=1.4,alpha=1)+
  geom_smooth(data=lightc55, linewidth=1.1, method=lm, formula = y~x, 
              color="#00bfc4", fill="#00bfc4", alpha=0.2)+
  geom_point(data=lightc55, color="#00bfc4", size=3, shape=1, stroke=1.4,
             alpha=1)+
  labs(x="Minutes", y="55 Days_Post_Fertilization alevin in the light")+
  coord_cartesian(ylim=c(0,20),xlim=c(1,10),expand=TRUE)+
  scale_y_continuous(breaks=seq(0,20,by=10))+
  scale_x_continuous(breaks=seq(0,10,by=5))+
  theme(legend.position=c(0,0))+
  annotate("text",x=1,y=19,label="B)",size=6,hjust=0, size=8)+
  theme_classic(20)

grid.arrange(plot51,plot55,ncol=2)

#statistics
lightc51<-lightc[lightc$Days_Post_Fertilization==51,]
lightq51<-lightq[lightq$Days_Post_Fertilization==51,]
t.test(lightc51$N_in_Light,lightq51$N_in_Light,paired=TRUE)

lightc55<-lightc[lightc$Days_Post_Fertilization==55,]
lightq55<-lightq[lightq$Days_Post_Fertilization==55,]
t.test(lightc55$N_in_Light,lightq55$N_in_Light,paired=TRUE)
sd(lightc$N_in_Light)
sd(lightq$N_in_Light)
