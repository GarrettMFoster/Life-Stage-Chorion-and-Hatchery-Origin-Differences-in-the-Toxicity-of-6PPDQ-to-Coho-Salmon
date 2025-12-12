require(drc)
require(ggplot2)
require(reshape2)
require(tidyverse)
require(car)

#voights alevin
acute_data<-read.csv("data/Acute_Exposure.csv")
valevin<-acute_data[acute_data$Group=="Voights Alevin",]
valevindrc<-drm(valevin$Mortality_Rate~valevin$Measured_6PPDQ_ng_L, fct=LL.2(),type="binomial")
plot(valevindrc, confidence.level = 0.95)
summary(valevindrc)
confint(valevindrc)
predict(valevindrc, data.frame(dose=254), se.fit=TRUE)

#voights fry
fry<-acute_data[acute_data$Group=="Voights Fry",]
frydrc<-drm(fry$Mortality_Rate~fry$Measured_6PPDQ_ng_L, fct=LL.2(),type="binomial")
plot(frydrc, confidence.level = 0.95)
summary(frydrc)
confint(frydrc)

#voights parr
parr<-acute_data[acute_data$Group=="Voights Parr",]
parrdrc<-drm(parr$Mortality_Rate~parr$Measured_6PPDQ_ng_L, fct=LL.2(),type="binomial")
plot(parrdrc, confidence.level = 0.95)
summary(parrdrc)
confint(parrdrc)

#combined juvenile drc
alevinfryparr<-rbind(valevin,fry,parr)
alevinfryparrdrc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, data=alevinfryparr,fct=LL.2(),type="binomial")
alevinfryparrsepdrc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, Group, data=alevinfryparr,fct=LL.2(),type="binomial")
plot(alevinfryparrdrc)
plot(alevinfryparrsepdrc)
summary(alevinfryparrdrc)
summary(alevinfryparrsepdrc)
confint(alevinfryparrsepdrc)

anova(alevinfryparrdrc,alevinfryparrsepdrc)

#plot of alevin, fry, and parr separate models
newdata_valevindrc <- expand.grid(conca=seq(0, 1650, length=10000))
pmvalevindrc <- predict(valevindrc, newdata=newdata_valevindrc, interval="confidence")
newdata_valevindrc$p <- pmvalevindrc[,1]
newdata_valevindrc$pmin <- pmvalevindrc[,2]
newdata_valevindrc$pmax <- pmvalevindrc[,3]

newdata_frydrc <- expand.grid(conca=seq(0, 1650, length=10000))
pmfrydrc <- predict(frydrc, newdata=newdata_frydrc, interval="confidence")
newdata_frydrc$p <- pmfrydrc[,1]
newdata_frydrc$pmin <- pmfrydrc[,2]
newdata_frydrc$pmax <- pmfrydrc[,3]

newdata_parrdrc <- expand.grid(conca=seq(0, 1650, length=10000))
pmparrdrc <- predict(parrdrc, newdata=newdata_parrdrc, interval="confidence")
newdata_parrdrc$p <- pmparrdrc[,1]
newdata_parrdrc$pmin <- pmparrdrc[,2]
newdata_parrdrc$pmax <- pmparrdrc[,3]

juvplot<-ggplot(fry, aes(x = Measured_6PPDQ_ng_L, y = Mortality_Rate)) +
  geom_ribbon(fill=4,color=4,data=newdata_frydrc, aes(x=conca, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(color=4, size=1.3,data=newdata_frydrc, aes(x=conca, y=p)) +
  geom_jitter(color=4, size=3, shape=3, stroke=2,width=10) +
  geom_ribbon(fill=3,color=3,data=newdata_valevindrc, aes(x=conca, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(color=3, size=1.3,data=newdata_valevindrc, aes(x=conca, y=p)) +
  geom_jitter(data=valevin,color=3, size=3, shape=2, stroke=2,width=10) +
  geom_ribbon(fill=8,color=8,data=newdata_parrdrc, aes(x=conca, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(color=8, size=1.3,data=newdata_parrdrc, aes(x=conca, y=p)) +
  geom_point(data=parr,color=8, size=3, shape=1, stroke=2) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5))+
  scale_x_continuous(breaks = seq(0, 1500, by = 500))+
  coord_cartesian(ylim=c(0,1), expand = TRUE)+
  labs(x="[6PPDQ] (ng/L)", y="Mortality probability")+
  theme_classic(20)+
  annotate("text",x=1400,y=.3,label="Parr",color=8,hjust=0, size=7)+
  geom_point(x=1350,y=.3,color=8, size=3, shape=1, stroke=1) +
  annotate("text",x=1400,y=.2,label="Fry",color=4,hjust=0,size=7)+
  geom_point(x=1350,y=.2,color=4, size=3, shape=3, stroke=1) +
  annotate("text",x=1400,y=.1,label="Alevin",color=3,hjust=0,size=7)+
  geom_point(x=1350,y=.1,color=3, size=3, shape=2, stroke=1) +
  coord_cartesian(ylim=c(0,1),expand=FALSE)
juvplot
