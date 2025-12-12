##Load packages
require(drc)
require(ggplot2)
require(reshape2)
require(tidyverse)

##Load data frames
acute_data<-read.csv("data/Acute_Exposure.csv")
adult<-acute_data[acute_data$Group=="Clark Adult",]

#adult model
adult_drc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, data=adult,fct=LL.2(),
               type="binomial")
plot(adult_drc, confidence.level = 0.95)
summary(adult_drc)
confint(adult_drc)

newdata_adult_drc <- expand.grid(conca=seq(0, 400, length=10000))
pm_adult_drc <- predict(adult_drc, newdata=newdata_adult_drc, 
                        interval="confidence", ymin=0,ymax=1)
newdata_adult_drc$p <- pm_adult_drc[,1]
newdata_adult_drc$pmin <- pm_adult_drc[,2]
newdata_adult_drc$pmax <- pm_adult_drc[,3]

#parr model
co_exposed_parr<-acute_data[acute_data$Group=="Coexposed Voights Parr",]
co_exposed_parr_drc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L,
                         data=co_exposed_parr, fct=LL.2(),type="binomial")
plot(co_exposed_parr_drc, confidence.level = 0.95)
summary(co_exposed_parr_drc)
confint(co_exposed_parr_drc)

newdata_co_exposed_parr_drc <- expand.grid(concp=seq(0, 400, length=10000))
pmco_exposed_parr_drc <- predict(co_exposed_parr_drc, newdata=newdata_co_exposed_parr_drc, 
                       interval="confidence")
newdata_co_exposed_parr_drc$p <- pmco_exposed_parr_drc[,1] 
newdata_co_exposed_parr_drc$pmin <- pmco_exposed_parr_drc[,2]
newdata_co_exposed_parr_drc$pmax <- pmco_exposed_parr_drc[,3]

#plot separate drcs on the same graph
adult_plot<-ggplot(adult, aes(x = Measured_6PPDQ_ng_L, y = Mortality_Rate)) +
  geom_ribbon(fill=6, color=6,data=newdata_adult_drc,
              aes(x=conca, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(color=6, size=1.3,data=newdata_adult_drc, aes(x=conca, y=p)) +
  geom_point(color=6, size=3, shape=2, stroke=2) +
  geom_ribbon(fill=8,color=8,data=newdata_co_exposed_parr_drc, 
              aes(x=concp, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(color=8, size=1.3,data=newdata_co_exposed_parr_drc, 
            aes(x=concp, y=p)) +
  geom_point(data=co_exposed_parr, 
             aes(x = Measured_6PPDQ_ng_L, y = Mortality_Rate),color=8, size=3, 
             shape=1, stroke=2) +
  labs(x="[6PPDQ] (ng/L)", y="Mortality probability")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.5))+
  coord_cartesian(ylim=c(0,1),xlim=c(0,350),expand=FALSE)+
  theme_classic(20)+
  geom_point(x=250,y=0.25,color=8, size=3, shape=1, stroke=1) +
  geom_point(x=250,y=0.15,color=6, size=3, shape=2, stroke=1) +
  annotate("text",x=260,y=0.25,label="Parr",color=8,hjust=0, size=7,stroke=1)+
  annotate("text",x=260,y=0.15,label="Adult",color=6,hjust=0,size=7,stroke=1)+
  annotate("text",x=14.6,y=0.95,label="A)",size=6,hjust=0, size=8)
adult_plot

#adult and co-exposed parr combined model
adult_co_exposed_parr<-rbind(adult,co_exposed_parr)
adult_co_exposed_parr_drc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, 
                               data=adult_co_exposed_parr,
                               fct=LL.2(),type="binomial")
adult_co_exposed_parr_separate_drc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, 
                                        Group,data=adult_co_exposed_parr,
                                        fct=LL.2(),type="binomial")
plot(adult_co_exposed_parr_drc)
plot(adult_co_exposed_parr_separate_drc)
summary(adult_co_exposed_parr_separate_drc)

modelFit(adult_co_exposed_parr_drc)
modelFit(adult_drc)

anova(adult_co_exposed_parr_drc,adult_co_exposed_parr_separate_drc)
