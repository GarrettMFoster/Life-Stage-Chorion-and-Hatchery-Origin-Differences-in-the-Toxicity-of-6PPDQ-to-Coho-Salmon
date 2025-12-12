require(drc)
require(ggplot2)
require(reshape2)
require(tidyverse)
require(car)
require(pwr)

#Voights alevin
acute_data<-read.csv("data/Acute_Exposure.csv")
valevin<-acute_data[acute_data$Group=="Voights Alevin",]
valevindrc<-drm(valevin$Mortality_Rate~valevin$Measured_6PPDQ_ng_L, fct=LL.2(),type="binomial")
plot(valevindrc, confidence.level = 0.95)
summary(valevindrc)
confint(valevindrc)

#Bingham alevin
balevin<-acute_data[acute_data$Group=="Bingham Alevin",]
balevindrc<-drm(balevin$Mortality_Rate~balevin$Measured_6PPDQ_ng_L, fct=LL.2(),type="binomial")
plot(balevindrc)
summary(balevindrc)


#Voight and Bingham combined model
bvalevin<-rbind(balevin,valevin)
bvalevindrc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L, data=bvalevin,fct=LL.2(),type="binomial")
bvsepdrc<-drm(Mortality_Rate~Measured_6PPDQ_ng_L,Group, data=bvalevin,fct=LL.2(),type="binomial",
              pmodels = list(~factor(Group)-1, ~factor(Group)-1))
plot(bvalevindrc)
summary(bvalevindrc)
confint(bvalevindrc)

plot(bvsepdrc)
summary(bvsepdrc)
confint(bvsepdrc)

#Likelihood ratio
anova(bvsepdrc,bvalevindrc,test=)