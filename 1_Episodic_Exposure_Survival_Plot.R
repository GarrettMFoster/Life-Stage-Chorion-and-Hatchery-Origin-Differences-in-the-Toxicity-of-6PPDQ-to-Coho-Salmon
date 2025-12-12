require(survival)
require(survminer)
require(dplyr)
require(ggplot2)
#import the dataset
q<-read.csv("data/Episodic_Exposure_Survival.csv")

#create a surv object
surv_object <- Surv(time = q$Days_Post_Fertilization, event = q$Outcome)
surv_object
fit1 <- survfit(surv_object ~ Treatment, data = q)
summary(fit1)

#ggplot survplot
survplot<-ggsurvplot(fit1, data = q, pval = FALSE, ylab="Survival probability",
                     xlab="Days post fertilization",conf.int=TRUE, 
                     legend=c(0.1,0.2),legend.title="Treatment",
                     legend.labs=c("6PPDQ","Control"),
                     ggtheme=theme_classic(base_size=20))
survplot$plot$layers[[4]]$aes_params$colour <- "black"
survplot

survdiff(Surv(Days_Post_Fertilization,Outcome)~Treatment,data=q)
