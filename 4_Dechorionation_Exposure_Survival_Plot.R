require("survival")	
require("survminer")	

dechor<- read.csv("data/Dechorionation_Survival.csv")

#Create survival object	
dechor_surv_object <- Surv(time=dechor$Days_Post_Fertilization, 
                           event = dechor$Outcome)	
dechor_fit <- survfit(dechor_surv_object ~Treatment, data=dechor)	
dechor_fit
summary(dechor_fit)
dechor_survplot <- ggsurvplot(dechor_fit, data = dechor, conf.int=TRUE, 
                              legend=c(0.2,0.2),legend.title="Treatment",
                              legend.labs=c("6PPDQ Dechorionated*",
                                            "6PPDQ Intact",
                                            "Control Dechorionated",
                                            "Control Intact"),
                              xlab = "Days post fertilization",
                              ggtheme=theme_classic(base_size=20))$plot
dechor_survplot

#create ggplot
dechor_ep<-data.frame(28,1)

dechor_survplot+
  geom_col(aes(x=28,y=1),data=dechor_ep,fill="#D55E00",width=1,
           position=position_nudge(-.5),alpha=0.2)+
  scale_y_continuous(breaks=seq(0,1,by=0.5))+
  scale_x_continuous(breaks=seq(27,46,by=1),labels=c("","","",30,"","","","","",
                                                     "","","","",40,"","","","",
                                                     "",""))+
  geom_vline(xintercept=c(43), linetype="dotted")+
  geom_label(aes(x=43,y=0.5,label="First hatch",angle=90),
             size=7,fill="white")+
  annotate("text",x=27.5,y=0.5,label="Exposure",angle=90,size=7)+
  annotate("text",x=44.2,y=0.33,label="*",size=7)+
  theme(legend.background = element_rect(fill = "transparent", color = NA))+
  coord_cartesian(ylim=c(0,1),xlim=c(27,45),expand=FALSE)

#Log-rank test for differences between groups
survdiff(Surv(Days_Post_Fertilization, Outcome) ~ Treatment, data=dechor)

#Log-rank HB corrected p values for each group comparison
pairwise_survdiff(Surv(Days_Post_Fertilization, Outcome) ~ Treatment, data=dechor)
