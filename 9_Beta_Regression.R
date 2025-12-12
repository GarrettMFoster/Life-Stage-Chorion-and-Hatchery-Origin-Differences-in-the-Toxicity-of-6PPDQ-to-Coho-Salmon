require(GGally)
require(BioRssay)
require(rgl)
require(truncnorm)
require(betareg)
require(statmod)
require(lmtest)
require(cowplot)

lcdf<-data.frame(Days_Post_Fertilization=c(54,54,74,74,342,342,300,300,1095,1095),
                 Measured_6PPDQ_ng_L=c("ED"(valevindrc,50),"ED"(frydrc,50),"ED"(parrdrc,50),
                        "ED"(co_exposed_parr_drc,50), "ED"(adult_drc,50)))
lcs<-lcdf[c(1,3,5,7,9),]
sds<-lcdf[c(2,4,6,8,10),]
colnames(sds)<-c("Days_Post_Fertilization","sd")
lcsds<-merge(lcs,sds, by="Days_Post_Fertilization")
data<-read.csv("data/Acute_Exposure.csv")


model<-betareg(Mortality_Rate~I(log(Measured_6PPDQ_ng_L))*I(log(Days_Post_Fertilization)), data = data)
summary(model)
lrtest(model)
plot(model)
shapiro.test(data$Mortality_Rate)

matrix <- matrix(rnorm(111), nrow=111, ncol=111)
rownames(matrix) <- seq(from=0.1, to=1100.1, by=10)
colnames(matrix) <- seq(from=0.1, to=1100.1,by=10)
matrix_melted <- melt(matrix)

colnames(matrix_melted) <- c("Days_Post_Fertilization", "Measured_6PPDQ_ng_L", "Mortality_Rate")
matrix_melted$Mortality_Rate<-predict(model, matrix_melted)
lcsds$Mortality_Rate<-predict(model,lcsds)

ggplot(data=matrix_melted, aes(x=Days_Post_Fertilization, y=Measured_6PPDQ_ng_L, fill=(Mortality_Rate))) +
  geom_raster(data=matrix_melted,interpolate=TRUE) +
  scale_fill_binned(name="P(Mortality_Rate)",low="#333333",high="white",n.breaks=6,
                    right=FALSE,transform="sqrt") +
  geom_jitter(data=data,shape=21,width=10,size=4,stroke=1.1,color="black")+
  geom_point(data=lcsds,color="black",size=4)+
  geom_errorbar(data=lcsds,aes(ymin=Measured_6PPDQ_ng_L-sd,ymax=Measured_6PPDQ_ng_L+sd),color="black")+
  labs(x="Days post fertilization", y="[6PPDQ] (ng/L)") +
  theme_classic(20)+
  scale_y_continuous(breaks = seq(0, 1000, by = 500))+
  scale_x_continuous(breaks = c(1,10,100,1000),labels=c("",10,100,1000))+
  guides(x=guide_axis_logticks())+
  coord_cartesian(ylim=c(0,1100),expand=FALSE)+
  annotate("text",x=55,y=785,label="Alevin",color="black",hjust=-0.1,
           fontface="bold",size=5)+
  annotate("text",x=130,y=240,label="Fry",color="black",
           fontface="bold",size=5)+
  annotate("text",x=360,y=220,label="Parr",color="black",
           fontface="bold",size=5)+
  annotate("text",x=1075,y=141,label="Adult",color="black",hjust=1.1,
           fontface="bold",size=5)+
  theme(legend.position="right", legend.direction="vertical",
        legend.margin=margin(grid::unit(0, "cm")),
        legend.key.height=grid::unit(2,"cm"),
        legend.key.width=grid::unit(0.2,"cm"))
