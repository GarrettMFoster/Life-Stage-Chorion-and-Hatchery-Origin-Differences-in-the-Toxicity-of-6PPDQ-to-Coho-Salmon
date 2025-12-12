require(ggplot2)
require(ggsignif)
require(gridExtra)

morphometrics<-read.csv("data/Episodic_Exposure_Morphometric.csv")
head(morphometrics)

#create a function for making morphometric dataframes
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#eye morphometrics
eyedf<-morphometrics[,c("Eye_Area_sq_mm","Days_Post_Fertilization","Treatment")]

eyeplot<-ggplot(eyedf, aes(x=Days_Post_Fertilization, y=Eye_Area_sq_mm, 
                             fill=Treatment)) + 
  geom_boxplot(aes(x=Days_Post_Fertilization, y=Eye_Area_sq_mm))+
  xlab(NULL)+
  ylab(bquote('Eye area (m' *m^'2'*')'))+
  coord_cartesian(ylim = c(0, 2.5), expand = TRUE) +  
  scale_y_continuous(breaks = seq(0, 2, by = 1))+
  theme_classic(20)+
  theme(legend.position="none")+
  geom_signif(y_position = c(1.5, 2.1), xmin=c(.75, 1.75), xmax = c(1.25, 2.25),
              annotation = c("p < 0.001", "p < 0.001"), tip_length = 0.05, textsize = 6, 
              size = .5)
eyeplot


#length morphometrics
lengthdf<-morphometrics[,c("Length_mm","Days_Post_Fertilization","Treatment")]

lengthplot<-ggplot(lengthdf, aes(x=Days_Post_Fertilization, y= Length_mm, 
                             fill=Treatment)) + 
  geom_boxplot(aes(x=Days_Post_Fertilization, y= Length_mm))+
  xlab(NULL)+
  ylab("Length (mm)")+
  coord_cartesian(ylim = c(0, 25), expand = TRUE) +  
  scale_y_continuous(breaks = seq(0, 20, by = 10))+
  theme_classic(20)+
  theme(legend.position="none")+
  geom_signif(y_position = c(17, 18.5), xmin=c(.75, 1.75), xmax = c(1.25, 2.25),
              annotation = c("p > 0.05", "p < 0.001"), tip_length = 0.05, textsize = 6, 
              size = .5)
lengthplot

#yolk morphometrics
yolkdf<-morphometrics[,c("Yolk_Area_sq_mm","Days_Post_Fertilization","Treatment")]

yolkplot<-ggplot(yolkdf, aes(x=Days_Post_Fertilization, y=Yolk_Area_sq_mm, 
                                    fill=Treatment)) + 
  geom_boxplot(aes(x=Days_Post_Fertilization, y=Yolk_Area_sq_mm))+
  xlab(NULL)+
  ylab(bquote('Yolk area (m' *m^'2'*')'))+
  coord_cartesian(ylim = c(0, 75), expand = TRUE) +  
  scale_y_continuous(breaks = seq(0, 60, by = 30))+
  theme_classic(20)+
  theme(legend.position=c(.35,.15),
        legend.title=element_blank())+
  geom_signif(y_position = c(73, 73), xmin=c(.75, 1.75), xmax = c(1.25, 2.25),
                annotation = c("p > 0.05", "p > 0.05"), tip_length = 0.05, textsize = 6, 
              size = .5)
yolkplot

#eyes normalized to length (relative eye area)
morphometrics$relEye_Area_sq_mm<-morphometrics$Eye_Area_sq_mm/morphometrics$Length_mm
releyedf<-morphometrics[,c("relEye_Area_sq_mm","Days_Post_Fertilization","Treatment")]
releyeplot<-ggplot(releyedf, aes(x=Days_Post_Fertilization, y=relEye_Area_sq_mm, 
                           fill=Treatment)) + 
  geom_boxplot(aes(x=Days_Post_Fertilization, y=relEye_Area_sq_mm))+
  xlab(NULL)+
  ylab(bquote('Eye area per length (m' *m^'2'*'/mm)'))+
  coord_cartesian(ylim = c(0, 0.25), expand = TRUE) +  
  scale_y_continuous(breaks = seq(0, .2, by = .1))+
  theme_classic(20)+
  theme(legend.position="none")+
  geom_signif(y_position = c(.11, .13), xmin=c(.75, 1.75), xmax = c(1.25, 2.25),
              annotation = c("p < 0.01", "p < 0.001"), tip_length = 0.05, textsize = 6, 
              size = .5)
releyeplot

#t tests
q<-morphometrics[morphometrics$Treatment == "6PPDQ",]
q34<-q[q$Days_Post_Fertilization == "34",]
q41<-q[q$Days_Post_Fertilization == "41",]
c<-morphometrics[morphometrics$Treatment == "Control",]
c34<-c[c$Days_Post_Fertilization == "34",]
c41<-c[c$Days_Post_Fertilization == "41",]
t.test(x=c34$Eye_Area_sq_mm, y=q34$Eye_Area_sq_mm)
t.test(x=c41$Eye_Area_sq_mm, y=q41$Eye_Area_sq_mm)
t.test(x=c34$Length_mm,y=q34$Length_mm)
t.test(x=c41$Length_mm,y=q41$Length_mm)
t.test(x=c34$Yolk_Area_sq_mm,y=q34$Yolk_Area_sq_mm)
t.test(x=c41$Yolk_Area_sq_mm,y=q41$Yolk_Area_sq_mm)
t.test(x=c34$relEye_Area_sq_mm, y=q34$relEye_Area_sq_mm)
t.test(x=c41$relEye_Area_sq_mm, y=q41$relEye_Area_sq_mm)

#display graphic
grid.arrange(yolkplot,eyeplot,lengthplot,ncol=3)

