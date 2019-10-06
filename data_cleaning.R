rm(list = ls())
library(ggplot2)

#Read the data

data<- read.csv("BodyFat.csv")[,-1]
col_name<- colnames(data)
n_row_0<- nrow(data)
n_col_0<- ncol(data)

#Outliers boxplot

# reshape the data into one-dim
data_all_0<- as.matrix(data)
dim(data_all_0)<- c(n_row_0*n_col_0,1)
facs_0<- gl(n_col_0,n_row_0,label=col_name)
data_fac_0<- data.frame(data_all_0,facs_0)

box_0<- ggplot(data_fac_0,aes(x=factor(data_fac_0$facs_0),y=data_fac_0$data_all_0,fill = factor(data_fac_0$facs_0)))+
  geom_boxplot(outlier.color = "red",outlier.shape=21,outlier.size = 1.5)+scale_fill_brewer(palette = "Pastel2")+
  labs(title = "boxplot of original data",xlab="factor",ylab="data value")+
  stat_summary(fun.y = "mean",geom="point",shape=23,size=3,fill="white")+guides(fill=FALSE)
ggsave("boxplot_original.png",box_0)

#============================================================

#Data Cleaning

#Bodyfat & BMI from calculation
bodyfat_siri<- (495/(data$DENSITY))-450
BMI_cal<- (703*data$WEIGHT)/((data$HEIGHT)^2)

compare_bodyfat<- data.frame(RealBodyfat=data$BODYFAT,CalBodyfat=bodyfat_siri)
compare_BMI<- data.frame(RealBMI=data$ADIPOSITY,CalBMI=BMI_cal)
#=============================================================
#compare BMI
#bmi=703*weight/height^2


weight_q<- c(quantile(data$WEIGHT,0.01),quantile(data$WEIGHT,0.99))
#(125.505,245.720)
height_q<- c(quantile(data$HEIGHT,0.01),quantile(data$HEIGHT,0.99))
#(64.3825 ,76.0000 )
density_q<- c(quantile(data$DENSITY,0.01),quantile(data$DENSITY,0.99))
#(1.016040 ,1.095393)
bodyfat_q<- c(quantile(data$BODYFAT,0.01),quantile(data$BODYFAT,0.99))
#(4.355 ,35.582)

bmi_plot_1<- ggplot(compare_BMI,aes(x=RealBMI,y=CalBMI))+geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  annotate("text",x=29.9,y=150.6,label="42",vjust=-1)+labs(title = "")

bmi_plot_1
ggsave("bmi_compare_1.png",bmi_plot_1)

data[42,]$WEIGHT #205
data[42,]$HEIGHT #29.5 not belong to (64.3825 ,76.0000), we will replace it by calculation
data_new<- data
data_new$HEIGHT[42]<- sqrt(703*(data$WEIGHT[42])/data$ADIPOSITY[42])
data_new$HEIGHT[42]

#after changing the height of point 42
compare_BMI$CalBMI[42]<- (703*data_new$WEIGHT[42])/((data_new$HEIGHT[42])^2)
bmi_plot_2<- ggplot(compare_BMI,aes(x=RealBMI,y=CalBMI))+geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  annotate("text",x=23.4,y=25,label="163",vjust=-1)+
  annotate("text",x=24.5,y=18,label="221",vjust=-1)
bmi_plot_2
ggsave("bmi_compare_2.png",bmi_plot_2)

max(compare_BMI$CalBMI-compare_BMI$RealBMI)
min(compare_BMI$CalBMI-compare_BMI$RealBMI)
data$WEIGHT[c(163,221)]
data$HEIGHT[c(163,221)]
data$BODYFAT[c(163,221)]
#they are all in the normal range, so we will keep them

#compare BodyFat
bodyfat_plot_1<- ggplot(compare_bodyfat,aes(x=RealBodyfat,y=CalBodyfat))+geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  annotate("text",x=6.4,y=9,label="48",vjust=-1)+
  annotate("text",x=17.3,y=-4,label="96",vjust=-1)+
  annotate("text",x=0,y=-3.7,label="182",vjust=-1)
bodyfat_plot_1
ggsave("bodyfat_compare.png",bodyfat_plot_1)

#we will remove point 182, since bodyfat can't be zero
data$DENSITY[c(48,96)]
data$BODYFAT[c(48,96)]
#point 48 is in the normal range, so we can keep it
#the density of point 96 is abnormal, we will remove it


#we remove point 182,96 and see the boxplot again
data_new<- data_new[-c(182,96),-2]

#deal with other outlier
del_outliers<- function(data,lower_b,upper_b){
  
  col_n<- ncol(data)
  for(i in 1:col_n){
    temp<- data[,i]
    q_lower<- quantile(temp,lower_b)
    q_upper<- quantile(temp,upper_b)
    data[temp < q_lower,i]<- q_lower
    data[temp > q_upper,i]<- q_upper
  }
  return(data)
}

data_new<- del_outliers(data_new,0.01,0.99)


write.csv(data_new,"cleaned.csv")


data_all<- as.matrix(data_new)
dim(data_all)<- c(nrow(data_new)*ncol(data_new),1)
col_name_1<- colnames(data_new)
facs<- gl(ncol(data_new),nrow(data_new),label=col_name_1)
data_fac<- data.frame(data_all,facs)

box<- ggplot(data_fac,aes(x=factor(data_fac$facs),y=data_fac$data_all,fill = factor(data_fac$facs)))+
  geom_boxplot(outlier.color = "red",outlier.shape=21,outlier.size = 1.5)+scale_fill_brewer(palette = "Pastel2")+
  labs(title = "boxplot of adjusted data",xlab="factor",ylab="data value")+guides(fill=FALSE)
box
ggsave("boxplot_adjusted.png",box)
#==========================================================
cor(data_new)
#Bodyfat has little correlation between age,height,ankle,forearm,wrist
#abdomen has strong correlation between weight,adiposity,neck,chest,hip,thigh,knee,biceps

#and then we can use the data_new to build a model


