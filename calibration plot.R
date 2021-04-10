plotCalibration(
  logRr,
  seLogRr,
  useMcmc = FALSE,
  legendPosition = "right",
  title,
  fileName = NULL
)


library(foreign) #载入外部数据需要应用此包
library(rms)#做列线图需要用的包

mydata<-read.table("2020090873.csv",header=T,sep = ",")#读取当前目录文件，CSV文件分割为","，标题为真，第一行设定为标题

str(mydata)#展示数据集的结构


mydata$MVI<-factor(mydata$MVI,labels=c("M1","M2","M3"))
mydata$preHBeAg<-factor(mydata$preHBeAg,labels=c("negative","positive"))
mydata$diameter5<-factor(mydata$diameter5,labels=c("≤5","＞5"))
mydata$Multiple.shots<-factor(mydata$Multiple.shots,labels=c("Single","Multiple"))
mydata$Satellite<-factor(mydata$Satellite,labels=c("no","yes"))
mydata$Serosa<-factor(mydata$Serosa,labels=c("no","yes"))
#把需要应用的分类变量设定哑变量并设定为无序多分类变量，原则上二分类变量无序上述改变，但改变后后续列线图会同时显示lable的名称。
#AFP值取了自然对数为底的对数，对其进行正太化，此处未显示

str(mydata)#再次展示数据集的结构，注意确定为因子变量的变量名称变化

attach(mydata)#把数据集加载入当前环境中

dev = mydata[mydata$devlopval==0,]#根据devlopval拆分为建模集
vad = mydata[mydata$devlopval==1,]#根据devlopval拆分为验证集


dd<-datadist(dev)
options(datadist='dd')
#以上两行命令对数据打包

fit.dev<-lrm(team~LnAFP+preHBeAg +diameter5+Multiple.shots+Serosa+MVI+Satellite,data=dev,x=T,y=T)
#构建Logstic回归模型，team为因变量，~后7个为自变量，data选择建模集，x=t和y=t的意思为在自变量及因变量出现缺失值时候的处理方式，数据集无缺失。
fit.dev#展示模型
summary(fit.dev)#用summary函数展示数据集


cal1 <- calibrate(fit.dev, cmethod='hare', method='boot', B=1000,data=vad)#建模组中绘制校准曲线
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0))#打印出校准曲线

valpro<-predict(object =fit.dev,type = "fitted",newdata=vad )#根据建模组方程，计算验证组预测概率

fit.vad<-lrm(team~valpro,data=vad,x=T,y=T)#验证组根据预测概率建立回归方程
fit.vad#展示fit.vad方程的参数

cal2 <- calibrate(fit.vad, cmethod='hare', method='boot', B=1000,data=vad)#验证组中绘制校准曲线
plot(cal2,xlim=c(0,1.0),ylim=c(0,1.0))#打印出校准曲线
