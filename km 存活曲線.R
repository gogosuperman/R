Anderson <- read.csv("路徑/Anderson.csv")
install.packages("survival")
library("survival")
my.surv<-(Anderson$Survt,Anderson$Relapse==1)#其中 Anderson$Survt 為存活時間，Anderson$Relapse為事件狀態(0=設限; 1=復發，即發生事件)。
km.fit<-surfit(my.surv~ Rx, data=Anderson)#接著，利用指令“survfit( )”獲得 K-M 估計式，其中 “survfit( )” 的 “my.surv” 指存活資料，因為希望看到不同治療組別的存活曲線是否有明顯差異，因此“~ Rx”指將存活資料以不同的治療組別進行計算，檔案為“Anderson”。
plot(km.fit, conf.int=F, mark.time=T, col=c(“brown3”, “dodgerblue3”),
     lwd=2, cex.lab=1.2, cex.main=1.3, main=”Kaplan-Meier”, xlab=”Time(weeks)”, ylab=”survival probability”)
legend(“topright”, col=c(“brown3”, “dodgerblue3”),lwd=2, bty=”n”,
        legend=c(names(km.fit$strata[1]), names(km.fit$strata[2])#“topright” 為圖例位置，“col=”顏色與圖中的線一樣，“bty=n”表示不要有外框， “names(km.fit$strata[1])” 表示以 “km.fit”分層中的第一個分層的名稱命名。即完成Kaplan-Meier的繪製。
#https://medium.com/@juliehsieh/r-%E5%AD%98%E6%B4%BB%E6%9B%B2%E7%B7%9Akaplan-meier-7b073a43f5b