library(readr)
data<- read.csv(file="/Users/zionhsu/Desktop/94_CVD_ALL.csv", skip = 4)
attach(data)#建立基本列連表（高血壓與心血管疾病）
TAB1<-table(收縮壓,心血管疾病)#使用table指令，將資料檔的hypertension 與cvd等兩個變數資料，建立一個基本的列聯表
d<-tab[1,1]
d<-tab[1,2]
b<-tab[2,1]
a<-tab[2,2]
relative_risk<-(a/(a+b))/(c/(c+d))#計算基本的rr與or
odds_risk<-(a/b)/(c/d)#使用epiR package 計算rr與or，並且計算95%信賴區間
install.packages('epiR')
librart('epiR')
epi.2by2(tab2,method="cohort.count",conf.level=0.95)
#將tab2所內涵的高血壓與心血管疾病之次數資料，建構列連表與其rr 與or數值。其中可以透過method調整列連表格式
#cohort.count是epi.2by2的四種方式之一
#也可以透過conf.level的設定來調整信賴區間的顯著水準