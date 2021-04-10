#install.packages('AER')
#安裝並載入gcookbook 
#install.packages(c("ggplot2", "gcookbook")) 
#library(ggplot2) 
#library(gcookbook) 

install.packages('ROCR')#ROCR套件中有現成的資料集，裡面分別是真實分類和預測機率。
require(ROCR)#ibrary和require都可以载入包，但二者存在区别。#在一个函数中，如果一个包不存在，执行到library将会停止执行，require则会继续执行。
data("ROCR.simple")
df <- data.frame(ROCR.simple)
df
df$predclass <- ifelse(df$predictions>0.5,1,0)#用0.5當作門檻值,prediciton>0.5的都预测为1，其余为0.
print(cf<-table(df[,c("predclass","labels")]))
tp<-cf[2,2]
tn<-cf[1,1]
fp<-cf[2,1]
fn<-cf[1,2]
accuracy<-(tp + tn)/(tp+tn+fp+fn)
accuracy
sensitivity <- tp/(tp+fn)
sensitivity
specificity <- tn/(tn+fp)
specificity
install.packages('caret')
require(caret)
install.packages('e1071', dependencies=TRUE)
confusionMatrix(cf,positive = "1")

library(knitr)#為了用kable # 安裝 magrittr 套件 ---------
install.packages("magrittr")# 載入 magrittr 套件 ---------
library(magrittr)#kable(as.data.frame(ROCR.simple)[1:10, ]) %>% kable_styling() %>% column_spec(1:2,width = "20em")
a = kable(as.data.frame(ROCR.simple)[1:10, ]) #為了方便只看前10筆資料，並轉換型態
a
pred <- ROCR::prediction(ROCR.simple$predictions,ROCR.simple$labels)#要predict 的column#ggplot2::ggplot()-->明確指出使用套件 ggplot2 內的函式 ggplot()
perf<- ROCR::performance(pred,"tpr","fpr")#performance()函数通過真陰性tpr和假陰性fpr来計算roc曲線下的面積auc
perf
install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)    # alternatively, this also loads %>% 結構化整理資料
auc <- performance(pred,'auc') %>% unlist %>% slot("y.values") %>% unlist()#算auc值
#出圖
plot(perf,colorize=TRUE,main=paste0("ROC Curce,AUC=",round(auc,2)));abline(0,1);grid()
#https://rpubs.com/ivan0628/probability_model_evaluation
#https://www.youtube.com/watch?v=qcvAqAH60Yw
