setwd("G:/workcloud/bjtudrive/Rworkspace/jiebaR_emotion")#！！！这只是我自己的根目录请记得按照自己的目录修改
library(jiebaRD)
library(jiebaR)#引入两个jieba包
jieba<-worker(type="tag",user="dict/emotioncal.dict.utf8",symbol = TRUE)#这里的user属性填写情感计算词典的路径，情感词典格式见例子
#定义分析函数
ecal<-function(str){
  result<-jieba<=str#将字符串进行分词和标记处理
  winfront<-1L#上一个窗口边界索引（可能是一个行首，一个情感词或者一个标点符号）
  count<-1#定义窗口的索引（便于句子结束时候的累加）
  winvalue<-c()#存放窗口积极感情倾向值的数组
  posvalue<-c()#存放单句积极情感倾向总值的数组
  negvalue<-c()#存放单句消极情感倾向总值的数组
  pvalue<-0L #最终积极情感值
  nvalue<-0L #最终消极情感值
  sencount<-1
  sencount<-1
  rheflag<-FALSE
  for (i in 1:length(result)){
    #开始查找情感倾向词
    if(names(result[i])=="positive"){      
      cat("发现积极感情倾向词：",result[i],"\n")      
      #判断是否第一位
      if(i==1)
        winvalue[count]<-1
      else{
        winvalue[count]<-1
        #找到下一个正向情感倾向词，向前遍历至窗口边界
        for (j in (i-1):winfront) {
          #记录程度副词数并计算
          if(!is.na((as.numeric(names(result[j])))))
            winvalue[count]=winvalue[count]*as.numeric(names(result[j]))
          #记录否定词数并计算
          else if(names(result[j])=="deny")
            winvalue[count]=winvalue[count]*(-1)
          #查找是否包含反问词
          else if(names(result[j])=="rhe")
            rheflag<-TRUE
        }
      }
      cat("经过计算这个滑动窗口值为：",winvalue[count],"\n")      
      #窗口计算值完毕
      count=count+1
      #更新上一个窗口边界
      winfront<-i+1
    }
    else if(names(result[i])=="negative"){
      cat("发现消极情感倾向词：",result[i],"\n")      
      if(i==1)
        winvalue[count]<--1
      else{
        winvalue[count]<--1
        #找到下一个负向情感倾向词，向前遍历至窗口边界
        for (j in (i-1):winfront) {
          #记录否定词数并计算
          if(names(result[j])=="deny")
            winvalue[count]=winvalue[count]*(-1)
          #记录程度副词数并计算
          else if(!is.na((as.numeric(names(result[j])))))
            winvalue[count]=winvalue[count]*as.numeric(names(result[j]))
          #查找是否包含反问词
          else if(names(result[j])=="rhe")
            rheflag<-TRUE
        }
      }
      cat("第",count,"个窗口计算结束，这个就窗口情感值为：",winvalue[count],"\n")      
      #窗口计算值完毕
      count=count+1
      #更新上一个窗口边界
      winfront<-i+1
    }
    #开始查找句子边界符号或字符串结尾
    if(result[i]%in%c("?",".","!","？","。","！")|is.na(result[i+1])){
      cat("发现结束符，句子结束，此时各个窗口值为：",winvalue,"\n")                  
      if(!is.null(winvalue)){
        posvalue[sencount]<-0
        negvalue[sencount]<-0
        #累加窗口值
        for (k in 1:length(winvalue)) {
          if(winvalue[k]>0){
            posvalue[sencount]<-posvalue[sencount]+winvalue[k]
          }
          else if(winvalue[k]<0){
            negvalue[sencount]<-negvalue[sencount]+winvalue[k]
          }
        }
        cat("第",,"句的积极情感值、消极情感值值分别是：",posvalue[sencount],negvalue[sencount],"\n")
        if(result[i]%in%c("!","！")){
          posvalue[sencount]=posvalue[sencount]*2
          negvalue[sencount]=negvalue[sencount]*2
          cat("处理感叹句，处理后积极情感值和消极情感值分别为：",posvalue[sencount],negvalue[sencount],"\n")          
        }
        else if(result[i]%in%c("?","？") & rheflag==TRUE ){
          temp<-negvalue[sencount]
          negvalue[sencount]=posvalue[sencount]*(-2)
          posvalue[sencount]=temp*(-2)
          cat("处理反问句，处理后积极情感值和消极情感值分别为：",posvalue[sencount],negvalue[sencount],"\n")          
        }
        #更新上一个窗口边界
        winfront<-i+1
        #窗口归位
        count<-1
        winvalue<-c()
        pvalue<-pvalue+posvalue[sencount]
        nvalue<-nvalue+negvalue[sencount]
        #下一个句子索引
        sencount<-sencount+1        
      }
    }
  }
  return(c(pvalue,nvalue))
}