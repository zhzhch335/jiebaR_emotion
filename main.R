setwd("G:/workcloud/bjtudrive/Rworkspace/emotion analysis")
library(jiebaRD)
library(jiebaR)
jieba<-worker(type="tag",user="G:\\R\\R-3.3.3\\library\\jiebaRD\\dict\\emotioncal.dict.utf8",symbol = TRUE)
ecal<-function(str){
  result<-jieba<="#2017两会#他被称作“最牛中学校长”，他快人快语，流传很多经典语录。“我们都没有把学生当成一个活生生的人”“凡是把孩子放在第一位的，多半是悲剧”…今晚8点，@人民日报 对话全国政协委员、北京四中校长刘长铭，敬请关注！http://t.cn/RijCRoW <U+200B><U+200B><U+200B>"
  winfront<-1L#上一个窗口边界索引（可能是一个行首，一个情感词或者一个标点符号）
  count<-1#定义窗口的索引（便于句子结束时候的累加）
  #value<--1#定义情感的基本方向（本例中是消极统计）
  winvalue<-c()#存放窗口积极感情倾向值的数组
  posvalue<-c()#存放单句积极情感倾向总值的数组
  negvalue<-c()#存放单句消极情感倾向总值的数组
  pvalue<-0L #最终积极情感值
  nvalue<-0L #最终消极情感值
  ppcount<-1
  nncount<-1
  rheflag<-FALSE
  for (i in 1:length(result)){
    #开始查找情感倾向词
    if(names(result[i])=="positive"){
      cat("find a positive word",result[i],".")
      print(" ")
      cat("这时候的pvalue nvalue值是",pvalue,nvalue,".")
      print(" ")
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
      cat("经过计算这个就窗口值为",winvalue[count])
      print(" ")
      #窗口计算值完毕
      count=count+1
      #更新上一个窗口边界
      winfront<-i+1
    }
    else if(names(result[i])=="negative"){
      cat("find a negative word",result[i])
      print(" ")
      cat("这时候的pvalue nvalue值是",pvalue,nvalue,".")
      print(" ")
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
      cat("经过计算这个就窗口值为",winvalue[count])
      print(" ")
      #窗口计算值完毕
      count=count+1
      #更新上一个窗口边界
      winfront<-i+1
    }
    #开始查找句子边界符号或字符串结尾
    if(result[i]%in%c("?",".","!","？","。","！")|is.na(result[i+1])){
      cat("发现结束符，句子结束，此时窗口值为",winvalue)
      print(" ")
      cat("这时候的pvalue nvalue值是",pvalue,nvalue,".")
      print(" ")
      if(!is.null(winvalue)){
        posvalue[ppcount]<-0
        negvalue[nncount]<-0
        #累加窗口值
        for (k in 1:length(winvalue)) {
          if(winvalue[k]>0){
            posvalue[ppcount]<-posvalue[ppcount]+winvalue[k]
          }
          else if(winvalue[k]<0){
            negvalue[nncount]<-negvalue[nncount]+winvalue[k]
          }
        }
        if(result[i]%in%c("!","！")){
          posvalue[ppcount]=posvalue[ppcount]*2
          negvalue[nncount]=negvalue[nncount]*2
          cat("处理感叹句，处理后pvalue和nvalue为",pvalue,nvalue)
          print(" ")
        }
        else if(result[i]%in%c("?","？") & rheflag==TRUE ){
          temp<-negvalue[nncount]
          negvalue[nncount]=posvalue[nncount]*(-2)
          posvalue[ppcount]=temp*(-2)
          cat("处理反问句，处理后pvalue和nvalue为",pvalue,nvalue)
          print(" ")
        }
        #更新上一个窗口边界
        winfront<-i+1
        #窗口归位
        count<-1
        winvalue<-c()
        pvalue<-pvalue+posvalue[ppcount]
        nvalue<-nvalue+negvalue[nncount]
        #下一个句子索引
        ppcount<-ppcount+1
        nncount<-nncount+1
      }
    }
  }

  # pvalue<-0L #最终积极情感值
  # nvalue<-0L #最终消极情感值
  # #输出各项指标
  # for (i in 1:ppcount-1) {
  #   pvalue<-pvalue+posvalue[i]
  # }
  # for (i in 1:nncount-1) {
  #   nvalue<-nvalue+negvalue[i]
  # }

}