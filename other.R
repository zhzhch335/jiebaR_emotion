setwd("G:/workcloud/bjtudrive/Rworkspace/emotion analysis")#！！！这只是我自己的根目录请记得按照自己的目录修改
library(jiebaRD)
library(jiebaR)
library(stringr)
#普通引擎
jieba<-worker("tag")
#网络词语引擎，自行选择修改
jieba_net<-worker(type="tag",user = "dict/net.dict.utf8")
#情感分析引擎，自行选择修改
jieba_porn<-worker(type="tag",user = "dict/emotion.dict.utf8")
#保留符号引擎，自行选择修改
jieba_sym<-worker(symbol = TRUE)
#祈使词引擎，自行选择修改
jieba_imp<-worker(type="tag",user = "dict/imperative.dict.utf8")
#逐行读取字符串信息，注意换行符
article_n<-readLines("这里填写你的文件路径.txt",encoding = "UTF-8",n=-1L)
####以上命令执行一次即可
#初始化输出向量
check_num<-c()#总词数
check_anum<-c()#总字数
check_noun<-c()#名词数
check_adj<-c()#形容词数
check_adv<-c()#副词数
check_numnum<-c()#数字数
check_fp<-c()#一人称代词
check_sp<-c()#二人称代词
check_tp<-c()#三人称代词
check_net<-c()#网络热词
check_url<-c()#链接
check_at<-c()#@数量
check_imp<-c()#祈使词
check_pos<-c()#积极词语
check_neg<-c()#消极词语
i<-1
#提取所有数字
numnum<-str_extract_all(article_n,"[0-9]+")
#提取链接中的数字
numnum_url<-str_extract_all(article_n,"[a-zA-Z][0-9]+[a-zA-z]")
#遍历所有记录，提取所需数据
for (i in 1:length(article_n)) {
  #分词和词性标注处理
  tagstr<-jieba<=article_n[i]
  #根据标签提取数据
  check_num[i]<-length(tagstr[names(tagstr)!='eng'])
  check_anum[i]<-nchar(article_n[i])
  check_noun[i]<-length(tagstr[names(tagstr)%in%c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng","nrfg")])
  check_adj[i]<-length(tagstr[names(tagstr)%in%c('a','ad','an','ag','al')])
  check_adv[i]<-length(tagstr[names(tagstr)=='d'])
  check_numnum[i]<-lengths(numnum[i])-lengths(numnum_url[i])
  check_fp[i]<-length(tagstr[tagstr%in%c("我","我们")])
  check_sp[i]<-length(tagstr[tagstr%in%c("你","你们")])
  check_tp[i]<-length(tagstr[tagstr%in%c("他","她","它","他们","她们","它们")])
  tagstr<-jieba_net<=article_n[i]
  check_net[i]<-length(tagstr[names(tagstr)=='net'])
  check_url[i]<-length(tagstr[tagstr=='http'])
  tagstr<-jieba_sym<=article_n[i]
  check_at[i]<-length(tagstr[tagstr=="@"])
  tagstr<-jieba_imp<=article_n[i]
  check_imp[i]<-length(tagstr[names(tagstr)=='imp'])
  tagstr<-jieba_porn<=article_n[i]
  check_pos[i]<-length(tagstr[names(tagstr)=='positive'])
  check_neg[i]<-length(tagstr[names(tagstr)=='negative'])
}
#拼合数据
result<-data.frame(内容=article_n,总词数=check_num,总字数=check_anum,名词=check_noun,
                       形容词=check_adj,副词=check_adv,数字=check_numnum,一人称代词=check_fp,
                       二人称代词=check_sp,三人称代词=check_tp,网络热词=check_net,
                       链接=check_url,所提及人=check_at,祈使词=check_imp,
                       积极情感词=check_pos,消极情感词=check_neg)
#生成数据文件，注意修改文件路径
file<-file("你的输出文件路径.csv","w")
write.table(result,file,sep=",",col.names = NA)
close(file)