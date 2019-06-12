### Ex 1-7, 특정키워드

setwd("D:/Workplace/R_Data_Analysis/part2/stage1_Wordcloud/Wordcloud/Ex07_특정키워드")

install.packages("KoNLP")
library(KoNLP)
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
useSejongDic()

alert <- readLines("oracle_alert_testdb.log")
head(alert,10)
error_1 <- gsub(" ","_",alert)
head(unlist(error_1),20)
error_2 <- unlist(error_1)
head(error_2,10)
error_2 <- Filter(function(x) {nchar(x)>=5}, error_2)

error_3 <- grep("^ORA-+",error_2,value=T) # 문장의 처음이 'ORA-'인 것 추출
head(unlist(error_3),20)
write(unlist(error_3),"alert_testdb2.log")
rev <- read.table("alert_testdb2.log")
errorcount <- table(rev)
head(sort(errorcount,decreasing=T),20)

palete <- brewer.pal(9,"Set1")
wordcloud(names(errorcount),freq=errorcount,scale=c(5,0.5),rot.per=0.25,min.freq=3,
          colors=palete,random.order=F,random.color=T)
legend(0.3,0.85,"Oracle Alert Log File 분석 결과",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="blue")

display.brewer.all(n=10,exact.n=F)
