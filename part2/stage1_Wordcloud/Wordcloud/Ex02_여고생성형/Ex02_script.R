### Ex 1-2, 여고생 성형

setwd("D:/Workplace/R_Data_Analysis/part2/stage1_Wordcloud/Wordcloud/Ex02_여고생성형")

install.packages('rJava')
library(rJava)
install.packages("KoNLP")
library(KoNLP)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
useSejongDic()

(data1 <- readLines("remake.txt"))
(data2 <- sapply(data1,extractNoun,USE.NAMES=F)) # 리스트 형태의 명사 추출
(data3 <- unlist(data2))
data3 <- Filter(function(x) {nchar(x)<=10}, data3)
head(unlist(data3),30)

data3 <- gsub("\\d+","",data3)
data3 <- gsub("쌍수","쌍꺼풀",data3)
data3 <- gsub("쌍커풀","쌍꺼풀",data3)
data3 <- gsub("메부리코","매부리코",data3)
data3 <- gsub("\\.","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("\\'","",data3)
data3

write(unlist(data3),"remake_2.txt")
data4 <- read.table("remake_2.txt")
(wordcount <- table(data4))
head(sort(wordcount,decreasing=T),20)

(txt <- readLines("성형gsub.txt"))
(cnt_txt <- length(txt))
for (i in 1:cnt_txt) {
  data3 <- gsub((txt[i]),"",data3)
}
data3
data3 <- Filter(function(x) {nchar(x)>=2}, data3)
data3 <- gsub("^ㅎ^ㅎ^ㅎ^ㅎ^ㅎ","",data3)
data3 <- gsub("ㅠㅠ","",data3)
data3 <- gsub("것","",data3)
data3 <- gsub("기","",data3)
data3 <- gsub("개","",data3)
data3 <- gsub("한","",data3)
data3 <- gsub("&","",data3)
data3 <- gsub("병원","",data3)
(txt <- readLines("성형부작용gsub.txt"))
(cnt_txt <- length(txt))
for (i in 1:cnt_txt) {
  data3 <- gsub((txt[i]),"",data3)
}
data3
write(unlist(data3),"remake_2.txt")
data4 <- read.table("remake_2.txt")
(wordcount <- table(data4))
head(sort(wordcount,decreasing=T),30)

palete <- brewer.pal(8,"Accent")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.3),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
legend(0.2,0.8,"여고생들이 선호하는 성형 수술 부위",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="red")
