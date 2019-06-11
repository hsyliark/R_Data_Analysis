### Ex 1-3, 제주도 여행

setwd("D:/Workplace/R_Data_Analysis/part2/stage1_Wordcloud/Wordcloud/Ex03_제주도여행")
install.packages('rJava')
library(rJava)
install.packages("KoNLP")
library(KoNLP)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("stringr")
library(stringr)
useSejongDic()


mergeUserDic(data.frame(readLines("제주도여행지.txt")),"ncn") # 사전에 단어 추가

(txt <- readLines("jeju.txt"))
(place <- sapply(txt,extractNoun,USE.NAMES=F))
head(unlist(place),30)
cdata <- unlist(place)
place <- str_replace_all(cdata,"[^[:alpha:]]","") # 한글, 영어 외는 삭제
place <- gsub(" ","",place)
(txt <- readLines("제주도여행코스gsub.txt"))
(cnt_txt <- length(txt))
for (i in 1:cnt_txt) {
  place <- gsub((txt[i]),"",place)
}
place
place <- Filter(function(x) {nchar(x)>=2}, place)
write(unlist(place),"jeju_2.txt")
rev <- read.table("jeju_2.txt")
nrow(rev)
wordcount <- table(rev)
head(sort(wordcount,decreasing=T),30)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.3),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
legend(0.2,0.8,"제주도 여행 추천 코스 분석",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="blue",text.font=2,box.col="blue")
