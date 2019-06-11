### Ex 1-1, 서울시 응답소

setwd("D:/Workplace/R_Data_Analysis/part2/stage1_Wordcloud/Wordcloud/Ex01_서울시응답소")

install.packages('rJava')
library(rJava)
install.packages("KoNLP")
library(KoNLP)
install.packages("wordcloud")
library(wordcloud)

useSejongDic()
# mergeUserDic(data.frame("서진수","ncn"))

data1 <- readLines("data/seoul_new.txt")
data1
data2 <- sapply(data1,extractNoun,USE.NAMES=F)
data2
head(unlist(data2),30)

data3 <- unlist(data2) # 명사 데이터
data3
# 필요없는 데이터 거르기
data3 <- gsub("\\d+","",data3) # 모든 숫자 없애기
data3 <- gsub("서울시","",data3)
data3 <- gsub("서울","",data3)
data3 <- gsub("요청","",data3)
data3 <- gsub("제안","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("-","",data3)
data3 <- gsub("00","",data3)
data3 <- gsub("~","",data3)
data3 <- gsub("B","",data3)

data3

write(unlist(data3),"data/seoul_2.txt")
data4 <- read.table("data/seoul_2.txt")
data4

nrow(data4)
wordcount <- table(data4)
wordcount
head(sort(wordcount,decreasing=T),20)

data3 <- gsub("OO","",data3)
data3 <- gsub("개선","",data3)
data3 <- gsub("문제","",data3)
data3 <- gsub("관리","",data3)
data3 <- gsub("민원","",data3)
data3 <- gsub("이용","",data3)
data3 <- gsub("관련","",data3)
data3 <- gsub("시장","",data3)
data3 <- gsub("님","",data3)

# 제거작업(반복문)
txt <- readLines("data/gsubfile.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for (i in 1:cnt_txt) {
  data3 <- gsub((txt[i]),"",data3)
}
data3

write(unlist(data3),"data/seoul_3.txt")
data4 <- read.table("data/seoul_3.txt")
wordcount <- table(data4)
head(sort(wordcount,decreasing=T),20)

# Word Cloud 그래픽
install.packages("RColorBrewer")
library(RColorBrewer)
palete <- brewer.pal(9,"Set3")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.3),rot.per=0.1,min.freq=1,
          random.order=F,random.color=T,colors=palete)
legend(0.3,0.7,"서울시 응답소 요청사항 분석",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="red")


## KoNLP

v1 <- ("봄이 지나면 여름이고 여름이 지나면 가을입니다. 그리고 겨울이죠.")
extractNoun(v1)
v2 <- ("봄이지나 면여름이고 여름이지나면가을입니다. 그리고 겨울이죠.") 
extractNoun(v2)
v3 <- c("봄이 지나면 여름이고 여름이 지나면 가을입니다.", 
        "그리고 겨울이죠.")
extractNoun(v3)
v4 <- sapply(v3,extractNoun,USE.NAMES=F)
v4

## wordcloud

wordcloud(c(letters,LETTERS,0:9),seq(1,1000,len=62))
palete <- brewer.pal(9,"Set1")
wordcloud(c(letters,LETTERS,0:9),seq(1,1000,len=62),colors=palete)
