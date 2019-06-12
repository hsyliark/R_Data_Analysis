### 6월 12일 그래프 과제

setwd("D:/Workplace/R_Data_Analysis/part2/stage2_Graph/6월 12일 그래프 과제")

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
install.packages("ggplot2")
library(ggplot2)
install.packages("tm")
library(tm)
install.packages("dplyr")
library(dplyr)
install.packages("sqldf")
library(sqldf)
useSejongDic()


## 데이터 NLP 처리

hiphop <- readLines("hiphop.txt")
hiphop <- unlist(sapply(hiphop,extractNoun,USE.NAMES=F))
hiphop <- Filter(function(x) {nchar(x)>=2}, hiphop) # 2글자 이상인 경우만 추출
head(hiphop,100)
hiphop <- Corpus(VectorSource(hiphop))
inspect(hiphop)

hiphop <- tm_map(hiphop,stripWhitespace) # 여러개 공백 -> 하나의 공백
hiphop <- tm_map(hiphop,tolower) # 대문자 -> 소문자
hiphop <- tm_map(hiphop,removeNumbers) # 숫자 제거
hiphop <- tm_map(hiphop,removePunctuation) # 문자 제거
stopword2 <- c(stopwords('en'),"and","but","not","설국열","들이",
               "frien","때까","니가","하기","al","re","기다","fas","tim","tende",
               "마르","jus","lov","dow","닳도","모르겠","좋았","gir","gam","소프라",
               "eidorad","돼주","알았","아프","집으","벌릴테","악보따","아인슈타",
               "fuc","이기","girlfrien","버티겠","바랬잖","nigh","poin","zer",
               "베토","씌우","뜬눈우","ambitio","놓치지않","tim","eh","reaso",
               "madeleine","studi","’","’m","nee") # 불용어 추가
hiphop <- tm_map(hiphop,removeWords,stopword2) #불용어 제거

hiphop <- TermDocumentMatrix(hiphop)
findFreqTerms(hiphop,10) # 빈도수가 10 이상인 경우

hiphop <- as.matrix(hiphop)
nrow(hiphop)

wordcount <- sort(rowSums(hiphop),decreasing=T)
head(wordcount,20)

palete <- brewer.pal(10,"Paired")
par(oma=rep(0,4))
wordcloud(names(wordcount),freq=wordcount,scale=c(6,1),rot.per=0.25,min.freq=1,
          colors=palete,random.order=F,random.color=T)
legend(0.3,0.85,"hiphop 가사 분석결과",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="blue")



## Pie Chart (ggplot2)

top10 <- head(sort(wordcount,decreasing=T),10)
top10 <- as.data.frame(top10)
noun <- as.data.frame(rownames(top10))
top10 <- cbind.data.frame(noun,top10)
colnames(top10) <- c("noun","top10")
options(digits=2)
top10 <- top10 %>% 
    mutate(pct=top10/sum(top10)*100) %>%
    mutate(ylabel=paste(sprintf("%s\n%4.1f",noun,pct),'%',sep=''))
top10 <- top10[c(2,7,4,9,8,5,1,3,6,10),]
top10 <- top10 %>%
    mutate(ypos=cumsum(pct)-0.5*pct)
colnames(top10) <- c("noun","freq","pct","ylabel","ypos")

ggplot(top10,aes(x="",y=pct,fill=noun)) +
  geom_bar(stat="identity", width=1) +  
  geom_text(aes(y=ypos,label=ylabel),
            color="black") +
  coord_polar(theta="y",start=0) +
  xlab("") + ylab("") +
  ggtitle("hiphop 가사 분석결과") +
  theme(plot.title=element_text(
    color="brown",size=16,
    face="bold",hjust=0.5))


## Barplot (ggplot2)

top10 <- top10 %>%
  mutate(freqpos=0.8*freq)

ggplot(top10,aes(x=noun,y=freq,fill=noun)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=freqpos,label=ylabel),
            color="black") +
  xlab("명사") + ylab("빈도수") +
  ggtitle("hiphop 가사 분석결과") +
  theme(plot.title=element_text(
    color="brown",size=16,
    face="bold",hjust=0.5))

