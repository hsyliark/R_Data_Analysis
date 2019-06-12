### Ex 1-6, 스티브잡스 

setwd("D:/Workplace/R_Data_Analysis/part2/stage1_Wordcloud/Wordcloud/Ex06_스티브잡스")

install.packages("wordcloud")
library(wordcloud)
install.packages("tm")
library(tm)
install.packages("RColorBrewer")
library(RColorBrewer)

(data1 <- readLines("tm_test1.txt"))
class(data1)
(corp1 <- Corpus(VectorSource(data1)))
inspect(corp1)
(tdm <- TermDocumentMatrix(corp1))
(m <- as.matrix(tdm))

corp2 <- tm_map(corp1,stripWhitespace) # 여러개 공백 -> 하나의 공백
corp2 <- tm_map(corp2,tolower) # 대문자 -> 소문자
corp2 <- tm_map(corp2,removeNumbers) # 숫자 제거
corp2 <- tm_map(corp2,removePunctuation) # 문자 제거
# corp2 <- tm_map(corp2,PlainTextDocument)
sword2 <- c(stopwords('en'),"and","but","not") # 불용어 추가
corp2 <- tm_map(corp2,removeWords,sword2) #불용어 제거
stopwords('en')

(tdm2 <- TermDocumentMatrix(corp2))
(m2 <- as.matrix(tdm2))
class(m2)
colnames(m2) <- 1:4
freq1 <- sort(rowSums(m2),decreasing=T)
head(freq1,20)
freq2 <- sort(colSums(m2),decreasing=T)
head(freq2,20) 
findFreqTerms(tdm2,2)
findAssocs(tdm2,"apple",0.5)

palete <- brewer.pal(7,"Set3")
wordcloud(names(freq1),freq=freq1,scale=c(5,1),min.freq=1,
          colors=palete,random.order=F,random.color=T)
legend(0.3,0.85,"tm package test #2",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="blue")
barplot(freq1,main="tm package test #2",las=2,ylim=c(0,5))

# Steve Jobs
(data1 <- readLines("steve.txt"))
(corp1 <- Corpus(VectorSource(data1)))
inspect(corp1)
(tdm <- TermDocumentMatrix(corp1))
(m <- as.matrix(tdm))

corp2 <- tm_map(corp1,stripWhitespace) # 여러개 공백 -> 하나의 공백
corp2 <- tm_map(corp2,tolower) # 대문자 -> 소문자
corp2 <- tm_map(corp2,removeNumbers) # 숫자 제거
corp2 <- tm_map(corp2,removePunctuation) # 문자 제거
# corp2 <- tm_map(corp2,PlainTextDocument)
sword2 <- c(stopwords('en'),"and","but","not") # 불용어 추가
corp2 <- tm_map(corp2,removeWords,sword2) #불용어 제거

(tdm2 <- TermDocumentMatrix(corp2))
(m2 <- as.matrix(tdm2))
colnames(m2) <- 1:59
freq1 <- sort(rowSums(m2),decreasing=T)
head(freq1,20)
freq2 <- sort(colSums(m2),decreasing=T)
head(freq2,20) 
findFreqTerms(tdm2,10)
findAssocs(tdm2,"apple",0.5)

palete <- brewer.pal(7,"Set3")
wordcloud(names(freq1),freq=freq1,scale=c(5,1),min.freq=1,
          colors=palete,random.order=F,random.color=T)
legend(0.3,0.85,"Speech of Steve Jobs",cex=0.8,fill=NA,border=NA,bg="white",
       text.col="red",text.font=2,box.col="blue")