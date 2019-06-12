### Ex 2-1, 제주도 여행코스 추천

setwd("D:/Workplace/R_Data_Analysis/part2/stage2_Graph/Ex01_제주도")
install.packages('rJava')
library(rJava)
install.packages("KoNLP")
library(KoNLP)
install.packages("wordcloud")
library(wordcloud)
install.packages("tm")
library(tm)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
useSejongDic()


## 데이터 처리

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


## 그래프

(top10 <- head(sort(wordcount,decreasing=T),10))
pie(top10,main="제주도 추천 여행 코스 TOP 10")
pie(top10,main="제주도 추천 여행 코스 TOP 10",col=rainbow(10),radius=1)

(pct <- round(top10/sum(top10)*100,1))
lab <- paste(names(top10),"\n",pct,"%")
paint <- brewer.pal(10,"Spectral")
pie(top10,main="제주도 추천 여행 코스 TOP 10",col=paint,labels=lab,cex=0.8)

(bchart <- head(sort(wordcount,decreasing=T),10))
bp <- barplot(bchart,main="제주도 추천 여행 코스 TOP 10",col=rainbow(10),
             cex.names=0.7,las=2,ylim=c(0,25))
pct <- round(bchart/sum(bchart)*100,1)
pct
text(x=bp,y=bchart*1.05,labels=paste("(",pct,"%",")"),col="black",cex=0.7)
text(x=bp,y=bchart*0.95,labels=paste(bchart,"건"),col="black",cex=0.7)

barplot(bchart,main="제주도 추천 여행 코스 TOP 10",col=rainbow(10),
        cex.names=0.7,las=1,xlim=c(0,25),horiz=T)
text(y=bp,x=bchart*1.15,labels=paste("(",pct,"%",")"),col="black",cex=0.7)
text(y=bp,x=bchart*0.9,labels=paste(bchart,"건"),col="black",cex=0.7)

# ggplot2

str(top10)
(df_top10 <- as.data.frame(top10))

install.packages("dplyr")
library(dplyr)
options(digits=2)

(df_top10 <- df_top10 %>%
  mutate(pct=Freq/sum(Freq)*100) %>%
  mutate(ylabel=paste(sprintf("%s\n%4.1f",rev,pct),'%',sep='')) %>%
  arrange(desc(rev)) %>%
  mutate(ypos=cumsum(pct)-0.5*pct)
)

install.packages("extrafont")
library(extrafont)
windowsFonts()
windowsFonts(malgun="맑은 고딕")
theme_update(text=element_text(family="malgun"))

ggplot(df_top10,aes(x="",y=Freq,fill=rev)) +
  geom_bar(stat="identity", width=1) +  
  geom_text(aes(y=ypos,label=ylabel),color="black") +
  coord_polar(theta="y",start=0) +
  xlab("") + ylab("") +
  ggtitle("제주도 추천 여행 코스 TOP 10") +
  theme(plot.title=element_text(color="brown",size=16,face="bold",hjust=0.5))
        

# 3D pie chart

install.packages("plotrix")
library(plotrix)
th_pct <- round(bchart/sum(bchart)*100,1)
th_names <- names(bchart)
th_labels <- paste(th_names,"\n","(",th_pct,")")
pie3D(bchart,main="제주도 추천 여행 코스 TOP 10",col=rainbow(10),
      cex=0.3,labels=th_labels,explode=0.05)
