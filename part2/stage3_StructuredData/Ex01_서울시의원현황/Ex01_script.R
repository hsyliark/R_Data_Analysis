### Ex 3-1, 서울시 의원현황

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/Ex01_서울시의원현황")
library(ggplot2)

(data1 <- read.csv("2013년_서울_주요구별_병원현황.csv",header=T))
barplot(as.matrix(data1[1:9,2:11]),
        main=paste("서울시 주요 구별 과목별 병원현황-2013년","\n",
                   "출처[국민건강보험공단]"),ylab="병원수",beside=T,
        col=rainbow(8),ylim=c(0,350))
abline(h=seq(0,350,10),lty=3,lwd=0.2)
(name <- data1$표시과목)
legend(25,350,name,csx=0.8,fill=rainbow(8),bg="white")

library(plyr)
library(dplyr)
data1_1 <- read.csv("2013년_서울_주요구별_병원현황_ggplot2.csv",header=T)
sort_med1 <- arrange(data1_1,지역구,표시과목)
sort_med2 <- ddply(sort_med1,"지역구",transform,누적합계=cumsum(병원수),
                   label=cumsum(병원수)-0.5*병원수)
ggplot(sort_med2,aes(x=지역구,y=병원수,fill=표시과목)) +
  geom_bar(stat="identity") +
  ggtitle("서울시 주요 구별 과목별 병원현황-2013년") +
  theme(plot.title=element_text(color="darkblue",size=16,face="bold",hjust=0.5),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1,colour="black",size=10))

# Long format

library(reshape2)
clinic <- melt(data1,id=c('표시과목'),variable.name="지역구",
               value.name="병원수")
View(clinic)
colnames(clinic) <- c("표시과목","지역구","병원수")
p <- ggplot(clinic,aes(x=지역구,y=병원수,fill=표시과목)) +
     geom_bar(stat="identity",position="dodge") +
     geom_hline(yintercept=seq(0,330,10),lty="dashed",size=0.1) +
     ggtitle(paste("서울시 주요 구별 과목별 병원현황-2013년","\n",
                   "출처[국민건강보험공단]")) +
     theme(plot.title=element_text(color="darkblue",size=16,face="bold",hjust=0.5))

install.packages('plotly')
library(plotly)  
ggplotly(p) # interactive graph
