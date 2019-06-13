### Ex 3-4, 년도별 기관별 보험청구 건수

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/Ex04_보험청구")

library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(reshape2)

count_hp <- read.csv("연도별요양기관별보험청구건수_2001_2013_세로.csv",header=T)
count_hp
colname <- count_hp$년도
colname
v1 <- count_hp[,2]/100000
v2 <- count_hp[,3]/100000
v3 <- count_hp[,4]/100000
v4 <- count_hp[,5]/100000
v5 <- count_hp[,6]/100000
v6 <- count_hp[,7]/100000
v7 <- count_hp[,8]/100000
v8 <- count_hp[,9]/100000
v9 <- count_hp[,10]/100000
v10 <- count_hp[,11]/100000

plot(v1,xlab="",ylab="",ylim=c(0,6000),axes=F,col="violet",type="o",
     lwd=2,main=paste("연도별 요양기관별 보험 청구 건수(단위:십만건)","\n",
                      "출처:건강보험심사평가원"))
axis(1,at=1:10,label=colname,las=2)
axis(2,las=1)
lines(v2,col="blue",type="o",lwd=2)
lines(v3,col="red",type="o",lwd=2)
lines(v4,col="black",type="o",lwd=2)
lines(v5,col="blue",type="o",lwd=2)
lines(v6,col="green",type="o",lwd=2)
lines(v7,col="cyan",type="o",lwd=2)
lines(v8,col="yellow",type="o",lwd=2)
lines(v9,col="brown",type="o",lwd=2)
lines(v10,col="pink",type="o",lwd=2)
abline(h=seq(0,6000,500),v=seq(1,100,1),lty=3,lwd=0.2)
col <- names(count_hp[1,2:10])
colors_hp <- c("violet","blue","red","black","orange",
               "green","cyan","yellow","brown","pink")
legend(1,6000,col,cex=0.8,col=colors_hp,lty=1,lwd=2,bg="white")

# ggplot2

re_count <- melt(count_hp,id=c('년도'),variable.name="병원",
                 value.name="보험청구건수")
re_count$보험청구건수_1 <- re_count$보험청구건수/100000 
View(re_count)
options(scipen=10)
ggplot(re_count,aes(x=년도,y=보험청구건수_1,color=병원,group=병원,fill=병원)) +
  ylim(0,5500) +
  geom_line(linetype=1,size=1) + geom_point(aes(size=보험청구건수_1)) +
  geom_hline(yintercept=seq(0,500000000,50000000),lty="dashed",size=0.1) +
  xlab("년도") + ylab("보험청구건수") +
  ggtitle(paste("연도별 요양기관별 보험 청구 건수(단위:십만건)","\n",
                "출처:건강보험심사평가원")) +
  theme(plot.title = element_text(color="black",size=16,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"))

