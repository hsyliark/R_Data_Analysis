### Ex 3-5, 년도별 기관별 보험청구 금액

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/Ex05_보험청구금액")
library(ggplot2)
library(plotly)
library(reshape2)

cash_ins <- read.csv("연도별요양기관별보험청구금액_2004_2013_세로.csv",header=T)

re_cash <- melt(cash_ins,id=c('년도'),variable.name='병원',value.name='보험청구금액')
re_cash$보험청구 <- re_cash$보험청구금액/1000000

options(scipen=10,digits=2)
p <- ggplot(re_cash,aes(x=년도,y=보험청구,color=병원,group=병원,fill=병원)) +
     ylim(0,10000) +
     geom_line(linetype=1,size=1) + geom_point(size=2) +
     geom_hline(yintercept=seq(0,10000,1000),lty="dotted",size=0.1) +
     xlab("년도") + ylab("보험청구금액") +
     ggtitle(paste("연도별 요양기관별 보험 청구 금액(단위:백만원)","\n",
                   "출처:건강보험심사평가원")) +
     theme(plot.title = element_text(color="black",size=16,face="bold.italic",hjust=0.5),
           axis.title.x = element_text(color="blue",size=13,face="bold"),
           axis.title.y = element_text(color="red",size=13,face="bold"))
ggplotly(p)
