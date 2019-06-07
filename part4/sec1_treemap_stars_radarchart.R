# treemap

install.packages("treemap")
library(treemap)
setwd("D:/Workplace/R_Data_Analysis/part4")
total <- read.csv("data/학생시험결과_전체점수.csv",header=T,sep=",")
total
treemap(total,vSize="점수",index=c('점수','이름'))
treemap(total,vSize="점수",index=c('조','이름'))
treemap(total,vSize="점수",index=c('점수','조','이름'))

# stars()

total <- read.table("data/학생별전체성적_new.txt",header=T,sep=",")
total
row.names(total) <- total$이름
total <- total[,2:7]
total
stars(total,flip.labels=F,draw.segment=F,frame.plot=T,full=T,
      main="학생별 과목별 성적분석-STAR Chart")
lab <- names(total)
value <- table(lab)
value
pie(value,labels=lab,radius=0.1,cex=0.6,col=NA)
stars(total,flip.labels=F,draw.segment=T,frame.plot=T,full=T,
      main="학생별 과목별 성적분석-Nightingale chart")
color <- c('black','red','green','blue','cyan','violet')
pie(value,labels=lab,radius=0.1,cex=0.6,col=color)
stars(total,flip.labels=F,draw.segment=T,frame.plot=T,full=F,
      main="학생별 과목별 성적분석-반원 차트")

# radarchart()

install.packages('fmsb')
library(fmsb)
layout <- data.frame(분석력=c(5,1),창의력=c(15,3),판단력=c(3,0),
                        리더쉽=c(5,1),사교성=c(5,1))
set.seed(123)
data1 <- data.frame(분석력=runif(3,1,5),창의력=rnorm(3,10,2),판단력=c(0.5,NA,3),
                       리더쉽=runif(3,1,5),사교성=c(5,2.5,4))
data2 <- rbind(layout,data1)
data2
par(mar=c(1,0.5,3,1),mfrow=c(2,2))
radarchart(data2,axistype=1,seg=5,plty=1,title="첫번째 타입")
radarchart(data2,axistype=2,pcol=topo.colors(3),seg=5,plty=1,title="두번째 타입")
radarchart(data2,axistype=3,pty=32,plty=1,axislabcol='grey',na.itp=F,title="세번째 타입")
radarchart(data2,axistype=0,plwd=1:5,pcol=1,title="네번째 타입")
par(mar=c(0,0,0,0),mfrow=c(1,1))
