setwd("D:/Workplace/R_Data_Analysis/part4")

# package ggplot2
install.packages('ggplot2')
library(ggplot2)

(korean <- read.table("data/학생별국어성적_new.txt",header=T,sep=","))
ggplot(korean,aes(x=이름,y=점수)) + 
  geom_point()
ggplot(korean,aes(x=이름,y=점수)) + 
  geom_bar(stat="identity")
gg <- ggplot(korean,aes(x=이름,y=점수)) + 
  geom_bar(stat="identity",fill="green",color="red")
gg + theme(axis.title.x=element_text(angle=45,hjust=1,vjust=1,color="blue",size=8))

(score_kem <- read.csv("data/학생별과목별성적_국영수_new.csv",header=T))
library(dplyr)
(sort_kem <- arrange(score_kem,이름,과목))
# (sort_kem2 <- ddply(sort_kem,"이름",transform,누적합계=cumsum(점수)))
sort_kem2 <- sort_kem %>%
  group_by(이름) %>%
  mutate(누적합계=cumsum(점수))
# (sort_kem3 <- ddply(sort_kem2,"이름",transform,누적합계=cumsum(점수),
#                   label=cumsum(점수)-0.5*점수))
sort_kem3 <- sort_kem2 %>%
  group_by(이름) %>%
  mutate(누적합계=cumsum(점수),label=cumsum(점수)-0.5*점수)
gg2 <- ggplot(sort_kem3,aes(x=이름,y=점수,fill=과목)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label,label=paste(점수,'점')),color="black",size=4) +
  guides(fill=guide_legend(reverse=T))
gg2 + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,color="black",size=8))

(score <- read.table("data/학생별전체성적_new.txt",header=T,sep=","))
ggplot(score,aes(x=영어,y=reorder(이름,영어))) +
  geom_point(size=6) +
  theme_bw() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(color="red",linetype="dashed"))
ggplot(score,aes(x=영어,y=reorder(이름,영어))) +
  geom_segment(aes(yend=이름),xend=0,color="blue") +
  geom_point(size=6,color="green") +
  theme_bw() +
  theme(panel.grid.major.y=element_blank())

install.packages("gridExtra")
library(gridExtra)

(v_mt <- mtcars)
View(v_mt)
graph1 <- ggplot(v_mt,aes(x=hp,y=mpg))
graph1 + geom_point()
graph2 <- graph1 + geom_point(color="blue")
graph2
graph3 <- graph2 + geom_point(aes(color=factor(am)))
graph3
graph4 <- graph1 + geom_point(size=7)
graph4
graph5 <- graph1 + geom_point(aes(size=wt))
graph5
graph6 <- graph1 + geom_point(aes(shape=factor(am),size=wt))
graph6
graph7 <- graph1 + geom_point(aes(shape=factor(am),color=factor(am),size=wt)) +
  scale_color_manual(values=c("red","green"))
graph7
graph8 <- graph1 + geom_point(color="red") + geom_line()
graph8
graph9 <- graph1 + geom_point(color="blue") +
  labs(x="마력",y="연비(mile/gallon)")
graph9

(three <- read.csv("data/학생별과목별성적_3기_3명.csv",header=T))
(sort_score <- arrange(three,이름,과목))
ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름)) +
  geom_line() + geom_point()
ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름,fill=이름)) +
  geom_line() + geom_point(size=6,shape=22) +
  ggtitle("학생별 과목별 성적") +
  theme(plot.title = element_text(color="brown",size=15,face="bold.italic",hjust=0.5),
    axis.title.x = element_text(color="black",size=12,face="bold"),
    axis.title.y = element_text(color="black",size=12,face="bold"))

(dis <- read.csv("data/1군전염병발병현황_년도별.csv",stringsAsFactors=F))
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) +
  geom_line()
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) +
  geom_area()
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) +
  geom_line() + geom_point()
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) +
  geom_area(fill="cyan",alpha=0.4) + geom_line()

