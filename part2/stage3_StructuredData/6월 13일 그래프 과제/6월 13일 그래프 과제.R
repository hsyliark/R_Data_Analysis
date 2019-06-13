### 6월 13일 그래프 과제

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/6월 13일 그래프 과제")

## 취업자현황

(workday <- read.csv("고용형태별_취업자현황_근무일수.csv",header=T))
(pay <- read.csv("고용형태별_취업자현황_월급현황.csv",header=T))

library(reshape2)
re_workday <- melt(workday,id=c("년도"),variable.name="고용형태",value.name="근무일수")
re_pay <- melt(pay,id=c("년도"),variable.name="고용형태",value.name="월급")

# 근무일수
ggplot(re_workday,aes(x=년도,y=근무일수,col=고용형태)) +
  geom_line(linetype=1,size=1) + 
  geom_point(size=2) +
  geom_hline(yintercept=seq(10,25,1),lty="dashed",size=0.1) +
  geom_vline(xintercept=seq(2007,2013,1),
             lty="dashed",size=0.1) +
  ylim(10,25) +
  ggtitle("고용형태별 근무일수(단위:일) 출처:통계청") +
  theme(plot.title = element_text(color="black",
                                  size=20,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="blue",size=15,face="bold"),
        axis.title.y = element_text(color="red",size=15,face="bold"))

# 급여현황
options(scipen=5)
ggplot(re_pay,aes(x=년도,y=월급,col=고용형태)) +
  geom_line(linetype=1,size=1) + geom_point(size=2) +
  geom_hline(yintercept=seq(500,3000,100),
             lty="dashed",size=0.1) +
  geom_vline(xintercept=seq(2007,2013,1),lty="dashed",size=0.1) +
  ylim(500,3000) +
  ggtitle("고용형태별 급여현황(단위:천원) 출처:통계청") +
  theme(plot.title = element_text(color="black",
                                  size=20,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="blue",size=15,face="bold"),
        axis.title.y = element_text(color="red",size=15,face="bold"))


## 마포09번 마을버스

(mapobus <- read.csv("마포09번이용현황.csv",header=T))
re_mapo <- melt(mapobus,id=c("정류소명"),variable.name="승하차",value.name="승객수")

# barplot
options(scipen=5)
ggplot(re_mapo,aes(x=정류소명,y=승객수,fill=승하차)) +
  geom_bar(stat='identity',position='dodge') +
  geom_hline(yintercept=seq(0,50000,5000),
             lty="dashed",size=0.1) +
  ylim(0,50000) +
  ggtitle("마포09번 이용 승객수(단위:명) - 2014년 1월 기준") +
  theme(plot.title = element_text(color="darkgreen",
                                  size=20,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="orange",size=15,face="bold"),
        axis.title.y = element_text(color="purple",size=15,face="bold"),
        axis.text.x = element_text(angle=90,hjust=1,vjust=1))


## 콩나물 마을 버스

(village <- read.csv("버스노선별이용현황합계.csv",header=T))
re_village <- melt(village,id=c("버스노선번호"),variable.name="승하차",value.name="승객수")
library(plyr)
library(dplyr)
(re_village <- re_village %>%
  mutate(guest=승객수/1000) %>%
  mutate(ylabel=guest*0.95))

# barplot
options(scipen=6)
ggplot(re_village,aes(x=버스노선번호,y=guest,fill=승하차)) +
  geom_bar(stat='identity',position='dodge') +
  geom_text(aes(y=ylabel,label=승객수),colour="maroon",size=4) +
  ylim(0,350) + xlab("노선명") + ylab("이용 승객수(단위:천명)") +
  ggtitle("서울 주요 마을 버스 이용 승객 현황(2014년 1월)") +
  theme(plot.title = element_text(color="forestgreen",
                                  size=20,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="navy",size=15,face="bold"),
        axis.title.y = element_text(color="darkkhaki",size=15,face="bold"),
        axis.text.x = element_text(angle=90,hjust=1,vjust=1))


## 프로야구

kbo <- read.csv("주요선수별성적-2013년.csv",header=T)
(kbo <- kbo %>%
  mutate(출루율=출루율/연봉*100) %>%
  mutate(타율=타율/연봉*100)) 
             
kbo <- kbo[,c(2,11,12,13,14,17,19,7,8)]
re_kbo <- melt(kbo,id=c("선수명"),variable.name="종류",value.name="성적")
re_kbo <- arrange(re_kbo,선수명,종류)

# 기준점이 서로 다른 변수들을 각각 표준화하여
# 상대적인 위치를 알기 용이하게 한다...
kbo_scale <- kbo 
for (i in 2:9) {
  kbo_scale[,i] <- scale(kbo_scale[,i])
}
re_kbo_scale <- melt(kbo_scale,id=c("선수명"),variable.name="종류",value.name="성적")
re_kbo_scale <- arrange(re_kbo_scale,선수명,종류)

# barplot by player (scale version)
ggplot(re_kbo_scale,aes(x="",y=성적,fill=종류)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~ 선수명) + 
  geom_hline(yintercept=0,lty="dashed",size=0.1) +
  xlab("") + ylab("") +
  ggtitle("야구 선수별 주요 성적 분석 - 2013년 (표준화)") +
  theme(plot.title=element_text(color="darkblue",size=16,
                                face="bold",hjust=0.5))

# pie chart by player
ggplot(re_kbo,aes(x="",y=성적,fill=종류)) +
  facet_wrap(~ 선수명) +
  geom_bar(stat="identity") +
  coord_polar(theta="y") +
  xlab("") + ylab("") +
  ggtitle("야구 선수별 주요 성적 분석 - 2013년") +
  theme(plot.title=element_text(color="darkblue",size=16,
    face="bold",hjust=0.5))


  

  

  



             
