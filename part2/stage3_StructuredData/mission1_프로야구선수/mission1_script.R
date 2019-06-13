### 프로야구선수

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/mission1_프로야구선수")

kbo <- read.csv("주요선수별성적-2013년.csv",header=T)
View(kbo)

library(dplyr)
kbo <- kbo %>%
  mutate(OPS=출루율+장타율) %>%
  mutate(성과율=OPS/연봉*100) %>%
  mutate(성과율퍼센트=paste(round(성과율,1),"%"))
soc <- round(mean(kbo$성과율)) 

str(kbo) 
library(ggplot2) 
library(RColorBrewer) 
palete <- c(rep(brewer.pal(12,"Paired"),2),"#56B4E9") 
ggplot(kbo,aes(x=선수명,y=성과율)) +
  geom_bar(stat="identity",col=palete,fill=palete) +
  geom_text(aes(y=성과율,label=성과율퍼센트),colour="black",size=4) +
  geom_text(aes(x=1.8,y=round(soc,1)+1,
                label=paste(soc,"%","(평균성과율)")),colour="brown",size=5) +
  geom_hline(yintercept=soc,lty="dashed",col="brown",size=0.1) +
  ggtitle("야구선수별 연봉 대비 성과율") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))
  
  
