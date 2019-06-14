### 지도 시각화

install.packages("ggiraphExtra")
install.packages('maps')
library(ggiraphExtra)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(extrafont)
windowsFonts(malgun="맑은 고딕")
theme_update(text=element_text(family="malgun"))


## 미국 강력범죄

str(USArrests)
head(USArrests)
tail(USArrests)
summary(USArrests)

library(tibble)

crime <- rownames_to_column(USArrests,var="state")
head(crime)
crime$state <- tolower(crime$state)

# 지도 데이터
library(maps)
states_map <- map_data("state")
str(states_map)

# 단계 구분도
install.packages('mapproj')
library(mapproj)

library(ggplot2)
library(ggiraphExtra)
m <- ggChoropleth(data=crime,aes(fill=Murder,map_id=state),map=states_map) +
  labs(title = "미국 주별 살인범죄 분포",
       subtitle = "(단위 : 인구 10만명당 건수)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))
a <- ggChoropleth(data=crime,aes(fill=Assault,map_id=state),map=states_map) +
  labs(title = "미국 주별 강도범죄 분포",
       subtitle = "(단위 : 인구 10만명당 건수)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))
u <- ggChoropleth(data=crime,aes(fill=UrbanPop,map_id=state),map=states_map) +
  labs(title = "미국 주별 거주인구 분포",
       subtitle = "(단위 : 인구 10만명당 건수)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))
r <- ggChoropleth(data=crime,aes(fill=Rape,map_id=state),map=states_map) +
  labs(title = "미국 주별 강간범죄 분포",
       subtitle = "(단위 : 인구 10만명당 건수)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))

library(gridExtra)
# windows()
grid.arrange(m,r,a,u,ncol=2,top="미국 주별 강력범죄 분포")

ggChoropleth(data=crime,aes(fill=Rape,map_id=state),map=states_map,
             interactive=T) # interactive 


## 대한민국 시도별 인구, 결핵 환자수

library(stringi)
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

# 시도별 인구데이터
str(changeCode(korpop1))
library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명 ,
                  name = 행정구역별_읍면동)
str(changeCode(korpop1))

library(ggiraphExtra)
library(ggplot2)
ggChoropleth(data=korpop1,aes(fill=pop,map_id=code),
             map=kormap1,interactive=T)

# 시도별 결핵환자수
str(changeCode(tbc))
ggChoropleth(data=tbc,aes(fill=NewPts,map_id=code),
             map=kormap1,interactive=T)

# graph
options(scipen=6)
a <- ggChoropleth(data=korpop1,aes(fill=pop,map_id=code),
             map=kormap1) +
  labs(title = "대한민국 시도별 인구수",
       subtitle = "(2014년)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))
b <- ggChoropleth(data=tbc,aes(fill=NewPts,map_id=code),
             map=kormap1) +
  labs(title = "대한민국 시도별 결핵환자수",
       subtitle = "(2014년)",
       x = "경도", y = "위도") +
  theme(plot.title = element_text(color="darkgreen",size=20,face="bold.italic",hjust=0.5),
        plot.subtitle = element_text(color="black",size=15,face="bold.italic",hjust=0.7),
        axis.title.x = element_text(color="blue",size=13,face="bold"),
        axis.title.y = element_text(color="red",size=13,face="bold"),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1))

library(gridExtra)
# windows()
grid.arrange(a,b,ncol=2,top="대한민국 시도별 인구 및 결핵환자수")
