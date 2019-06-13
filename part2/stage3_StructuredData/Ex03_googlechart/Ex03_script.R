### Ex 3-3, 구글차트

setwd("D:/Workplace/R_Data_Analysis/part2/stage3_StructuredData/Ex03_googlechart")

library(googleVis)
data1 <- read.csv("2013년_서울_구별_주요과목별병원현황_구글용.csv",header=T)
data1

hos <- gvisColumnChart(data1,options=list(title="지역별 병원현황",height=400,weight=500))

header <- hos$html$header
header <- gsub('charset=utf-8','charset=euc-kr',header)
header
hos$html$header <- header

plot(hos)
