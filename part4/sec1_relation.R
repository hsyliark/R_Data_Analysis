# 관계도

install.packages('igraph')
library(igraph)
g1 <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(g1)
str(g1)
name <- c('서진수 대표이사','일지매 부장','김유신 과장','손흥민 대리','노정호 대리',
          '이순신 부장','유관순 과장','신사임당 대리','강감찬 부장','광개토 과장','정몽주 대리')
pemp <- c('서진수 대표이사','서진수 대표이사','일지매 부장','김유신 과장','김유신 과장',
          '서진수 대표이사','이순신 부장','유관순 과장','서진수 대표이사','강감찬 부장','광개토 과장')
emp <- data.frame(이름=name,상사이름=pemp)
emp
g <- graph.data.frame(emp,directed=T)
plot(g,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5)
str(g)
png("graph/network_2.png", width=600, height=600)
plot(g,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5)
dev.off()
g1 <- graph.data.frame(emp,directed=F)
plot(g1,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5,
     vertex.label=NA)

install.packages('devtools')
library(devtools)
install.packages('d3Network')
library(d3Network)
install.packages('RCurl')
library('RCurl')
name <- c('Angela Bassett','Jessica Lange','Winona Ryder','Michelle Pfeiffer',
          'Whoopi Goldberg','Emma Thompson','Julia Roberts','Sharon Stone','Meryl Streep',
          'Susan Sarandon','Nicole Kidman')
pemp <- c('Angela Bassett','Angela Bassett','Jessica Lange','Winona Ryder','Winona Ryder',
          'Angela Bassett','Emma Thompson','Julia Roberts','Angela Bassett',
          'Meryl Streep','Susan Sarandon')
emp <- data.frame(이름=name,상사이름=pemp)
emp
d3SimpleNetwork(emp,width=600,height=600,file='graph/d3.html')

g <- read.csv("data/군집분석.csv", head=T)
graph1 <- data.frame(학생=g$학생, 교수=g$교수)
g <- graph.data.frame(graph1,directed=T)
plot(g,layout=layout.fruchterman.reingold,vertex.size=2,edge.arrow.size=0.5,
     vertex.color='green',vertex.label=NA)
plot(g,layout=layout.kamada.kawai,vertex.size=2,edge.arrow.size=0.5,
     vertex.color='green',vertex.label=NA)
g
library(stringr)
V(g)$name
gubun1 <- V(g)$name
gubun1
gubun <- str_sub(gubun1,start=1,end=1)
gubun
color <- c()
for (i in 1:length(gubun)) {
  if (gubun[i]=='S') {
    color <- c(color,'red') }
  else {
    color <- c(color,'green') }
}
size <- c()
for (i in 1:length(gubun)) {
  if (gubun[i]=='S') {
    size <- c(size,2) }
  else {
    size <- c(size,6) }
}
plot(g,layout=layout.fruchterman.reingold,vertex.size=size,
     edge.arrow.size=0.5,vertex.color=color)
plot(g,layout=layout.fruchterman.reingold,vertex.size=size,
     edge.arrow.size=0.5,vertex.color=color,vertex.label=NA)
plot(g,layout=layout.fruchterman.reingold,vertex.size=size,edge.arrow.size=0,
     edge.arrow.size=0.5,vertex.color=color,vertex.label=NA)
plot(g,layout=layout.kamada.kawai,vertex.size=size,edge.arrow.size=0,
     edge.arrow.size=0.5,vertex.color=color,vertex.label=NA)
shape <- c()
for (i in 1:length(gubun)) {
  if (gubun[i]=='S') {
    shape <- c(shape,'circle') }
  else {
    shape <- c(shape,'square') }
}
plot(g,layout=layout.kamada.kawai,vertex.size=size,edge.arrow.size=0,
     edge.arrow.size=0.5,vertex.color=color,vertex.label=NA,vertex.shape=shape)

virus1 <- read.csv("data/메르스전염현황.csv",header=T)
d3SimpleNetwork(virus1,width=2000,height=1000,file="graph/mers.html")
