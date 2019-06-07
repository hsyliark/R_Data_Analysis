# 그래프 기초

setwd("D:/Workplace/R_Data_Analysis/part4")
var1 <- 1:5
plot(var1)
var1 <- 5:1
plot(var1)
var2 <- rep(2,3)
plot(var2)
x <- 1:3
y <- 4:2
plot(x,y)
x <- 1:3
y <- 3:1
plot(x,y,xlim=c(0,10),ylim=c(0,5),
     xlab="x value",ylab="y value",main="Plot test")

v1 <- c(100,130,120,160,150)
plot(v1,type='o',col='red',ylim=c(0,200),axes=F,ann=F)
axis(1,at=1:5,lab=c('Mon','Tue','Wed','Thu','Fri'))
axis(2,ylim=c(0,200))
title(main='FRUIT',col.main='green',font.main=4)
title(xlab='Day',col.lab='black')
title(ylab='Price',col.lab='blue')

v1
par(mfrow=c(1,3))
plot(v1,type='o')
plot(v1,type='s')
plot(v1,type='l')
v1
par(mfrow=c(1,3))
pie(v1)
plot(v1,type='o')
barplot(v1)

a <- 1:3
par(mfrow=c(1,1))
plot(a,xlab='aaa')
par(mgp=c(0,1,0))
plot(a,xlab='aaa')
par(mgp=c(3,1,0))
plot(a,xlab='aaa')
par(mgp=c(3,2,0))
plot(a,xlab='aaa')
par(mgp=c(3,2,1))
plot(a,xlab='aaa')
par(oma=c(2,1,0,0))
plot(a,xlab='aaa')
par(oma=c(0,2,0,0))
plot(a,xlab='aaa')

v1 <- 1:5 ; v2 <- 5:1 ; v3 <- 3:7
plot(v1,type='s',col='red',ylim=c(1,5))
par(new=T)
plot(v2,type='o',col='blue',ylim=c(1,5))
par(new=T)
plot(v3,type='l',col='green')

par(new=F)
plot(v1,type='s',col='red',ylim=c(1,10))
lines(v2,type='o',col='blue',ylim=c(1,5))
lines(v3,type='l',col='green',ylim=c(1,15))
legend(4,9,c('v1','v2','v3'),cex=0.9,col=c('red','blue','green'),lty=1)

x <- 1:5
barplot(x)
barplot(x,horiz=T)

x <- matrix(c(5,4,3,2),2,2)
x
barplot(x,beside=T,names=c(5,3),col=c('green','yellow'))
barplot(x,names=c(5,3),col=c('green','yellow'),ylim=c(0,12))
par(oma=c(1,0,2,0))
barplot(x,names=c(5,3),beside=T,col=c('green','yellow'),horiz=T)
barplot(x,names=c(5,3),col=c('green','yellow'),xlim=c(0,12),horiz=T)

v1 <- seq(100,180,20) ; v2 <- c(120,130,150,140,170) ; v3 <- c(140,170,120,110,160)
qty <- data.frame(banana=v1,cherry=v2,orange=v3)
qty
barplot(as.matrix(qty),main="Fruit's Sales QTY",
        beside=T,col=rainbow(nrow(qty)),ylim=c(0,400))
legend(14,400,c('Mon','Tue','Wed','Thu','Fri'),cex=0.8,fill=rainbow(nrow(qty)))
barplot(t(qty),main="Fruit's Sales QTY",ylim=c(0,900),
        col=rainbow(length(qty)),space=0.1,cex.axis=0.8,las=1,
        names.arg=c('Mon','Tue','Wed','Thu','Fri'),cex=0.8)
legend(0.2,800,names(qty),cex=0.7,fill=rainbow(length(qty)))

peach <- c(180,200,250,198,170)
color <- c()
for (i in 1:length(peach)) {
  if (peach[i] >= 200) {
    color <- c(color,'red') }
  else if (peach[i] >= 180) {
    color <- c(color,'yellow') }
  else {
    color <- c(color,'green') }
}
barplot(peach,main="peach Sales QTY",
        names.arg=c('Mon','Tue','Wed','Thu','Fri'),col=color)

height <- c(182,175,167,172,163,178,181,166,159,155)
hist(height,main="histogram og height")
par(mfrow=c(1,2),oma=c(2,2,0.1,0.1))
y <- c(1,1,2,3,3,3)
hist(y)
plot(y,main='plot')

par(mfrow=c(1,1),oma=c(0.5,0.5,0.1,0.1))
p1 <- seq(10,40,10)
pie(p1,radius=1)
pie(p1,redius=1,init.angle=90)
pie(p1,redius=1,init.angle=90,col=rainbow(length(p1)),
    label=c('week1','week2','week3','week4'))

pct <- round(p1/sum(p1)*100,1)
lab <- paste(pct,"%")
pie(pct,radius=1,init.angle=90,col=rainbow(length(pct)),
    label=lab)
legend(1,1.1,c('week1','week2','week3','week4'),
       cex=0.5,fill=rainbow(length(pct)))
lab1 <- c('week1','week2','week3','week4')
lab2 <- paste(lab1,'\n',pct,' %')
pie(p1,radius=1,init.angle=90,col=rainbow(length(p1)),label=lab2)

install.packages("plotrix")
library(plotrix)
p1 <- seq(10,50,10)
f_day <- round(p1/sum(p1)*100,1)
f_label <- paste(f_day,"%")
pie3D(p1,main="3D Pie Chart",col=rainbow(length(p1)),
      cex=0.5,labels=f_label,explode=0.05)
legend(0.5,1,c('Mon','Tue','Wed','Thu','Fri'),cex=0.6,
       fill=rainbow(length(p1)))

v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)
boxplot(v1,v2,v3)
boxplot(v1,v2,v3,col=c('blue','yellow','pink'),
        names=c('blue','yellow','pink'),
        horizontal=T)
