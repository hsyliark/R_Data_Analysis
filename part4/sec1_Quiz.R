setwd("D:/Workplace/R_Data_Analysis")


# Q1

View(iris)
attach(iris)
iris.split <- split(iris, Species)
setosa <- iris.split$setosa
versicolor <- iris.split$versicolor
virginica <- iris.split$virginica

par(oma=rep(1.5,4),mfrow=c(3,2))
plot(setosa$Sepal.Length,setosa$Sepal.Width,type="p",
     col="red",xlab="Length",ylab="Width",main="Sepal of setosa")
plot(setosa$Petal.Length,setosa$Petal.Width,type="p",
     col="blue",xlab="Length",ylab="Width",main="Petal of setosa")
plot(versicolor$Sepal.Length,versicolor$Sepal.Width,type="p",
     col="orange",xlab="Length",ylab="Width",main="Sepal of versicolor")
plot(versicolor$Petal.Length,versicolor$Petal.Width,type="p",
     col="green",xlab="Length",ylab="Width",main="Petal of versicolor")
plot(virginica$Sepal.Length,virginica$Sepal.Width,type="p",
     col="brown",xlab="Length",ylab="Width",main="Sepal of virginica")
plot(virginica$Petal.Length,virginica$Petal.Width,type="p",
     col="purple",xlab="Length",ylab="Width",main="Petal of virginica")


# Q2

par(mfrow=c(1,1))
iris.mean <- matrix(c(setosa$Sepal.Length,setosa$Sepal.Width,setosa$Petal.Length,setosa$Petal.Width,
                      versicolor$Sepal.Length,versicolor$Sepal.Width,versicolor$Petal.Length,versicolor$Petal.Width,
                      virginica$Sepal.Length,virginica$Sepal.Width,virginica$Petal.Length,virginica$Petal.Width),4,3)
colnames(iris.mean) <- c('setosa','versicolor','virginica') 
barplot(iris.mean,main="Compare mean by Species ver1",
        beside=T,col=rainbow(nrow(iris.mean)),ylim=c(0,10),ylab='average')
legend(10,10,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
       cex=1,fill=rainbow(nrow(iris.mean)))
barplot(iris.mean,main="Compare mean by Species ver2",
        col=rainbow(nrow(iris.mean)),xlab='average',,xlim=c(0,30),horiz=T)
legend(22,3,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
       cex=1,fill=rainbow(nrow(iris.mean)))


# Q3

setosa <- setosa[,1:4]
versicolor <- versicolor[,1:4]
virginica <- virginica[,1:4]
par(oma=rep(0.5,4),mfrow=c(1,3))
boxplot(setosa,col=c("red","blue","yellow","green"),main="Boxplot of setosa")
boxplot(versicolor,col=c("red","blue","yellow","green"),main="Boxplot of versicolor")
boxplot(virginica,col=c("red","blue","yellow","green"),main="Boxplot of virginica")
