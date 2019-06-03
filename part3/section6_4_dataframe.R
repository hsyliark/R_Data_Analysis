#DataFrame

no <- c(1:4)
name <- c('Apple','Peach','Banana','Grape')
price <- c(500,200,100,50)
Qty <- c(5,2,4,7)
sales <- data.frame(NO=no,NAME=name,PRICE=price,QTY=Qty)
sales

sales2 <- matrix(c(1,'Apple',500,5,
                   2,'Peach',200,2,
                   3,'Banana',100,4,
                   4,'Grape',50,7),nrow=4,byrow=T)
sales2
df1<-data.frame(sales2)
names(df1) <- c('NO','NAME','PRICE','QTY')
df1

sales$NAME                  #NAME columns를 가져옴

sales[1,3]
sales[1,]
sales[,3]


sales[c(1,2),]
sales[,c(1,2)]
sales[,c(1:3)]

subset(sales,Qty<5)         #subset()함수 사용 예시
subset(sales,price==200)
subset(sales,price!=500)
subset(sales,name=='Apple')
subset(sales,name=='Grape')


no <- c(1:3)
name <- c('apple','banana','peach')
price <- c(100,200,300)
df1 <- data.frame(NO=no,NAME=name,PRICE=price)
df1
no<-seq(10,30,10)
no
name <- c('train','car','airplane')
price <- seq(1000,3000,1000)
df2 <- data.frame(NO=no,NAME=name,PRICE=price)
df2

df3<- cbind(df1,df2)    # columns 방향 합치기

df3$NAME                #앞에 칼럼만 나옴..
names(df3)

df4 <- rbind(df1,df2)   # row 방향 합치기
df4

df5 <-data.frame(name=c('apple','banana','cherry'),price=seq(300,100,-100))
df6 <-data.frame(name=c('apple','cherry','berry'),qty=seq(10,30,10))

df5
df6

merge(df5,df6)          #df5와 df6 의 inner join
merge(df5,df6,all=T)    #df5, df6 outer join
rbind(df5,df6)          #columns name이 서로 맞지 않아 rbind 불가능
cbind(df5,df6)
cbind(df6,df5)

new <- data.frame(name='mango', price=400)
df5 <- rbind(df5,new)
df5


df5 <- rbind(df5,data.frame(name='berry', price=500))
df5

df5 <- rbind(df5,data.frame(name='watermelon', price=1000))
df5
df5 <- cbind(df5, data.frame(qty=seq(10, nrow(df5)*10,10)))
df5




no <- seq(1,5,1)
name <- c('신은총','주시현','박진원','박진원2','윤정웅')
address <- c('익산','대전','전주','경주','경기')
tel <- seq(1111,5555,1111)
hobby <- c('독서','미술','놀고먹기','먹고놀기','노는애감시하기')
member <-data.frame(NO=no,NAME=name,ADDRESS=address,TEL=tel,HOBBY=hobby)
member

member2 <- subset(member,select=c(NO,NAME,TEL))          #특정 컬럼만 지정
member2

member3 <- subset(member,select=-TEL)                    #특정 컬럼만 제외
member3
colnames(member3) <- c('번호','이름','주소','취미')      #컬럼이름 변경
member3

rownames(sales) <- c('one','two','three','four')
ncol(sales)     #컬럼의 갯수
nrow(sales)     #열의 갯수
names(sales)    #컬럼의 이름
rownames(sales) #행의 이름
colnames(sales) #컬럼의 이름
sales[c(2,3,1),] #보는 순서 바꿔줌 (row의 배치)
sales[,c(2,3,1)] #보는순서 바꿔줌  (column의 배치)
sales

