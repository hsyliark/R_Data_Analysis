#변수

var <- c("a","b","c"); var #여러건의 데이터 한꺼번에 담기

# 일반적인 laguage : l-value(변수) = r-value(값)
# R에서는 좌우가 상관없음

111 -> var1 -> var2; var1; var2;
var3 <- var4 <- 222; var3; var4

string1 <- "Very Easy R Programming"
string2 <- "I'm James Seo" # "와 '의 구분을 명확히 해야함
string3 <- "He said, \"I am single\"."; string3 # \를 통해 구분 가능

comp <- c(1,'2'); comp #둘다 문자열로 나옴
class(comp)

num1 <- 1
num2 <- 2
num1 + num2 #변수 값을 이용한 산술 연산

char1 <- 'a'

num1 +char1 # 숫자+ 문자 연산 --> error

#변수에 연속적인 값 대입하기

seq1 <- 1:5; seq1
seq2 <- "a":"f" #문자는 연속적인 할당이 안됨.

date1 <- seq(from=as.Date('2019-05-01'),to=as.Date('2019-05-31'), by=1);date1
class(date1)
date2 <- seq(from=as.Date('2019-01-01'),to=as.Date('2019-05-31'), by='month');date2
date3 <- seq(from=as.Date('2000-01-01'),to=as.Date('2019-05-31'), by='year');date3
date4 <- seq(from=as.Date('2019-05-01'),to=as.Date('2019-05-31'), by='day');date4
date5 <- seq(from=as.Date('2019-05-01'),to=as.Date('2019-05-31'), by=3);date5 #3일 간격


date6 <- seq(from=as.POSIXct("2014-11-01 20:00:00"), to=as.POSIXct("2014-11-01 23:59:59"), by='min');date6 #분단위
date7 <- seq(from=as.POSIXct("2014-11-01 20:00:00"), to=as.POSIXct("2014-11-01 23:59:59"), by='hour');date7 #시간단위
date8 <- seq(from=as.POSIXct("2014-11-01 20:00:00"), to=as.POSIXct("2014-11-01 23:59:59"), by='sec');date8 #초단위 max:13400



.hidden <- 3
.hidden

objects() #hidden변수 제외 (.으로 시작하는 변수)
objects(all.names=T) # 생성한 모든 변수 확인. Hidden 변수까지

rm(char1) #변수 지우기
ls()
rm(list = objects()) # ls()==objects()
objects(all.names=T)
