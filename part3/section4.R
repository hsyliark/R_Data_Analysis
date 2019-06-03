
# Data Type
# 문자와 정수와 실수등을 해석하는 방법이 각각 다르다.

print(5/2)
print(5%/%2) # 나눗셈의 몫
print(5%%2) # 나눗셈의 나머지
print(5^2) # 승수구하기1
print(5**2)  # 승수구하기2

10000
100000 # 여기서부터 10의 승수로 표현
1000000
100000+100000
1/1000000
1/10000 # 여기서 부터 10의 승수로 표현
1/1000 


'1' + '2' # non-numeric argument to binary operator

as.numeric('1') + as.numeric('2') #문자열을 숫자로 바꾸어줌 (문자열이 수의 형태일 경우)
as.numeric('Hello') + as.numeric('World') # Not working

as.character(1) # 숫자를 문자형으로 바꾸어줌

#문자형

'First' # 문자라서 홀따옴표
"Second" # 쌍따옴표도 가능
Third # 변수 이름으로 인식되어 에러 발생

Third <- 1 # <- 연산자와 = 연산자의 차이는 <-가 = 보다 우선순위가 높다
Third


# 데이터의 형태를 검사해줌
class('1') # character
class(1) # numeric


3&0 # 3곱하기0의 뜻 : False
3&1 # 3곱하기1의 뜻 : True
3|0 # 참더하기 거짓 : True
!0 # 거짓이 아닌것 : True
!1  # 참이 아닌것 : False
class(!0) # type : logical type



# NA : Not Applicable, Not Available
# Null : 값이 없을 경우

cat(1,NA,2) #NA 그대로출력
cat(1,NULL,2) # NULL 값 제거되고 출력

sum(1,NA,2) #NA를 더하니까 NA로 출력
sum(1,NULL,2) # NULL 값은 제외해버리고 나머지 값만 더함
sum(1,2,NA,na.rm=T) #NA값을 제거하고 올바른 계산을 함
class(NA) #logical
class(NULL) #NULL

setwd('d:/Workplace/part3/data')
txt1 <- read.csv('factor_test.csv') # csv 파일 불러오기
class(txt1) #type : data.frame
factor1 <- factor(txt1$blood)
factor1
summary(factor1)
sex1<-factor(txt1$sex)
sex1
summary(sex1)
summary(txt1)
View(txt1)

stringAsFactors=FALSE #TRUE : 범주형, FALSE : 문자열

Sys.Date() # 오늘 날짜를 알려줌, 대문자 주의
Sys.time() # 오늘 날짜와 시간까지 알려줌.
date() # 영어로 날짜와 시간을 보여줌

class(Sys.Date()) # Date type
class(Sys.time()) # POSIXct, POSIXt type : ??

# 날짜(Date): Date 클래스
# 시간(time): POSIXct, POSIXlt 클래스
# POSIXct 클래스는 매우 큰 정수로 시간정보를 데이터프레임으로 저장할 때 유용하다.
# POSIXlt 클래스는 리스트 자료형으로 요일, 년, 월, 일 등의 정보를 리스트 내부 원소로 저장되어 유용하다.

class(date()) # Character


as.Date("2019-05-30") # 문자형태로 저장된 날짜를 날짜타입으로 변경
as.Date("2019/05/30")
class(as.Date("2019-05-30")) # Date type

as.Date("01-11-2014") # 의도치 않은 날짜가 나옴...
as.Date("01-11-2014",format = "%d-%m-%Y") # 날짜 형태를 지정해야함
as.Date("19-05-30") # 의도치 않은 날짜가 나옴...
as.Date("19-05-30", format = "%Y-%m-%d") # 0019년이 됨
as.Date("19-05-30", format = "%y-%m-%d") # year를 소문자로하면 인식함
as.Date("2019년 5월 30일", format = "%Y년 %m월 %d일")
as.Date("05-30", format = "%m-%d")
as.Date("190530", format = "%y%m%d")

# %B와 %b 사용은??
as.Date("2019-05-30",format = "%Y-%b-%d") #??


#기준일자를 주고 날짜 찾기
as.Date(10, origin = "2019-05-30") #10일후 날짜가 나옴
as.Date(-10, origin = "2019-05-30") #10일전 날짜가 나옴
as.Date(20, origin=Sys.Date())


"2019-05-30" - "2019-05-27" # 문자열이기 때문에 연산이 불가함.
as.Date("2019-05-30") - as.Date("2019-05-27") # 날짜 타입으로 바꾸어 연산가능
as.Date("2019-11-14") - Sys.Date()
as.Date("2019-05-30 20:00:00") #날짜만 출력됨
as.POSIXct("2019-05-30 20:00:00") - as.POSIXct("2019-05-30 18:30:30") # UNIX 1970-01-01 에서 시작
0.4916667 *60

# install.packages("lubridate") # lubridate packages 설치

library(lubridate) #packages를 사용하겠다는 의미

now()
date <- now()
year(date) #년도만 출력하기
month(date) #월만 출력하기
month(date, label=T)
day(date)
wday(date,label=T)
wday(date,label=F)

date <- date - days(2); date #2일전 날짜 출력력
month(date) <- 2; date #월을 2월로 설정
date + years(1); #1년 추가하기
date + hours(1) #1시간 추가하기
date+seconds(1) #1초 추가하기
date+minutes(1) #1분 추가하기

date <- hm("22:30"); date #시간 분 지정하기
date <- hms("22:30:15"); date #시간 분 초 지정하기

