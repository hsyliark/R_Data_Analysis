#벡터형

c(1,2,3,4,5)

c(1,2,3,4,'5') # 마지막요소가 문자라서 모두 문자로 변환되었음.

vec1 <- c(1,2,3,4,5)
vec1

#CRUD

vec1[3] # 3번째 요소 값만 보여줌
vec1[-3] # -를 부틸경우 3번째 만 빼고 보어줌
vec1[1:(length(vec1)-2)] # vec1의 총 길이에서 2개를 뺀 갯수만큼 출력
length(vec1) # vec1의 길이를 출력함
vec1[-1:-3] #1부터 3까지 요소를 빼고 보여줌

vec1[2]
vec1[2] <- 6 # 2번 항목을 6으로 변경
vec1

vec1 <- c(vec1,7) #벡터에 새로운 내용 추가
vec1

vec1[9] <- 9 
vec1 #NULL이 아닌 NA가 추가됨

append(vec1,10,after=3) #3위치 뒤에뒤에 10을 넣는다
append(vec1,c(10,11),after=3) #3위치 뒤에 10,11를 넣는다
append(vec1,11,after=0) #0은 가장 앞자리



#벡터의 연산

c(1,2,3) + c(4,5,6)
c(1,2,3) + 1

var1 <- c(1,2,3)
var2 <- c(3,4,5)

var1 + var2

var3 <- c('3','4',5)
var3 #문자열 벡터
var1 +var3  #error 숫자+문자

union(var1,var3) #합집합.중복되는 값은 제거 데이터형이 다를경우에 사용가능

var4 <- c(1,2,3,4,5)
var1; var4
union(var1,var4)
var1 + var4 #경고가 나오며 실행

#var1 : 1 2 3  --> 1 2 3 1 2
#var4 : 1 2 3 4 5 --> 1 2 3 4 5
# 두 벡터의 길이가 다를 경우 순환 원리가 적용됨.


#뺄셈

var1 - var2 #벡터간 뺄셈

setdiff(var1,var2) #var1에 있는데 var2에 없는 요소 출력
setdiff(var2,var1) #var2에 있는데 var1에 없는 요소 출력력

intersect(var1,var2) #두 변수의 공통적인 요소 찾기

fruits <- c(10,20,30)
fruits
names(fruits) <- c('apple','banana','peach') #이름 지정
fruits
fruits['apple']


#sep(), rep()

var5 <- seq(1,5); var5
var6 <- seq(2,-2); var6
var7 <- seq(1,10,2); var7 # 2씩 증가하면서 값을 할당 : seq(시작, 끝, 증분)

var8 <- rep(1:3,2); var8 # 2회 repeat
var9 <- rep(1:3, each=2); var9

var7
length(var7)
NROW(var7)
3 %in% var7 # var7안에 3이 있는지 확인
4 %in% var7 # 결과는 TRUE FALSE로 나옴

rm(list=ls())
