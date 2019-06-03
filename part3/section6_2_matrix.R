#Matrix

mat1 <- matrix(c(1,2,3,4)) # nrow의 디폴트 값은 1
mat1

mat2 <- matrix(c(1,2,3,4),nrow=2) # 열 우선으로 입력됨
mat2

mat3 <- matrix(c(1,2,3,4),nrow=2,byrow=T); mat3 # byrow=T를 통해 행 우선으로 입력됨
mat3[,1]    #모든행의 1열 값을 출력
mat3[1,]    #1행의 모든 열값을 출력
mat3[1,1]   # 1행 1열의 값 출력


mat4 <- matrix(seq(1:9),nrow=3,byrow=T)
mat4

mat4 <- rbind(mat4,c(11,12,13)) #마지막행에 추가
mat4 <- rbind(mat4,c(14,15,16,17)) #길이가 다를경우 에러가남

mat5 <- matrix(c('a','b','c','d'),nrow=2,byrow=T)
mat5

mat5 <- cbind(mat5,c('e','f')) #마지막에 컬럼 추가
mat5

colnames(mat5) <- c('First','Second','Third'); mat5 # Columns name 지정
rownames(mat5) <- c('하나','둘'); mat5 # row name 지정
mat5['하나',]
mat5[,'First']
mat5['하나','First']
mat5[1,]
mat5[,1]
mat5[1,1]


#Practice p328 -p329

#1
seasons0 <- matrix(c('봄','여름','가을','겨울'), nrow=2)
seasons0

seasons1 <- matrix(c('봄','여름','가을','겨울'), nrow=2,byrow=T)
seasons1


#2
seasons1[,2]

#3

seasons2 <- rbind(seasons1,c('초봄','초가을'))
seasons2

#4

seasons3 <- cbind(seasons2,c('초여름','초겨울','한겨울'))
seasons3


seasons3 <- seasons3[,-3]
seasons3 <- seasons3[-3,]
seasons3


rm(list=ls())
