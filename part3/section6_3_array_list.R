#Array

array1 <- array(c(1:12), dim=c(4,3), byrow=T); array1

array2 <- array(c(1:12), dim=c(2,2,3))
array2

array3 <- array(c(1:12), dim=c(2,2,5))
array3

array4 <- array(c(1,16), dim=c(2,2,4))

array2[1,1,3] #[열,행,층]
length(array2)

#List
list1 <- list(name='eunchong', address = 'Iksan', tel ='010-****-3702', ages ='31')
list1
list1$name
list1[1:2]

list1$birth <- '1989-03-24' #list 추가
list1

list1$name <- c('eunchong','JWP') #하나의 key에 두개의 value 동시에넣기

list1$birth <- NULL #birth key 삭제
list1

rm(list=objects(all.names = T))
