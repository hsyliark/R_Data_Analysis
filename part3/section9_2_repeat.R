## 반복문

no <- 0
while (no < 5) {
  print(no)
  if (no == 3) break
  no <- no+1 
}

x <- -1
while (x <= 10) {
  x <- x+1
  if (x%%2==1) next
  print(x)
}

y <- 0
while (y <= 10) {
  y <- y+1
  if (10%%y==0) 
    print(y)
}

for (i in 1:10) {
  print(i)
}

x <- 0
for (i in 1:100) {
  x <- x + i
}
print(x)

y <- 0
for (i in 1:100) {
  if ( i%%3 %in% c(1,2) ) next
  y <- y + i
}
print(y)

my.multi <- function(lim, num) {
  x <- 0
  for (i in 1:lim) {
    if (i %% num == 0) {
      x <- x + i
    }
  }
  return(x)
}

my.multi(1000, 4)

library(stringr)

while (T) {
for (i in 1:4) {
  line <- ''
  for (k in 1:i) {
    line <- str_c(line, '#')
  }
  print(line)
}
for (i in 1:5) {
  line <- ''
  if (i != 1) {
    for (k in 1:(i-1))
      line <- str_c(line, ' ')
  }
  for (k in i:5) {
    line <- str_c(line, '#')
  }
  print(line)
}
  break;
}

# 약수 구하는 함수
my.dim <- function(x) {
  den <- c(1)
  if (x >= 2) {
    for (i in 2:x) {
      if (x %% i == 0)
        den <- c(den, i)
    }
  }
  return(den)
}
my.dim(60)

my.div <- function(n) {
  y <- 0
  while (y <= n) {
    y <- y+1
    if (n%%y==0) 
      print(y)
  }
}
my.div(120)
my.div(250)

# Diamond 그리기
library(stringr)
while (T) {
  for (i in 1:3) {
    line <- ''
    for (k in 1:(4-i)) {
      line <- str_c(line, ' ')
    }
    for (k in 1:(2*i-1)) {
      line <- str_c(line, '#')
    }
    for (k in 1:(4-i)) {
      line <- str_c(line, ' ')
    }
    print(line)
  }
  for (i in 1:4) {
    line <- ''
    if (i != 1) {
      for (k in 1:(i-1))
        line <- str_c(line, ' ')
    }
    for (k in 1:(9-2*i)) {
      line <- str_c(line, '#')
    }
    if (i != 1) {
      for (k in 1:(i-1))
        line <- str_c(line, ' ')
    }
    print(line)
  }
  break;
}

# 구구단 출력
for (i in 2:9) {
  line <- str_c(i, '단')
  print(line)
  for (k in 1:9) {
    line <- str_c(i,' * ',k,' = ',i*k)
    print(line)
  }
}

# for loop

x <- 0
for (i in seq(3,100,3)) {
  x <- x + i
}
print(x)

for (i in seq(1,0,1)) {
  print(i)
}

for (i in c(1,3,5,7,9)) {
  print(i)
}

