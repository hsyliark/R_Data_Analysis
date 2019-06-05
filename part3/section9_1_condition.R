## 조건문

my.abs <- function(x) {
  if (x >= 0) {
    return(x)
  } else {
      return(-x)
    }
}
my.abs(-3.2)
my.abs(4.5)

mf1 <- function(x) {
  if (x > 0) {
    return(x^2)
  } else {
    return(0)
  }
}
mf1(4.6)
mf1(-8.7)

mf2 <- function(x) {
  if (x > 0) {
    return(x*2)
  } else if (x == 0) {
    return(0) }
  else {
    return(-2*x)
  }
}
mf2(8)
mf2(-4.7)

no <- scan()
10
ifelse(no%%2==0,'짝수','홀수')

# Q1
myf1 <- function(x) {
  if (x > 5) {
    return(1)
  } else {
    return(0)
  }
}
myf1(7)
myf1(1)

# Q2
myf2 <- function(x) {
  if (x > 0) {
    return(1)
  } else {
    return(0)
  }
}
myf2(350)
myf2(-23)

# Q3
myf3 <- function(x,y) {
  if (x >= y) {
    return(x-y) }
  else {
    return(y-x) }
}
myf3(4,3)
myf3(4,4)
myf3(2,5)

# Q4
myf4 <- function(x) {
  if (x <= 1) {
    return(0)
  } else if (x > 1 && x < 5) {
    return(1)
  } else {
    return(10)
  }
}

# Q5
my.ftn <- function(ent) {
  if (ent %in% c('Y','y')) {
    return('Yes')
  } else {
    return('Not Yes')
  }
}
my.ftn('y')
my.ftn('a')

myRealD <- function(a,b,c) {
  D = b^2 - 4*a*c
  if ( abs(D) < 1e-10 ) {
    return(1)
  } else if (D > 0) {
    return(2)
  } else {
    return(0)
  }
}
