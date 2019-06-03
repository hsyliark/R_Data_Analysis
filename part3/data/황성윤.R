install.packages("ggplot2")
library(ggplot2)
attach(mpg)
mpg

library(plyr)
library(dplyr)

## Q1

displ <- as.numeric(displ)
hwy1 <- hwy[displ >= 5]
mean(hwy1) # 18.07895
hwy2 <- hwy[displ <= 4]
mean(hwy2) # 25.96319
# 배기량이 4 이하인 자동차의 연비가 더 좋음.

## Q2

mpg %>%
  group_by(manufacturer) %>%
  summarise(average = mean(cty, na.rm=T))
# 아우디 : 17.6, 토요타 : 18.5
# 토요타의 도시 연비가 평균적으로 더 좋음.

## Q3

mpg %>%
  filter(manufacturer %in% c("chevrolet", "ford", "honda")) %>%
  summarise(average=mean(hwy,na.rm=T)) 
# answer : 22.5

## Q4

mpg_1 <- select(mpg, class, cty)
head(mpg_1)
# A tibble: 6 x 2
# class     cty
# <chr>   <int>
# 1 compact    18
# 2 compact    21
# 3 compact    20
# 4 compact    21
# 5 compact    16
# 6 compact    18

## Q5

attach(mpg_1)
mean(cty[class=="suv"]) # 13.5
mean(cty[class=="compact"]) # 20.12766
# compact 자동차의 연비가 더 높음.

## Q6

mpg_2 <- filter(mpg,manufacturer=="audi")
attach(mpg_2) 
arrange(mpg_2,desc(hwy))[1:5,]
# A tibble: 5 x 11
# manufacturer model      displ  year   cyl trans      drv     cty   hwy fl    class  
# <chr>        <chr>      <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>  
# 1 audi         a4           2    2008     4 manual(m6) f        20    31 p     compact
# 2 audi         a4           2    2008     4 auto(av)   f        21    30 p     compact
# 3 audi         a4           1.8  1999     4 auto(l5)   f        18    29 p     compact
# 4 audi         a4           1.8  1999     4 manual(m5) f        21    29 p     compact
# 5 audi         a4 quattro   2    2008     4 manual(m6) 4        20    28 p     compact

## Q7

attach(mpg)
mpg$avy <- (cty+hwy)/2
arrange(mpg, desc(avy))[1:3,]
# A tibble: 3 x 12
# manufacturer model      displ  year   cyl trans      drv     cty   hwy fl    class        avy
# <chr>        <chr>      <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>      <dbl>
# 1 volkswagen   new beetle   1.9  1999     4 manual(m5) f        35    44 d     subcompact  39.5
# 2 volkswagen   jetta        1.9  1999     4 manual(m5) f        33    44 d     compact     38.5
# 3 volkswagen   new beetle   1.9  1999     4 auto(l4)   f        29    41 d     subcompact  35  

mpg %>%
  mutate(ctyhwy = cty + hwy) %>%
  mutate(avy = ctyhwy/2) %>%
  arrange(mpg, desc(avy))[1:3,]

## Q8

mpg %>%
  group_by(class) %>%
  summarise(average=mean(cty),na.rm=T)
# A tibble: 7 x 3
# class      average na.rm
# <chr>        <dbl> <lgl>
# 1 2seater       15.4 TRUE 
# 2 compact       20.1 TRUE 
# 3 midsize       18.8 TRUE 
# 4 minivan       15.8 TRUE 
# 5 pickup        13   TRUE 
# 6 subcompact    20.4 TRUE 
# 7 suv           13.5 TRUE 

## Q9

mpg %>%
  group_by(class) %>%
  summarise(average=mean(cty),na.rm=T) %>%
  arrange(desc(average))
# A tibble: 7 x 3
# class      average na.rm
# <chr>        <dbl> <lgl>
# 1 subcompact    20.4 TRUE 
# 2 compact       20.1 TRUE 
# 3 midsize       18.8 TRUE 
# 4 minivan       15.8 TRUE 
# 5 2seater       15.4 TRUE 
# 6 suv           13.5 TRUE 
# 7 pickup        13   TRUE 

## Q10

head(mpg %>%
  group_by(manufacturer) %>%
  summarise(average=mean(hwy),na.rm=T) %>%
  arrange(desc(average)),3)
# A tibble: 6 x 3
# manufacturer average na.rm
# <chr>          <dbl> <lgl>
# 1 honda           32.6 TRUE 
# 2 volkswagen      29.2 TRUE 
# 3 hyundai         26.9 TRUE 

## Q11

summary(mpg)
mpg_3 <- filter(mpg, class=="compact")
mpg_3 %>%
  group_by(manufacturer) %>%
  summarise(number=n(),na.rm=T) %>%
  arrange(desc(manufacturer))
# A tibble: 5 x 3
# manufacturer number na.rm
# <chr>         <int> <lgl>
# 1 volkswagen       14 TRUE 
# 2 toyota           12 TRUE 
# 3 subaru            4 TRUE 
# 4 nissan            2 TRUE 
# 5 audi             15 TRUE 

  

            