library(ggplot2)
View(mpg)
library(dplyr)

mpg %>%
  filter(manufacturer %in% c('audi','toyota')) %>%
  group_by(manufacturer) %>%
  summarise(고속도로연비평균=mean(hwy),na.rm=T)

mpg %>%
  filter(manufacturer %in% c('chevrolet','ford','honda')) %>%
  group_by(manufacturer) %>%
  summarise(시내연비평균=mean(cty),na.rm=T)

## Q1
mpg %>%
  mutate(속도=(cty+hwy)/2) %>%
  group_by(drv) %>%
  select(drv,속도) %>%
  summarise(평균속도=mean(속도))

## Q2
mpg %>%
  mutate(속도=(cty+hwy)/2) %>%
  group_by(cyl) %>%
  arrange(속도)

## Q3
mpg %>%
  mutate(avg_spd=(cty+hwy)/2) %>%
  group_by(class) %>%
  summarise(class별평균속도=mean(avg_spd)) %>%
  arrange(desc(class별평균속도)) %>%
  head(5)
  
mpg %>%
  mutate(avg_spd=(cty+hwy)/2) %>%
  select(manufacturer, class, cty, hwy, avg_spd) %>%
  head(5)

mtcars  

mtcars %>%
  mutate(mpg_per_disp=mpg/disp) %>%
  arrange(desc(mpg_per_disp)) %>%
  head(5)

mtcars %>%
  group_by(cyl) %>%
  summarise(평균마력=mean(hp))

View(mtcars)

iris

iris %>%
  group_by(Species) %>%
  summarise_each(list(mean), Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
