## sqldf() 패키지 활용

install.packages("sqldf")
library(sqldf)
library(googleVis)
Fruits

sqldf(' select * from Fruits ')
sqldf(" select * from Fruits where Fruit='Apples' ")
sqldf(' select * from Fruits where Fruit=\'Apples\' ')
sqldf(' select * from Fruits limit 3 ')     # LIMIT 0, 3
sqldf(' select * from Fruits limit 3, 5 ')
sqldf(' select * from Fruits order by Year ')
sqldf(' select * from Fruits order by Year desc ')

sqldf(' select sum(Sales) from Fruits ')
sqldf(' select max(Sales) from Fruits ')
sqldf(' select min(Sales) from Fruits ')

sqldf(' select Fruit, sum(Sales) from Fruits group by Fruit')
sqldf(' select Fruit, sum(Sales), sum(Expenses), sum(Profit) 
      from Fruits group by Fruit ')
sqldf(' select Year, avg(Sales), avg(Expenses), avg(Profit) 
      from Fruits group by Year ')
sqldf(' select Fruit, sum(Sales), sum(Expenses), sum(Profit) 
      from Fruits group by Fruit 
      order by sum(Profit) desc ')
sqldf(' select Fruit, max(Sales), min(Sales) from Fruits group by Fruit ')
sqldf(' select * from Fruits where Sales = ( select min(Sales) from Fruits ) ')
sqldf(' select * from Fruits where Expenses = ( select max(Expenses) from Fruits ) ')

# 화면에 보이는 내용만 변경
sqldf(c(' update Fruits set Profit=50 where Fruit=\'Apples\' and Year=2008 ',
        'select * from Fruits'))

song <- read.csv("song.csv", header = F, fileEncoding = 'utf8')
names(song) <- c(V1="_id", V2="title", V3="lyrics", V4="girl_group_id")
girl_group <- read.csv("girl_group.csv", header=F, fileEncoding='utf8')
names(girl_group) <- c(V1="_id", V2="name", V3="debut")

sqldf(" select gg._id, gg.name, gg.debut
      from girl_group as gg
      inner join song as s 
      on gg._id = s.girl_group_id")
