### 한국복지패널

setwd("D:/Workplace/R_Data_Analysis/part2/stage6_RealAnalysis/Ex04_복지")

install.packages("foreign") # spss 파일 설치
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# read data
raw_welfare <- read.spss(file="09-1.Koweps_hpc10_2015_beta1.sav",
                         to.data.frame=T)
welfare <- raw_welfare # 복사본


# check data
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# rename
welfare <- rename(welfare,                  
                  sex=h10_g3,               # 성별 
                  birth=h10_g4,             # 태어난 연도
                  marriage=h10_g10,         # 혼인 상태
                  religion=h10_g11,         # 종교
                  code_job=h10_eco9,        # 직종 코드
                  code_region=h10_reg7,     # 지역 코드
                  income=p1002_8aq1)        # 월급


## 성별에 따른 월급 차이

class(welfare$sex)
table(welfare$sex)
welfare$welfare$sex <- ifelse(welfare$sex==9,NA,welfare$sex) # 결측치 처리
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex==1,"male","female") # 성별 항목이름 부여
table(welfare$sex)
qplot(welfare$sex)


# 월급 변수 검토 및 전처리

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income) #결측치 처리
table(is.na(welfare$income)) # 결측치 확인


# 성별 월급표

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income=mean(income))
sex_income
ggplot(sex_income,aes(x=sex,y=mean_income,fill=sex)) +
  geom_col()


## 나이, 월급

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

summary(welfare$birth)
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth==9999,NA,welfare$birth)
table(is.na(welfare$birth))

# 나이 파생변수 생성

welfare$age <- 2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

# 나이별 월급표

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income=mean(income))
head(age_income)
summary(age_income)
ggplot(age_income,aes(x=age,y=mean_income)) +
  geom_line()
welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income=mean(income)) %>%
  arrange(desc(mean_income)) %>%
  head(5)


## 연령대, 월급

# 연령대 파생변수
welfare <- welfare %>%
  mutate(ageg=ifelse(age<30,"young",
                     ifelse(age<=59,"middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>%         # 연령대별 월급표
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income=mean(income))
ageg_income
ggplot(ageg_income,aes(x=ageg,y=mean_income,fill=ageg)) +
  geom_col() +
  scale_x_discrete(limits=c("young","middle","old"))

# 연령대 파생변수 2
welfare <- welfare %>%
  mutate(ageg2=ifelse(age<30,"20대 이하",
                      ifelse(age<40,"30대",
                             ifelse(age<50,"40대",
                                    ifelse(age<60,"50대","60대 이상")))))
table(welfare$ageg2)
qplot(welfare$ageg2)

ageg2_income <- welfare %>%         # 연령대별 월급표
  filter(!is.na(income)) %>%
  group_by(ageg2) %>%
  summarise(mean_income=mean(income))
ageg2_income
ggplot(ageg2_income,aes(x=ageg2,y=mean_income,fill=ageg2)) +
  geom_col() +
  xlab("연령대") + ylab("월급")


## 연령대 및 성별 월급 차이

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%
  summarise(mean_income=mean(income))
sex_income

# barplot
ggplot(sex_income,aes(x=ageg,y=mean_income,fill=sex)) +
  geom_col() +
  scale_x_discrete(limits=c("young","middle","old"))
ggplot(sex_income,aes(x=ageg,y=mean_income,fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("young","middle","old"))

# line graph
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age,sex) %>%
  summarise(mean_income=mean(income))
head(sex_age)
ggplot(sex_age,aes(x=age,y=mean_income,col=sex)) +
  geom_line()


## 직업별 월급

class(welfare$code_job)
table(welfare$code_job)

# 직업분류코드
library(readxl)
list_job <- read_excel("09-1.Koweps_Codebook.xlsx",col_names=T,sheet=2)
head(list_job)
dim(list_job)

# welfare에 직업명 결합
welfare <- left_join(welfare,list_job,id="code_job")
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income=mean(income)) 
head(job_income)

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10 # 상위 10
ggplot(top10,aes(x=reorder(job,mean_income),y=mean_income,fill=job)) +
  geom_col() +
  coord_flip()
bottom10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  tail(10)
bottom10 # 하위 10
ggplot(bottom10,aes(x=reorder(job,-mean_income),y=mean_income,fill=job)) +
  geom_col() +
  coord_flip()


## 성별에 따른 직업

# male
job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

# female
job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

# graph
ggplot(job_male,aes(x=reorder(job,n),y=n,fill=job)) +
  geom_col() +
  coord_flip()
ggplot(job_female,aes(x=reorder(job,n),y=n,fill=job)) +
  geom_col() +
  coord_flip()


## 종교 유무에 따른 이혼율

class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion==1,"yes","no")
table(welfare$religion)
qplot(welfare$religion)

class(welfare$marriage)
table(welfare$marriage)

# 이혼 여부 변수
welfare$group_marriage <- ifelse(welfare$marriage==1,"marriage",
                                 ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# 종교 유무에 따른 이혼율
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion,group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
religion_marriage

divorce <- religion_marriage %>%
  filter(group_marriage=="divorce") %>%
  select(religion,pct)
divorce
ggplot(divorce,aes(x=religion,y=pct,fill=religion)) +
  geom_col()

# 연령대별 이혼율
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg,group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage
ageg_divorce <- ageg_marriage %>%
  filter(ageg!="young" & group_marriage=="divorce") %>%
  select(ageg,pct)
ageg_divorce
ggplot(ageg_divorce,aes(x=ageg,y=pct,fill=ageg)) +
  geom_col()

# 연령대, 종교유무, 결혼상태별 이혼율

ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  group_by(ageg,religion,group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
ageg_religion_marriage
df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage=="divorce") %>%
  select(ageg,religion,pct)
df_divorce
ggplot(df_divorce,aes(x=ageg,y=pct,fill=religion)) +
  geom_col(position="dodge")


## 지역별 연령대 비율

class(welfare$code_region)
table(welfare$code_region)

# 전처리
list_region <- data.frame(code_region=1:7,
                          region=c("서울",
                                   "수도권(인천/경기)",
                                   "부산/경남/울산",
                                   "대구/경북",
                                   "대전/충남",
                                   "강원/충북",
                                   "광주/전남/전북/제주도"))
list_region
welfare <- left_join(welfare,list_region,id="code_region") # 지역명 변수 추가
welfare %>%
  select(code_region,region) %>%
  head

# 지역별 연령대 비율표
region_ageg <- welfare %>%
  group_by(region,ageg) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,2))
head(region_ageg)
ggplot(region_ageg,aes(x=region,y=pct,fill=ageg)) +
  geom_col() +
  coord_flip()

# 노년층 비율 내림차순
list_order_old <- region_ageg %>%
  filter(ageg=="old") %>%
  arrange(desc(pct))
list_order_old
order <- list_order_old$region
order
ggplot(list_order_old,aes(x=region,y=pct,fill=region)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits=order)

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg,
                           level=c("old","middle","young"))
levels(region_ageg$ageg)
ggplot(region_ageg,aes(x=region,y=pct,fill=ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits=order)
