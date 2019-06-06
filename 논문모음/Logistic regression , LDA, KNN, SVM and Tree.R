german <- read.csv("D:/수업자료/대학/(4)Senior/졸업논문/german credit data/germancredit.csv",sep=",",header=T)
head(german)

#### 1. Logistic regression model
---------------------------------
  
## Logistic regression model
  
glm.german <- glm(Default~checkingstatus1+duration+history+purpose
                  +amount+savings+employ+factor(installment)+status
                  +others+factor(residence)+property+age+otherplans
                  +housing+factor(cards)+job+factor(liable)
                  +tele+foreign,data=german,family="binomial")
summary(glm.german)

# Not available variable 1 : purpose, employ, installment, status,
#                           residence, property, age, housing,
#                           cards, job, liable, tele

glm.german <- glm(Default~checkingstatus1+duration+history
                  +amount+savings+others+otherplans
                  +foreign,data=german,family="binomial")
summary(glm.german)

# Not available variable 2 : amount

glm.german <- glm(Default~checkingstatus1+duration+history
                  +savings+others+otherplans
                  +foreign,data=german,family="binomial")
summary(glm.german)
par(mfrow=c(2,2))
plot(glm.german)
anova(glm.german,test="Cp")
anova(glm.german,test="LRT")
anova(glm.german,test="Rao")

## Semiparametric logistic regression model

install.packages("SemiPar")
library(SemiPar)

attach(german)
spm.german <- spm(Default~checkingstatus1+f(duration)+history+purpose
                  +f(amount)+savings+employ+factor(installment)+status
                  +others+factor(residence)+property+f(age)+otherplans
                  +housing+factor(cards)+job+factor(liable)
                  +tele+foreign,family="binomial")
summary(spm.german)

# Not available variable 1 : purpose, residence, housing, cards,
#                            job, liable, tele

spm.german <- spm(Default~checkingstatus1+f(duration)+history
                  +f(amount)+savings+employ+factor(installment)+status
                  +others+property+f(age)+otherplans
                  +foreign,family="binomial")
summary(spm.german)

# Not available variable 2 : others, property, foreign

spm.german <- spm(Default~checkingstatus1+f(duration)+history
                  +f(amount)+savings+employ+factor(installment)+status
                  +f(age)+otherplans,family="binomial")
summary(spm.german)
par(mfrow=c(2,2))
plot(spm.german)
detach(german)
# 그래프를 보면 모든 설명변수에 대한 선들이 거의 직선에 가깝다.
# 모형은 되도록이면 간단하면서 설명력이 높을 수록 좋은 것이므로
# 일반적인 logistic regression을 이용한 model을 선택하기로 함.

### Misclassification rate

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(n/5)
error.rate.glm <- NULL
for ( k in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  glm.german <- glm(Default~checkingstatus1+duration+history
                    +savings+others+otherplans
                    +foreign,data=german.train,family="binomial")
  pred.glm <- (predict(glm.german, german.test, type="response") >= 0.5)
  error.rate.glm <- c(error.rate.glm, mean(actual != pred.glm))
}
boxplot(error.rate.glm,main="Misclassification rate (Logistic Regression Model)")
mean(error.rate.glm) 

#### 2. LDA(Linear Discriminant Analysis) : 선형판별분석
--------------------------------------------------------
  
install.packages("MASS")
library(MASS)

lda.german <- lda(Default~.,data=german)
lda.german
summary(lda.german)
attributes(lda.german)

### Misclassification rate

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(n/5)
error.rate.lda <- NULL
for ( k in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  lda.german <- lda(Default~.,data=german.train)
  pred.lda <- predict(lda.german, german.test)$class
  error.rate.lda <- c(error.rate.lda, mean(actual != pred.lda))
}
boxplot(error.rate.lda,main="Misclassification rate (Linear Discriminant Analysis)")
mean(error.rate.lda) 

#### 3. KNN(K-Nearest Neighbor) Classification
-----------------------------------------------
  
germanknn <- read.csv("C:/Users/user/Dropbox/수업자료/대학/(4)Senior/졸업논문/german credit data/germanknn.csv",sep=",",header=T)

# train data와 test data의 size가 반드시 동일해야 한다.
# 설명변수가 문자가 아닌 숫자여야 한다. (Using Euclidean Distance)

install.packages("class")
library(class)

## Standardization
# 20가지의 설명변수들의 단위가 모두 다르기 때문에
# 발생할 수 있는 문제점을 제거하기 위하여
# 단위를 통일시키기 위한 표준화 작업 실시.

germanknn.std <- scale(germanknn[,2:21])
germanknn1 <- cbind(germanknn[,1],germanknn.std)
colnames(germanknn1)[1] <- "Default"

### Misclassification rate

par(mfrow=c(1,1))
n <- dim(germanknn1)[1]
test.size <- round(n/2)
error.rate.knn <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- germanknn1[test,-1]; actual <- germanknn1[test,1]
  german.train <- germanknn1[-test,-1]
  pred.knn <- knn(german.train,german.test,actual,k=30)
  error.rate.knn <- c(error.rate.knn, mean(actual != pred.knn))
}
boxplot(error.rate.knn,main="Misclassification rate (K-nearest Neighbor)")
mean(error.rate.knn) 

#### 4. SVM(Support Vector machine)
------------------------------------

germansvm <- read.csv("C:/Users/user/Dropbox/수업자료/대학/(4)Senior/졸업논문/german credit data/germansvm.csv",sep=",",header=T)

### Method 1 : Using svm()

install.packages("e1071")
library(e1071)

svm.german <- svm(Default~.,data=germansvm)
svm.german
summary(svm.german)
attributes(svm.german)

## Misclassification rate

par(mfrow=c(1,1))
n <- dim(germansvm)[1]
test.size <- round(n/5)
error.rate.svm <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- germansvm[test,-1]; actual <- germansvm[test,1]
  german.train <- germansvm[-test,]
  svm.german <- svm(Default~.,data=german.train)
  pred.svm <- predict(svm.german,german.test)
  error.rate.svm <- c(error.rate.svm, mean(actual != pred.svm))
}
boxplot(error.rate.svm,main="Misclassification rate (Support Vector Machine)")
mean(error.rate.svm)

### Method 2 : Using ksvm()

install.packages("kernlab")
library(kernlab)

svm.german <- ksvm(Default~.,data=germansvm)
svm.german
summary(svm.german)
attributes(svm.german)

## Misclassification rate

par(mfrow=c(1,1))
n <- dim(germansvm)[1]
test.size <- round(n/5)
error.rate.svm <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- germansvm[test,-1]; actual <- germansvm[test,1]
  german.train <- germansvm[-test,]
  svm.german <- ksvm(Default~.,data=german.train)
  pred.svm <- predict(svm.german,german.test)
  error.rate.svm <- c(error.rate.svm, mean(actual != pred.svm))
}
boxplot(error.rate.svm,main="Misclassification rate (Support Vector Machine)")
mean(error.rate.svm) 

#### 5. Tree method
--------------------

install.packages("rpart")
library(rpart)

tr.german <- rpart(Default~.,data=german)
print(tr.german)
attributes(tr.german)
plot(tr.german)
# 더 멋진 그림
install.packages("partykit")
library(partykit)
plot(as.party(tr.german))

### Misclassification rate

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(n/5)
error.rate.tr <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  tr.german <- rpart(Default~.,data=german.train)
  pred.tr <- (predict(tr.german,german.test) >= 0.5)
  error.rate.tr <- c(error.rate.tr, mean(actual != pred.tr))
}
boxplot(error.rate.tr,main="Misclassification rate (Tree Method)")
mean(error.rate.tr) 

## Random experiment result
# Logistic Regression Model : 0.2483983
# Linear Discriminant Analysis : 0.2485033
# K-Nearest Neighbor Classification : 0.3044587
# Support Vector Machine : 0.264475
# Tree Method : 0.2775917 