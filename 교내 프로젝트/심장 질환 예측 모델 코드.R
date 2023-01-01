# binary response인 HeartDisease 컬럼을 예측하는 여러 통계학습 모델 (수업시간에 배운 통계학습 모델 사용)을 만들어서 AUC값을 비교하시오. 

# K-fold CV방법을 활용하여 HeartDisease 컬럼에 대한 예측을 진행하시오. 

# 조원 별로 다른 모델로 적합해 보고 나중에 취합해서 비교하는 형식으로 리포트를 작성하시오. 즉 예측력이 잘 나올 것 같은 모델 2~3개를 선택해서 적합해 보고 값을 AUC값을 비교하는 형식으로 하면 좋을 것 같습니다. 

# 리포트 마지막 파트에는 참여한 조원들에 대한 역할에 대해 자세히 서술해 주시기 바립니다.
rm(list=ls())


## 데이터 불러오기
path <- 'F:/3학년 2학기/데이터마이닝/프로젝트/'
heart <- read.csv(paste0(path,'heart.csv'))

head(heart)


## 범주형 데이터 형식 펙터로 바꾸기
heart$Sex <- factor(heart$Sex)
heart$ChestPainType <- factor(heart$ChestPainType)
heart$RestingECG <- factor(heart$RestingECG)
heart$ExerciseAngina <- factor(heart$ExerciseAngina)
heart$ST_Slope <- factor(heart$ST_Slope)

summary(heart)


## 연속형/이산형 자료 분포 확인
num_feature <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
qual_feature <- setdiff(names(heart), num_feature)

par(mfrow = c(2,2))

sapply(num_feature, function(x) {
  hist(heart[,x], main = x, freq = F, col = "skyblue")
})
sapply(qual_feature, function(x) {
  barplot(table(heart[,x]), col = "skyblue", main = x)
})



## 각 열에서 결측치의 여부를 판단해본다.
par(mfrow = c(1,1))
sapply(names(heart), function(x) sum(is.na(heart$x)))

## RestingBP이 0인 관측치 하나는 잘못된 관측치로 판단하고 제거한다.
sum(heart$RestingBP == 0)
heart <- heart[-which(heart$RestingBP == 0),]
hist(heart[,'RestingBP'], freq = F, col = "skyblue")

## Cholesterol 변수에 대해
sum(heart$Cholesterol == 0)

par(mfrow = c(1,2))
barplot(table(heart$HeartDisease), col = "skyblue")
barplot(table(heart[heart$Cholesterol == 0,]$HeartDisease),
        col = "skyblue")
par(mfrow = c(1,1))

## 데이터를 K개(10개)로 분리한다.
set.seed(45)
kfoldseed <- sample(1:nrow(heart), nrow(heart))
k <- 10
kfold <- split(kfoldseed, f = ceiling(seq_along(kfoldseed)/round(nrow(heart)/k)))



## LDA 방법 적용 

library(MASS)
#install.packages('pROC')
library(pROC)

acc_lda <- numeric(k)
auc_lda <- numeric(k)

for (i in 1:k){
  lda.fit <- lda(HeartDisease~., data=heart[-kfold[[i]],])
  pred <- predict(lda.fit, newdata=heart[kfold[[i]],])
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, pred$class)
  acc_lda[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_lda[i] <- roc(as.numeric(real), as.numeric(pred$posterior[,2]))$auc
}
acc_lda
mean(acc_lda)
auc_lda
mean(auc_lda)


## QDA 방법 적용 

acc_qda <- numeric(k)
auc_qda <- numeric(k)

for (i in 1:k){
  qda.fit <- qda(HeartDisease~., data=heart[-kfold[[i]],])
  pred <- predict(qda.fit, newdata=heart[kfold[[i]],])
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, pred$class)
  acc_qda[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_qda[i] <- roc(as.numeric(real), as.numeric(pred$posterior[,2]))$auc
}
acc_qda
mean(acc_qda)
auc_qda
mean(auc_qda)

## 트리 방법 적용 
#install.packages('tree')
library(tree)
acc_tree <- numeric(k)
auc_tree <- numeric(k)
for (i in 1:k){
  tree.fit <- tree(HeartDisease~., data=heart[-kfold[[i]],])
  pred <- predict(tree.fit, newdata=heart[kfold[[i]],])
  predvalue <- ifelse(pred > 0.5, 1, 0)
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, predvalue)
  acc_tree[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_tree[i] <- roc(as.numeric(real), as.numeric(pred))$auc
}
acc_tree
mean(acc_tree)
auc_tree
mean(auc_tree)


## 부스팅 방법 적용
library(gbm)
set.seed(45)
acc_boost <- numeric(k)
auc_boost <- numeric(k)
for (i in 1:k){
  boost.fit <- gbm(HeartDisease~., data=heart[-kfold[[i]],],
                   distribution ="bernoulli")
  pred <- predict(boost.fit, newdata=heart[kfold[[i]],])
  predvalue <- ifelse(pred > 0.5, 1, 0)
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, predvalue)
  acc_boost[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_boost[i] <- roc(as.numeric(real), as.numeric(pred))$auc
}
acc_boost
mean(acc_boost)
auc_boost
mean(auc_boost)



## 부스팅 방법 적용(+ 하이퍼 파라미터 조정)
boostfun <- function(depth = 5){
  set.seed(45)
  auc_boost <- numeric(k)
  for (i in 1:k){
    boost.fit <- gbm(HeartDisease~., data=heart[-kfold[[i]],],
                     distribution ="bernoulli",
                     interaction.depth = depth)
    pred <- predict(boost.fit, newdata=heart[kfold[[i]],])
    predvalue <- ifelse(pred > 0.5, 1, 0)
    real <- heart$HeartDisease[kfold[[i]]]
    auc_boost[i] <- roc(as.numeric(real), as.numeric(pred))$auc
  }
  return(mean(auc_boost))
}
sapply(c(2,3,5,7,10),function(x) boostfun(depth = x))



## 부스팅 방법 최종 적용
set.seed(45)
acc_boost <- numeric(k)
auc_boost <- numeric(k)
for (i in 1:k){
  boost.fit <- gbm(HeartDisease~., data=heart[-kfold[[i]],],
                   distribution ="bernoulli",
                   interaction.depth = 2)
  pred <- predict(boost.fit, newdata=heart[kfold[[i]],])
  predvalue <- ifelse(pred > 0.5, 1, 0)
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, predvalue)
  acc_boost[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_boost[i] <- roc(as.numeric(real), as.numeric(pred))$auc
}
acc_boost
mean(acc_boost)
auc_boost
mean(auc_boost)








## SVC 방법 적용

#install.packages('e1071')
library(e1071)
set.seed(45)
acc_svm <- numeric(k)
auc_svm <- numeric(k)
for (i in 1:k){
  svm.fit <- svm(HeartDisease~., data=heart[-kfold[[i]],],
                   kernel ="linear", cost = 1, scale = FALSE)
  pred <- predict(svm.fit, newdata=heart[kfold[[i]],])
  predvalue <- ifelse(pred > 0.5, 1, 0)
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, predvalue)
  acc_svm[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_svm[i] <- roc(as.numeric(real), as.numeric(pred))$auc
}
acc_svm
mean(acc_svm)
auc_svm
mean(auc_svm)


## SVM 방법 적용
set.seed(45)
acc_svm <- numeric(k)
auc_svm <- numeric(k)
for (i in 1:k){
  svm.fit <- svm(HeartDisease~., data=heart[-kfold[[i]],],
                 kernel ="radial", cost = 1, scale = FALSE)
  pred <- predict(svm.fit, newdata=heart[kfold[[i]],])
  predvalue <- ifelse(pred > 0.5, 1, 0)
  real <- heart$HeartDisease[kfold[[i]]]
  confu <- table(real, predvalue)
  acc_svm[i] <- sum(confu[col(confu) == row(confu)])/ sum(confu)
  auc_svm[i] <- roc(as.numeric(real), as.numeric(pred))$auc
}
acc_svm
mean(acc_svm)
auc_svm
mean(auc_svm)


