rm(list = ls())

## 패키지 불러오기
library(forecast)
library(zoo)

## 데이터 불러오기.
data <-read.csv('F:\\4학년 1학기\\시계열분석\\Amtrak data.csv')

## ts 형태로 변환하기.
data.ts <- ts(data$Ridership, start = c(1991, 1), 
              end = c(2004, 3), freq = 12)

## validation, test 길이 설정
nvalid <- 24
ntest <- 24
ntrain <- length(data.ts) - (nvalid + ntest)

## 데이터 분할
train.ts <- window(data.ts, start = c(1991, 1),
                   end = c(1991, ntrain))
valid.ts <- window(data.ts, start = c(1991, ntrain+1),
                   end = c(1991, ntrain + nvalid))
test.ts <- window(data.ts, start = c(1991, ntrain+nvalid+1),
                  end = c(1991, ntrain+nvalid+ntest))


## 탐색적 자료 분석
ma <- rollmean(train.ts, k = 12, align = 'center')

plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", 
     xlim = c(1991,2000.25), main = "Ridership")

lines(ma, lwd = 2)



## 나이브 예측치(계절성 고려 롤 포워드 방식)
naive.pred <- ts(data$Ridership[(ntrain-11):(ntrain+nvalid-12)], 
                 start = c(1991, ntrain + 1), 
                 end = c(1991, ntrain + nvalid), 
                 freq = 12)


## 12 윈도우 이동평균 예측치
ma <- rollmean(train.ts, k = 12, align = 'right')
last.ma <- tail(ma, 1)
ma.pred <- ts(rep(last.ma, nvalid), 
              start = c(1991, ntrain + 1), 
              end = c(1991, ntrain + nvalid), 
              freq = 12)


## Holt-Winter의 지수평할기법 이용 예측치
## hwin1 : 가법 추세, 승법 계절, 오차
hwin1 <- ets(train.ts, model = 'MAM')
hwin1.pred <- forecast(hwin1, h = nvalid, level = 0)

## hwin2 : 가법 추세, 계절, 오차
hwin2 <- ets(train.ts, model = 'AAA')
hwin2.pred <- forecast(hwin2, h = nvalid, level = 0)


## 회귀기반 모형, 계절성을 더미변수로 한 예측치 
lm <- tslm(train.ts ~ trend + I(trend^2) + season)
lm.pred <- forecast(lm, h = nvalid, level = 0)


## 회귀기반 모형, 계절성을 삼각함수로 한 예측치 
lm2 <- tslm(train.ts ~ trend + I(trend ^ 2) 
            + I(sin(2*pi*trend /12)) 
            + I(cos(2*pi*trend /12)))
lm2.pred <- forecast(lm2, h = nvalid, level = 0)


## 모형 별 RMSE 값 비교
accuracy(naive.pred,valid.ts)[2]
accuracy(ma.pred,valid.ts)[2]
accuracy(hwin1.pred,valid.ts)[4]
accuracy(hwin2.pred,valid.ts)[4]
accuracy(lm.pred,valid.ts)[4]
accuracy(lm2.pred,valid.ts)[4]


## train, validation 기간 시각화
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", 
     xlim = c(1991,2002.25), main = "Ridership")
lines(valid.ts)
lines(c(2000.2, 2000.2), c(0, 3500)) 
text(1996.25, 2500, "Training")
text(2001.25, 2500, "Validation")

TrainValid.ts <- window(data.ts, start = c(1991, 1),
              end = c(1991, ntrain + nvalid))
ma <- rollmean(TrainValid.ts, k = 12, align = "center")

lines(ma, lwd = 2)



## 최종모형 적합1 (회귀기반 모형)
finalm <- tslm(TrainValid.ts ~ trend + I(trend^2) + season)
finalm.pred <- forecast(finalm, h = ntest, level = 0)

accuracy(finalm.pred,test.ts)


## 최종모형 적합2 (hwin 모형)
finalhwin <- ets(TrainValid.ts, model = 'MAM')
finalhwin.pred <- forecast(finalhwin, h = nvalid, level = 0)

accuracy(finalhwin.pred,test.ts)



## 최종모형 예측 시각화
plot(finalm.pred, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", 
     xlim = c(2002.25,2004.25), main = "lm Pred")
lines(test.ts)

plot(finalhwin.pred, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", 
     xlim = c(2002.25,2004.25), main = "hwin Pred")
lines(test.ts)





