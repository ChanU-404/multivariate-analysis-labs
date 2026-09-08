# ******************************************************
#
#					세션 6
#				다중 선형 회귀 (Multiple Linear Regression)
#	 평가지표 및 변수 선택 (Metrics & Variable Selection)
#  	 진단 및 데이터 변환 (Diagnostics & Transformations)
#
# ******************************************************

# 필요한 라이브러리들을 로드합니다.
library(here)      # 파일 경로를 쉽게 관리
library(dplyr)     # 데이터 조작 (필터링, 선택 등)
library(ggplot2)   # 시각화 (그래프 그리기)
library(ggpubr)    # 여러 그래프 배치 및 통계 그래프 보조
library(ggfortify) # 회귀 진단 그래프(autoplot) 시각화 지원
library(MASS)      # Box-Cox 변환 및 통계 함수 제공
library(lindia)    # 선형 회귀 진단 도구 제공

# 데이터 불러오기 준비
# --- 경로 수정 부분 ---

# 1. 분석 스크립트와 도구 파일들이 저장된 폴더 경로
sourcedir <- "~/Desktop/다변량_실습파일"

# 2. 실제 사고 데이터(CSV)들이 저장된 폴더 경로
traindir  <- "~/Desktop/다변량_실습파일/Train Data"

# 작업 디렉토리를 설정하고 외부 스크립트(도구함)를 불러옵니다.
setwd(sourcedir)
source("AccidentInput.R") # 데이터 입력 함수
source("TestSet.R")       # 학습/테스트 데이터 분리 함수
source("PCAplots.R")      # 시각화 보조 함수

# 메모리에 두 가지 데이터 구조가 생성되어야 합니다.
# acts: 각 연도별 사고 데이터프레임이 담긴 리스트
acts <- file.inputl(traindir)

# 각 연도별 데이터의 차원(행과 열의 수)을 확인합니다.
sapply(acts, dim)

# 2001년부터 2020년까지의 데이터를 하나로 합쳐 'totacts'를 만듭니다.
totacts <- combine.data(acts)

# 사고 피해액(ACCDMG)을 기준으로 '극단적인 사고(Extreme accidents)' 데이터프레임을 구축합니다.
# 박스플롯을 통해 이상치 범위를 확인합니다.
dmgbox <- boxplot(totacts$ACCDMG)

# ggplot을 이용해 강철색(steelblue) 박스플롯을 그려 시각적으로 확인합니다.
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip() # 가로로 눕히기

# 박스플롯의 상단 수염(stats[5])보다 큰 피해액을 가진 사고들만 추출하여 xdmg에 저장합니다.
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# [참고] 9/11 테러 관련 데이터를 분석에서 제외합니다 (특이 케이스).
xdmg <- xdmg[-102,]

# 중복된 사고 데이터를 제거합니다 (사고번호, 연, 월, 일, 시간이 같은 경우).
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# 행 번호를 1부터 순차적으로 다시 부여합니다.
rownames(xdmgnd) <- NULL


# 선형 회귀 모델 구축: lm(종속변수 ~ 독립변수들)
# 모델 1: 피해액을 온도, 기차속도, 차량수, 기관차 위치(HEADEND1)로 예측
xdmgnd.lm1 <- lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd)

# 모델 2: 기관차 위치를 제외하고 온도, 기차속도, 차량수만 사용
xdmgnd.lm2 <- lm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd)


# 각 모델의 회귀 분석 결과 요약 보고서를 출력합니다.
summary(xdmgnd.lm1)
summary(xdmgnd.lm2)

# [확인 사항] 결과에서 추정 계수(Coefficients), 잔차(Residuals), t-검정 결과, 
# F-검정 결과, 결정계수(R^2), 수정된 결정계수(Adjusted R^2)를 찾을 수 있어야 합니다.

# 모델 객체 내에 저장된 정보들의 이름을 확인합니다.
names(xdmgnd.lm1)

# [질문] 각 선형 모델의 계수(Coefficients)는 무엇인가요?
# (Intercept)        TEMP      TRNSPD        CARS    HEADEND1 
# 496991.1987    310.6949  17797.0739   4592.7896 -65112.5422 
coef(xdmgnd.lm1)

# [질문] 잔차 제곱합(Sum of the Residuals Squared, SSE)은 얼마인가요?
sum(xdmgnd.lm1$res^2)
# [1] 1.229044e+16

# **********************************
# 평가지표 및 변수 선택 (Metrics and Variable Selection)
# **********************************

# 기준 기반 평가 (Criterion based assessments)

# 수정된 결정계수 (Adjusted R^2): 변수가 늘어남에 따른 패널티를 고려한 설명력
summary(xdmgnd.lm1)$adj.r.squared

# AIC (Akaike Information Criterion): 모델의 적합도와 복잡도의 균형 (낮을수록 좋음)
AIC(xdmgnd.lm1)

# BIC (Bayesian Information Criterion): AIC보다 변수 증가에 더 엄격한 패널티 부여
AIC(xdmgnd.lm1, k=log(nrow(xdmgnd)))


# 변수 선택 (Variable Selection)

# 단계적 회귀법 (Stepwise Regression): AIC를 기준으로 최적의 변수 조합을 자동으로 찾음
# trace=T는 각 단계를 화면에 보여줍니다.
xdmgnd.lm1.step <- step(xdmgnd.lm1, trace=T)

# 자동 선택된 최종 모델의 요약 결과를 봅니다.
summary(xdmgnd.lm1.step)


# 부분 F 검정 (Partial F Test)
# 두 모델이 서로 중첩(Nested)될 때(한 모델이 다른 모델의 변수를 모두 포함할 때) 사용합니다.
anova(xdmgnd.lm1, xdmgnd.lm2)
anova(xdmgnd.lm1, xdmgnd.lm1.step)


# **********************************
# 테스트 세트 (Test Sets) - 모델 검증
# **********************************

# TestSet.R 파일의 함수를 사용합니다.
source("TestSet.R")

# 데이터를 학습용(Train)과 테스트용(Test)으로 나눌 비율을 설정합니다 (여기서는 1/3을 테스트용으로).
test.size <- 1/3

# 원본 데이터를 학습 세트와 테스트 세트로 분리합니다.
xdmgnd.data <- test.set(xdmgnd, test.size)

# [질문] 테스트 세트와 학습 세트의 피해액(ACCDMG) 분포가 비슷한지 확인하세요.

# 방법 1: 기본 히스토그램 비교
par(mfrow=c(2,2)) # 화면을 2x2로 분할
hist(xdmgnd.data$train$ACCDMG, main="학습 세트")
hist(xdmgnd.data$test$ACCDMG, main="테스트 세트")
hist(xdmgnd$ACCDMG, main="전체 데이터")
par(mfrow=c(1,1))

# 방법 2: ggplot과 ggarrange를 이용한 비교
a <- ggplot(as.data.frame(xdmgnd.data$train$ACCDMG), aes(xdmgnd.data$train$ACCDMG)) + geom_histogram()
b <- ggplot(as.data.frame(xdmgnd.data$test$ACCDMG), aes(xdmgnd.data$test$ACCDMG)) + geom_histogram()
c <- ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_histogram()
ggarrange(a, b, c, ncol=2, nrow = 2)

# 학습 세트(train)만을 사용하여 모델을 다시 만듭니다.
xdmgnd.lm1.train <- lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd.data$train)
xdmgnd.lm2.train <- lm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd.data$train)


# 예측 성능 측정 (Predicted MSE)
# 테스트 세트의 독립변수를 넣어 피해액을 예측해 봅니다.
xdmgnd.lm1.pred <- predict(xdmgnd.lm1.train, newdata=xdmgnd.data$test) 
xdmgnd.lm2.pred <- predict(xdmgnd.lm2.train, newdata=xdmgnd.data$test)

# 예측값과 실제값의 차이인 예측 오차 제곱 평균(PMSE)을 계산합니다.
pmse.xdmgnd.lm1 <- mse(xdmgnd.lm1.pred, xdmgnd.data$test$ACCDMG)
pmse.xdmgnd.lm1

pmse.xdmgnd.lm2 <- mse(xdmgnd.lm2.pred, xdmgnd.data$test$ACCDMG)
pmse.xdmgnd.lm2

# [질문] PMSE를 기준으로 볼 때 어떤 모델이 더 우수한가요? (낮을수록 좋습니다)

#모델1

# 버전 2: 여러 번 반복(20회)하여 평균적인 성능 비교
pmse1.result <- NULL;
pmse2.result <- NULL;

for (i in c(1:20)){
  test.size <- 1/3
  xdmgnd.data <- test.set(xdmgnd, test.size) # 무작위 분리
  
  # 학습 및 예측 반복
  lm1.train <- lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd.data$train)
  lm2.train <- lm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd.data$train)
  
  lm1.pred <- predict(lm1.train, newdata=xdmgnd.data$test) 
  lm2.pred <- predict(lm2.train, newdata=xdmgnd.data$test) 
  
  pmse.lm1 <- mse(lm1.pred, xdmgnd.data$test$ACCDMG)
  pmse.lm2 <- mse(lm2.pred, xdmgnd.data$test$ACCDMG)
  
  # 결과 저장
  pmse1.result <- c(pmse1.result, pmse.lm1)
  pmse2.result <- c(pmse2.result, pmse.lm2)
}

# 20회 반복 수행 결과 시각화 비교

# 방법 1: 기본 선 그래프 (파란색=모델1, 빨간색=모델2)
plot(pmse1.result, type='b', col='blue', xlab="반복 횟수", ylab="PMSE")
lines(pmse2.result, type='b', col='red')
title(main="PMSE 기준 모델 비교")

# 방법 2: ggplot 시각화
Index <- 1:length(pmse1.result);
df <- data.frame(Index, pmse1.result, pmse2.result)
ggplot(data=df, aes(x=Index)) +
  geom_line(aes(y = pmse1.result), color = 'blue', size = 1) +
  geom_line(aes(y = pmse2.result), color = 'red', linetype = 'twodash', size = 1) +
  ggtitle("PMSE 기준 모델 비교 (ggplot)") +
  theme(plot.title = element_text(hjust = 0.5))

# [질문] 그래프를 육안으로 확인했을 때 어떤 모델이 더 낫습니까?
#blue
# 통계적 검정을 통한 비교
# 대응 표본 t-검정 (Paired t-test)
t.test(pmse1.result, pmse2.result, paired=T)

# 윌콕슨 부호 순위 검정 (Wilcoxon Test, 비모수 검정)
wilcox.test(pmse1.result, pmse2.result, paired=T)

# [질문] t-검정과 윌콕슨 검정 결과를 바탕으로 어떤 모델의 성능이 더 좋다고 판단되나요?

성능 같음
# 교차 검증 (Cross-Validation)
library(boot)

# 교차 검증을 위해서는 lm 대신 glm 함수를 사용해야 합니다.
# (lm은 glm의 특수한 형태이므로 결과는 같습니다.)
xdmgnd.lm1.cv <- glm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd)
xdmgnd.lm2.cv <- glm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd)

# 10-겹 교차 검증 (10-fold Cross-validation) 수행
xdmgnd.lm1.err <- cv.glm(xdmgnd, xdmgnd.lm1.cv, K=10)
xdmgnd.lm1.err$delta # 오차 값 확인

xdmgnd.lm2.err <- cv.glm(xdmgnd, xdmgnd.lm2.cv, K=10)
xdmgnd.lm2.err$delta

# [참고] delta의 첫 번째 값은 가공되지 않은 CV 추정치, 두 번째 값은 조정된(adjusted) 추정치입니다.

# [질문] 조정된 CV 추정치를 바탕으로 모델 1과 모델 2 중 어떤 것이 더 우수한가요?


# **********************************
# 회귀 진단 플롯 (Diagnostics Plot)
# **********************************

# lindia 패키지로 한눈에 진단하기
gg_diagnose(xdmgnd.lm1)

# ggfortify의 autoplot을 사용하여 6가지 진단 그래프 그리기
autoplot(xdmgnd.lm1, which=1:6, label.size = 3) + theme_bw()

# 주요 4가지 진단 그래프 (잔차, QQ, Scale-Location, 영향력)
autoplot(xdmgnd.lm1, which = c(1,2,3,5), ncol = 2, label.size = 3) + theme_bw()

# 개별 그래프 분석
autoplot(xdmgnd.lm1, which=1) # Residual vs Fitted: 선형성 및 등분산성 확인
autoplot(xdmgnd.lm1, which=2) # Normal Q-Q: 오차항의 정규성 확인
autoplot(xdmgnd.lm1, which=3) # Scale-Location: 잔차의 고른 퍼짐 정도 확인
autoplot(xdmgnd.lm1, which=4) # Cook's distance: 영향력이 큰 이상치 확인
autoplot(xdmgnd.lm1, which=5) # Residuals vs Leverage: 이상치가 회귀선에 주는 영향 확인

# [질문] Cook's Distance 그래프에서 지목된 특정 사고들에서는 어떤 일이 있었나요?

# [질문] 모델 1의 각 진단 플롯에서 무엇이 관찰됩니까? 관찰 내용과 문제점을 논의하세요.
# a. 잔차 vs 적합값 (residuals vs. fitted)
# b. QQ-플롯 (qq-plot)
# c. 스케일-위치 (Scale-Location)
# d. Cook's 거리 / 잔차 vs 레버리지 (Cook's distance / Residuals vs. Leverage)


# **********************************
# 데이터 변환 (Transformations)
# **********************************

# 종속변수인 피해액(ACCDMG)의 밀도 그래프를 그려봅니다.
ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_density()

# [질문] 종속변수의 분포 가정을 위반하고 있나요? (정규분포를 따르는가?)


# Box-Cox 변환: 비정규 데이터를 정규성에 가깝게 만드는 람다(λ) 값을 찾습니다.
boxcox(xdmgnd.lm1) # Box-Cox 그래프 시각화

# ggplot 버전 Box-Cox
gg_boxcox(xdmgnd.lm1)

# 람다 범위를 지정하여 확인
boxcox(xdmgnd.lm1, plotit=T, lambda=seq(-2,2,by=0.5))

# 최적의 람다 값 찾기
BC_values <- boxcox(xdmgnd.lm1, plotit=F)
L <- BC_values$x[which.max(BC_values$y)] # 로그 우도를 최대화하는 람다 선택
L # 최적의 L(lambda) 값 출력

# 최적의 람다 변환을 적용한 새로운 모델 구축
xdmgnd.lm1.boxcox <- lm((ACCDMG^L-1)/L ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd)

# 원본 모델과 변환 모델의 결과 비교 (결정계수 상승 확인)
summary(xdmgnd.lm1)
summary(xdmgnd.lm1.boxcox)

# 변환 후 종속변수의 분포 확인 (더 정규분포에 가까워졌는지 확인)
ggplot(as.data.frame((xdmgnd$ACCDMG^L-1)/L), aes((xdmgnd$ACCDMG^L-1)/L)) + geom_density()

# 새 모델의 진단 플롯 확인
autoplot(xdmgnd.lm1.boxcox, which = c(1,2,3,5), ncol = 2) + theme_bw()

# [질문] 새로운 Box-Cox 모델의 진단 플롯에서 무엇이 관찰됩니까? 변환이 도움이 되었나요?


# 로그 변환 (Logarithm Transform) 시도
# 많은 경우 로그 변환만으로도 큰 효과를 봅니다.
ggplot(as.data.frame(log(xdmgnd$ACCDMG)), aes(log(xdmgnd$ACCDMG))) + geom_density()

# 로그 변환 모델 구축
xdmgnd.lm1.log <- lm(log(ACCDMG) ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd)

# 로그 모델 진단 및 요약
autoplot(xdmgnd.lm1.log, which = c(1,2,3,5), ncol = 2) + theme_bw()
summary(xdmgnd.lm1.log)

# [질문] 로그 변환 모델의 진단 플롯에서는 무엇이 관찰됩니까? 
# Box-Cox 변환과 로그 변환 중 어떤 것을 선택하시겠습니까?