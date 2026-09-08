#******************************************************
#
#   다중 선형 회귀 분석 (Multiple Linear Regression) 1
#
#******************************************************

# 작업 디렉토리 설정
# 1. 분석 스크립트가 있는 폴더 경로
sourcedir <- "~/Desktop/다변량_실습파일"
# 2. 실제 데이터(CSV 파일들)가 들어있는 폴더 경로
traindir  <- "~/Desktop/다변량_실습파일/Train Data"

## 데이터 불러오기
setwd(sourcedir)
getwd()
source("AccidentInput.R") # 사고 데이터를 읽어오고 합치는 함수가 담긴 파일 실행

# 필요한 라이브러리 로드
library(ggplot2)   # 시각화 기본
library(GGally)    # 상관관계 행렬(ggpairs)용
library(devtools)  # 외부 패키지 로드용
library(psych)     # 기술 통계 및 상관 분석(pairs.panels)용
library(ggpubr)    # 그래프 배치용

# 데이터 입력 및 통합
acts <- file.inputl(traindir)
totacts <- combine.data(acts)

## 사고 피해액(ACCDMG)의 극단적 이상치(Extreme accidents)만 별도로 추출
# 박스플롯을 통해 통계적 이상치 경계 확인
dmgbox <- boxplot(totacts$ACCDMG)

# ggplot을 이용해 세로형 박스플롯 시각화
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

# 박스플롯의 상단 수염(stats[5])보다 큰 데이터만 xdmg에 저장
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# 9/11 테러 데이터 제거 (일반적인 사고 모델 분석을 왜곡시키는 특수 사례)
xdmg <- xdmg[-102,]

## 중복 데이터 제거 (사고번호, 시간 등이 동일한 데이터 처리)
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# -----------------------------------------------------------
# 범주형 변수 설정 (Categorical Variables Setup)
# -----------------------------------------------------------

# 1. 사고 원인(Cause) 변수 생성: 코드의 첫 글자를 따서 대분류로 그룹화
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M" # 기타
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T" # 선로/구조물
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S" # 신호
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H" # 인적 요인
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E" # 기계/전기 고장

# 범주형(Factor) 변수로 변환
xdmgnd$Cause <- factor(xdmgnd$Cause)

# 2. 사고 유형(Type) 변수 변환 및 의미 있는 라벨 부여
xdmgnd$Type <- factor(
  xdmgnd$TYPE, labels = c(
    "Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", 
    "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"
  )
)

# 3. 열차 종류(TYPEQ) 변환 및 라벨 부여
xdmgnd$TYPEQ <- as.numeric(xdmgnd$TYPEQ)
xdmgnd$TYPEQ <- factor(xdmgnd$TYPEQ, labels = c(
  "Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"
)
)

# -----------------------------------------------------------
# 사고 피해액 예측을 위한 잠재적 독립 변수 탐색
# -----------------------------------------------------------

# 산점도 행렬(SPM): 양적 변수들 간의 상관관계 확인
# 피해액, 열차속도, 차량수, 시간, 온도 간의 관계 분석
pairs.panels(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])
ggpairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# 주성분 분석(PCA): 상관행렬 기반으로 양적 변수 요약
source("PCAplots.R")
pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )
biplot(pred.pca)

# ggbiplot을 이용한 세련된 시각화
ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])

# [문제] ## Which predictors are most correlated with accident damage?
# 바이플롯에서 화살표의 길이가 길어 설명력이 높고 ACCDMG와 유사한 차원(PC2 양의 방향 등)을 공유하는
#**CARS(열차 칸 수)**와 제1주성분의 핵심 변수인 
#**TRNSPD(열차 속도)**가 사고 피해액과 가장 상관관계가 높은 주요 변수입니다.


# -----------------------------------------------------------
# 범주형 변수 기반 시각화 (Categorical plots)
# -----------------------------------------------------------

# 히트맵: 사고 원인(Cause)과 유형(Type)에 따른 사고 빈도 분석
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")

# [문제] ## Which accident causes and types have the highest numbers of extreme accidents?
#히트맵 분석 결과, 선로 및 구조물 결함(T)으로 인한 탈선(Derailment) 사고가 
#극단적 사고 중 가장 압도적으로 높은 빈도를 기록하고 있습니다.

# 사고 유형별 열차 속도(TRNSPD)와 피해액(log 변환)의 관계 (Trellis Plot)
library(lattice)
xyplot(log(ACCDMG)~TRNSPD | Type, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Type, scales = "free")

# 사고 원인별 열차 속도와 피해액의 관계 분석
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause, scales = "free")

# [문제] ##What is notable about the relationship between train speed and accident
# 모든 원인에서 속도가 빠를수록 피해액이 커지는 양의 상관관계를 보이지만, 
#특히 **인적 요인(H)과 신호(S)**에 의한 사고는 속도 증가에 따른 피해액의 상승 폭(회귀선의 기울기)이 
#타 원인에 비해 매우 가파르게 나타납니다.

# [문제] ##damages for different accident causes and types?
# **정면 충돌(HeadOn)과 측면 충돌(Side)**은 속도와 피해액 간의 매우 뚜렷하고 강한 양의 선형 관계를 보이나, 화재(Fire)나 폭발(Explosive) 유형은 속도가 피해액에 미치는 영향이 거의 없거나 오히려 미미한 것으로 관찰됩니다.

# 복합 조건부 산점도: 사고 원인 X 유형별 분석
xyplot(log(ACCDMG)~TRNSPD | Cause * Type, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Type, scales = "free")

# 탈선(Derailment) 여부에 따른 사고 원인별 상호작용 분석
xdmgnd$Derail <- (xdmgnd$Type == "Derailment")
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Derail, scales = "free")

# [문제] ## How might these results inform your hypotheses?
#이 결과는 단순히 속도가 피해액을 높인다는 일반론을 넘어, 사고의 원인과 유형(특히 탈선 여부)에 따라 속도의 영향력이 극명하게 달라지는 **'상호작용 효과'**를 가설에 반드시 반영해야 함을 시사합니다.

# [문제] ## Use the multivariate visualizations as evidence to form at least 1 hypothesis.
# 선 사고(Derailment = TRUE)의 경우, 비탈선 사고에 비해 열차 속도 증가가 사고 피해액 상승에 미치는 영향이 통계적으로 훨씬 더 강력하고 유의미할 것이라는 가설을 세울 수 있습니다.


# -----------------------------------------------------------
# 선형 회귀 모델 구축 (Linear Models)
# -----------------------------------------------------------

# [모델 1] 단일 예측 변수: 온도(TEMP)
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
summary(xdmgnd.lm1)

# [문제] #null hypothesis for model utility? 
# 모든 회귀 계수는 0
# [문제] #Btemp = 0
# 독립 변수인 온도(TEMP)가 사고 피해액(ACCDMG)에 미치는 선형적인 영향력이 존재하지 않는다는 개별 변수에 대한 귀무가설
# [문제] #fail to reject the null hypothesis because temp is not significant at p<0.05 level
# TEMP의 p-value가 0.705로 유의수준 0.05보다 훨씬 크기 때문에 귀무가설을 기각하는 데 실패하며, 결과적으로 온도는 피해액을 설명하는 데 유의미한 변수가 아님.

names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2) # 잔차 제곱합 확인


# [모델 2] 두 개의 예측 변수: 온도(TEMP) + 열차 속도(TRNSPD)
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)

# [문제] #null hypothesis for the model utilty?
# 모든 예측 변수의 회귀 계수는 0이다
# [문제] #null hypothesis for the t-test for temperature?
# 열차 속도가 모델에 포함되어 있을 때 온도의 계수는 0이다
summary(xdmgnd.lm2)
names(xdmgnd.lm2)
coef(xdmgnd.lm2)


# [모델 3] 세 개의 예측 변수: 온도 + 속도 + 차량수(CARS)
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm3)

coef(xdmgnd.lm3)

# -----------------------------------------------------------
# 결과 해석 및 가설 검정 (Interpretation & Hypothesis Testing)
# -----------------------------------------------------------

# [문제] # Interpret your model coefficients.  Do they make sense?
# 열차 속도(TRNSPD, 약 $16,786 증가)와 차량 수(CARS, 약 $3,732 증가)가 늘어날수록 사고 피해액이 커지는 양의 계수를 가지므로 물리적 상식에 부합하지만, 온도(TEMP)는 계수가 매우 작고 통계적으로 유의미하지 않아 영향력이 거의 없습니다.

# [문제] # Interpret your developed models using the model utility test and t-test.
# F-통계량의 p-value가 매우 낮아 모델이 전체적으로 유의미함(Utility)을 보여주며, 개별 변수 중 속도와 차량 수는 유의미한 예측 변수(t-test 통과)인 반면 온도는 피해액 설명에 기여하지 못하는 것으로 나타났습니다.

# [문제] # Write out the null and alternative hypothesis for each of the tests. 
# 모델 효용성: $H_0: \beta_{TEMP} = \beta_{TRNSPD} = \beta_{CARS} = 0$ / $H_1$: 적어도 하나의 계수는 0이 아니다.개별 변수 유의성: $H_0: \beta_i = 0$ / $H_1: \beta_i \neq 0$ (각 독립 변수 $i$에 대해 수행)

# [문제] # Do you reject or fail to reject H0 for each test?
# 모델 전체($F$)와 속도 및 차량 수($t$)에 대한 귀무가설은 유의수준 0.05에서 **기각(Reject)**하며, 온도($t$)에 대한 귀무가설은 p-value(0.527)가 높으므로 **채택(Fail to reject)**합니다.


####################################
#	사망자(TOTKLD) 및 부상자(TOTINJ)에 대해 동일한 프로세스 반복
####################################