#******************************************************
#
#   Session 3: 중복 데이터, 범주형 변수 관계 및 이상치 분석
#
#******************************************************

#***************************
# 0.1 필요한 라이브러리 설치 및 로드
#***************************
library(ggplot2)   # 시각화의 기본
library(dplyr)     # 데이터 조작 (filter, select, mutate 등)
library(stringr)   # 문자열 처리
library(GGally)    # 확장된 시각화 (산점도 행렬 등)
library(psych)     # 기술통계 분석
library(lattice)   # 격자형 그래프
library(ggpubr)    # 그래프 배치 및 출판용 정렬
library(gplots)    # 추가적인 도표 시각화

#**********************************************************
# 1. 데이터 불러오기
#**********************************************************

# 실제 본인의 환경에 맞춘 경로 설정
sourcedir <- "~/Desktop/다변량_실습파일"
traindir  <- "~/Desktop/다변량_실습파일/Train Data"

# 작업 디렉토리 변경
setwd(sourcedir)

# 사고 데이터를 읽어오는 함수가 정의된 외부 R 스크립트 실행
source("AccidentInput.R")

# 1. 데이터를 리스트 형식으로 불러오기
acts <- file.inputl(traindir)

# 2. 리스트 형태의 데이터를 하나의 데이터 프레임으로 결합
# (이 과정이 완료되어야 'totacts'라는 통합 데이터셋이 생깁니다)
totacts <- combine.data(acts)


#*************************************************
#		2. 데이터 정제 (More Data Cleaning)
#*************************************************

#***********************************************************
#		2.1 범주형 변수 설정 (Factor 처리)
#***********************************************************

# 사고 유형(TYPE) 숫자를 읽기 쉬운 라벨(탈선, 정면충돌 등)로 변환
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", 
                                                "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", 
                                                "GradeX", "Obstruction", "Explosive", "Fire","Other",
                                                "SeeNarrative" ))

# 열차 유형(TYPEQ) 데이터 정제: 불필요한 ".0"이나 공백 제거
totacts$TYPEQ <- sub("\\.0$", "", totacts$TYPEQ)
totacts$TYPEQ[totacts$TYPEQ %in% c("O", "")] <- ""
# 열차 종류를 범주형 변수로 변환 (화물열차, 여객열차 등)
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("NA", "Freight", "Passenger", "Commuter", 
                                                  "Work",  "Single", "CutofCars", "Yard", "Light", "Maint",
                                                  "MaintOfWay", "Passenger", "Commuter", "ElectricMulti", "ElectricMulti"))

# 사고 원인(Cause) 그룹화: 상세 코드의 첫 글자만 따서 대분류 생성
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M" # 기타 (MISCELLANEOUS)
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T" # 선로, 노반 및 구조물
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S" # 신호 통신
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H" # 인적 요인 (Human Factors)
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E" # 기계 및 전기적 고장

# 생성한 원인 분류를 범주형 변수로 확정
totacts$Cause <- factor(totacts$Cause)


#***********************************************************
#		2.2 이상치(Extreme data points) 탐색
#***********************************************************

# 사고 피해액(ACCDMG) 분포 확인 (히스토그램)
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_histogram()

# 박스플롯을 통해 이상치(수염 위로 삐져나온 점들) 시각화
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# ggplot 내부 데이터를 추출하여 통계치 확인
# 이 함수는 그래프의 축 범위, 수염(whisker)의 끝값 등을 계산해줍니다.
dmgbox.built <- ggplot_build(dmgbox)

# 박스플롯 데이터에서 'ymax'(상단 수염 끝값) 추출
# 이 값보다 큰 데이터는 통계적으로 이상치로 간주됩니다.
upper <- dmgbox.built$data[[1]]$ymax


#***********************************************************
#		2.2.1 이상치(Extreme) 데이터만 분리 및 분석
#***********************************************************

# 상단 수염보다 피해액이 큰 데이터만 필터링하여 새로운 데이터셋 생성
xdmg <- totacts %>% filter(ACCDMG > upper)

# 이상치 사고 건수 확인
count(xdmg)

# 전체 사고 중 극단적인 사고(이상치)가 차지하는 비율 계산
count(xdmg)/count(totacts)

# --- 피해 금액 비중 분석 ---
# 1. 극단적 사고의 총 피해액 합계
extreme.sum <- totacts %>% 
  dplyr::select(ACCDMG) %>%
  filter(ACCDMG > dmgbox.built$data[[1]]$ymax) %>% 
  sum()
# 2. 전체 사고의 총 피해액 합계
total.sum <- sum(totacts$ACCDMG)

# 극단적인 소수의 사고가 전체 비용에서 차지하는 비중 (보통 매우 높게 나타남)
extreme.sum/total.sum

# --- 초고액 사고 탐색 ---
# 피해액이 1,500만 달러를 초과하는 행 인덱스 확인
which(xdmg$ACCDMG > 15e6)

# 해당 사고의 구체적인 내용(Narrative, 사고 경위 설명) 확인
# 102번째 행의 서술형 데이터(122~136열)를 살펴봅니다.
xdmg[102, c(122:136)]


#***********************************************************
#		2.2.2 중복 데이터(Duplicate data points) 처리
#***********************************************************

# 최대 피해액 사고 찾기
which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# 동일한 최대 금액이 여러 개인지 확인 (중복 입력 가능성)
xdmg %>% 
  dplyr::select(ACCDMG) %>% 
  filter(ACCDMG == max(ACCDMG))
# 사고 설명(Narrative) 열만 선택하여 중복 여부 판단
# "NARR"로 시작하는 열들을 골라 최대 금액 사고의 내용을 확인합니다.
narrative = xdmg %>%  
  dplyr::select(ACCDMG, starts_with("NARR")) %>% 
  filter(ACCDMG == max(ACCDMG))

# 연도, 월, 일, 시간 등 발생 시점이 완벽히 동일한 데이터가 있는지 검사 (상위 100개)
duplicated(xdmg[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# 특정 사고 번호("110058")를 가진 데이터의 발생 시간 확인
xdmg %>% 
  dplyr::select(INCDTNO, YEAR, MONTH, DAY, TIMEHR, TIMEMIN) %>% 
  filter(INCDTNO == "110058")

# 사고 번호와 시간까지 모두 동일한 중복 데이터 확인
duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# --- 중복 데이터 제거 ---
# 방법 1: R 기본 함수(duplicated)의 역(!)을 이용하여 중복되지 않은 행만 추출
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])), ]

# 방법 2: Tidyverse의 distinct 함수 사용 (권장: 더 직관적이며 .keep_all로 다른 열 유지 가능)
xdmgnd <- xdmg %>% distinct(INCDTNO, YEAR, MONTH, DAY, TIMEHR, TIMEMIN, .keep_all = TRUE)

# --- 결과 비교 ---
# 중복 제거 전(xdmg)과 후(xdmgnd)의 데이터 크기 비교
dim(xdmg)
dim(xdmgnd)

# 최종적으로 제거된 중복 데이터의 건수 확인
count(xdmg) - count(xdmgnd)
