#********************************************************************************
#              일변량 그래픽 (Univariate Graphics)
#********************************************************************************

#***************************
# 0.1 시각화를 위한 라이브러리 설치 및 로드 준비
#***************************
rm(list = ls()) # 현재 R 환경에 저장된 모든 변수와 데이터를 삭제하여 초기화합니다.
sessionInfo()   # 현재 사용 중인 R의 버전과 로드된 패키지 정보를 출력합니다.


#***************************
# 0.2 이 세션에 필요한 라이브러리 설치 및 로드
#***************************
# 패키지 설치는 처음에 한 번만 하면 됩니다 (주석 처리됨).
# install.packages('ggplot2')
library(ggplot2) # 데이터 시각화를 위한 핵심 패키지 로드
# install.packages('dplyr')
library(dplyr)   # 데이터 조작(필터링, 선택 등)을 위한 패키지 로드

#**********************************************************
# 1. 데이터 읽어오기 (Reading in data)
#**********************************************************

#***************************
# 1.1 작업 디렉토리 설정
#***************************

# 분석 스크립트와 보조 함수(PCAplots.R 등)가 저장된 폴더 경로
sourcedir <- "~/Desktop/다변량_실습파일"

# 실제 철도 사고 CSV 데이터 파일들이 들어있는 폴더 경로
traindir <- "~/Desktop/다변량_실습파일/Train Data"

# 작업 디렉토리를 소스 파일이 있는 곳으로 설정합니다.
setwd(sourcedir)
getwd() # 설정된 현재 작업 디렉토리를 확인합니다.

# 외부 R 스크립트 파일 실행 (데이터 입력 및 그래프 함수 등을 가져옴)
source("AccidentInput.R")
source("PCAplots.R")


#***************************
# 1.2 데이터 로드 (특정 연도)
#***************************

# 2025년도 철도 사고 데이터를 먼저 읽어옵니다.
# file.path 함수는 운영체제에 상관없이 폴더 경로와 파일명을 안전하게 합쳐줍니다.
accident_data_25 <- read.csv(file.path(traindir, "RailAccidents25.csv"))

# (참고) 아래와 같이 직접 전체 경로를 입력해서 읽어올 수도 있습니다.
# accident_data_25 <- read.csv("~/Desktop/다변량_실습파일/Train Data/RailAccidents25.csv")

#***************************
# 1.3 선택된 연도 데이터의 정보 확인
#***************************

head(accident_data_25)    # 데이터의 앞부분 6행을 보여줍니다.
dim(accident_data_25)     # 데이터의 차원(행의 개수, 열의 개수)을 확인합니다.
summary(accident_data_25)  # 각 변수(열)별 요약 통계량(평균, 최솟값, 최댓값 등)을 보여줍니다.
str(accident_data_25)      # 데이터의 구조(변수 타입, 관측치 수 등)를 상세히 확인합니다.

# 특정 변수들("ACCDMG": 사고피해액, "TOTKLD": 사망자수, "CARS": 차량수, "STATION": 역)의 구조만 확인
str(accident_data_25[,c("ACCDMG", "TOTKLD", "CARS", "STATION")])

colnames(accident_data_25) # 데이터셋의 모든 열 이름(변수명)을 출력합니다.
class(accident_data_25$TOTKLD) # 'TOTKLD' 변수의 데이터 타입(예: 숫자형, 문자형)을 확인합니다.
var(accident_data_25$TOTKLD)   # 사망자 수의 분산을 구합니다 (수치형 데이터).
mean(accident_data_25$TOTKLD)  # 사망자 수의 평균을 구합니다.
table(accident_data_25$TOTKLD) # 사망자 수의 값별 빈도수(도수분포표)를 만듭니다.

class(accident_data_25$STATION) # 'STATION' 변수의 타입을 확인합니다.
levels(accident_data_25$STATION) # 범주형 변수의 항목(레벨)들을 확인합니다.
levels(as.factor(accident_data_25$STATION)) # 요인(Factor)형으로 변환 후 항목 확인
table(as.factor(accident_data_25$STATION))  # 각 역(STATION)별 사고 빈도수 확인

# round() 함수를 사용하여 평균값을 반올림하여 출력합니다.
print(mean(accident_data_25$TOTKLD))
print(round(mean(accident_data_25$TOTKLD)))

#***************************
# 1.4 전체 연도(약 20~23년치) 데이터 로드
#***************************

# 모든 데이터를 하나의 '리스트' 구조에 넣을 것입니다.
# 소스해둔 AccidentInput.R의 함수를 활용합니다.
setwd(sourcedir)
source("AccidentInput.R")

# 데이터 파일들이 모여있는 폴더(traindir)의 파일 목록을 가져옵니다.
setwd(traindir)
my.files <- list.files(traindir)

# stringi 라이브러리를 사용하여 파일 목록 중 "csv"가 포함된 파일만 골라냅니다.
library(stringi)
my.csv.files <- my.files[which(stri_detect_fixed(my.files,"csv")==TRUE)]

# 방법 1: lapply 함수를 사용해 모든 csv 파일을 리스트 형식으로 읽어오기
acts <- lapply(my.csv.files, read.csv)

# 방법 2: AccidentInput.R에 정의된 file.inputl 함수를 사용해 읽어오기
acts <- file.inputl(traindir) 

# acts[[1]]은 첫 번째 파일(2001년), acts[[2]]는 두 번째 파일 데이터를 담고 있습니다.
acts[[1]]$YEAR # 첫 번째 데이터셋의 연도 확인 (1 -> 2001년 의미)
acts[[5]]$YEAR # 다섯 번째 데이터셋의 연도 확인 (5 -> 2005년 의미)


##################################################
#
#	2. 데이터 정제 (Data Cleaning)
#
##################################################

#***************************
# 2.1 모든 연도 데이터 합치기
#***************************
# 리스트 형태인 acts에 담긴 모든 데이터프레임을 하나로 합칩니다.
totacts <- combine.data(acts)

# 합쳐진 전체 데이터셋의 행과 열 개수를 확인합니다.
dim(totacts)

# 루프를 돌며 각 연도별 데이터 행 수를 더해 전체 행 수가 맞는지 검증합니다.
rows = 0
for(acc_year in acts){
  print(dim(acc_year))
  rows = rows + dim(acc_year)[1]
  print(rows)
}
# 위에서 계산한 rows 값과 dim(totacts)[1] 값이 같아야 정상입니다.
print(rows)
dim(totacts)[1]


# tidyverse(dplyr)를 사용하여 연도별 사고 건수(데이터 행 수)를 집계하고 내림차순 정렬합니다.
library(tidyverse)
annualCounts = totacts %>% group_by(YEAR) %>% 
  summarise(entries = n()) %>% 
  arrange(desc(entries))

#***********************************
#
# 	3. 특정 사고 조사 (Investigating particular accidents)
#
#***********************************

# 가장 큰 금전적 피해(ACCDMG)를 입힌 사고는 무엇인가?
which(totacts$ACCDMG == max(totacts$ACCDMG)) # 최댓값이 위치한 행 번호 찾기
totacts %>% filter(ACCDMG == max(ACCDMG))    # 해당 사고 데이터 필터링
totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG))] # 최대 피해 금액 확인

worst_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG)) # 별도 변수에 저장


# 최대 피해를 입힌 사고의 종류(TYPE)는?
totacts$TYPE[which(totacts$ACCDMG == max(totacts$ACCDMG))]
worst_dmg %>% dplyr::select(TYPE)

# 부상자수(TOTINJ)가 가장 많았던 사고 찾기
max_totinj = which(totacts$TOTINJ == max(totacts$TOTINJ))
totacts$TOTINJ[max_totinj] # 최대 부상자 수 확인

totacts[max_totinj,]        # 해당 행 전체 데이터 보기
totacts[max_totinj,122:136] # 해당 행의 122~136번째 열(보통 설명 데이터)만 보기
totacts$ACCDMG[max_totinj]  # 이 사고의 금전적 피해액 확인

worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ))
worst_inj %>% dplyr::select(TOTINJ)


# 부상자가 가장 많았던 사고의 서사(Narrative, 사고 경위) 확인
# dplyr 패키지의 select를 명시적으로 사용하여 "NARR"로 시작하는 모든 열을 선택합니다.
worst_inj %>% 
  dplyr::select(starts_with("NARR"))

# 사고 피해액과 서사를 함께 보기
worst_inj %>% 
  dplyr::select(ACCDMG, starts_with("NARR"))


#***********************************
#
# 	4. 시각화 (Visualization)
#
#***********************************

#***************************
# 4.1 히스토그램 (Histogram) - 분포 확인
#***************************

# 2011년도(11번째 리스트) 사고 피해액 히스토그램
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue") + # 막대 색상을 강철색으로 설정
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5)) # 제목 중앙 정렬

# 구간 너비(bins)를 조절하는 다양한 방법

# FD 법칙(Freedman-Diaconis)을 이용한 구간 설정
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.FD(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011 (FD)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Scott 법칙을 이용한 구간 설정
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.scott(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011 (Scott)") + 
  theme(plot.title = element_text(hjust = 0.5))

# 구간 수를 강제로 20개로 설정
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 20) + 
  ggtitle("Total Accident Damage in 2011 (Bins=20)") + 
  theme(plot.title = element_text(hjust = 0.5))

# 구간 수를 2개로 설정 (너무 단순화됨)
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 2) + 
  ggtitle("Total Accident Damage in 2011 (Bins=2)") + 
  theme(plot.title = element_text(hjust = 0.5))


# 특정 연도들(1, 4, 8, 11년차)을 선택하여 비교
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR) # 연도별로 그래프를 나누어 그림

# 23개 연도 중 랜덤으로 4개를 골라 비교
selected_years = totacts %>% filter(YEAR %in% sample(1:23,4))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  facet_wrap(~YEAR)


# 2009년~2020년(9~20년차) 데이터 히스토그램
selected_years = totacts %>% filter(YEAR %in% c(9:20))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 10) + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  facet_wrap(~YEAR)


# 축의 범위를 고정하여 연도별 규모를 객관적으로 비교 (1~12년차)
selected_years = totacts %>% filter(YEAR %in% c(1:12))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  xlim(c(0,1.7e7)) + ylim(c(0,1000)) + # x축과 y축 범위 고정
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~YEAR)


#***************************
# 4.2 박스플롯 (Boxplots) - 이상치 및 사분위수 확인
#***************************

# 기본 박스플롯 (가로/세로 전환 포함)
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + 
  coord_flip() # 그래프 방향을 세로로 회전

# 이상치(Outlier) 모양 변경 (동그라미, 별표, X자 등)
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) +
  geom_boxplot(col= "steelblue", outlier.shape=1) + coord_flip()

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = "*", outlier.size = 5) + coord_flip() 


# 여러 연도를 하나의 그래프에서 박스플롯으로 비교
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  coord_flip() + 
  facet_wrap(~YEAR)

# x축(피해액) 범위를 제한하여 상자 부분을 더 자세히 보기
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  coord_flip() + 
  xlim(c(0,1.7e7)) +
  facet_wrap(~YEAR)


#***************************
# 4.3 QQ 플롯 (QQ plots) - 정규성 확인
#***************************

# 사고 피해액(ACCDMG)이 정규분포를 따르는지 확인 (대체로 직선에서 벗어남)
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(sample=acts[[11]]$ACCDMG)) + 
  stat_qq() + 
  stat_qq_line() + # 기준선 추가
  ggtitle("Total Accident Damage QQ Plot")

# 사고 당시 온도(TEMP)의 QQ 플롯 (피해액보다는 정규분포에 가까운 형태)
ggplot(as.data.frame(acts[[11]]$TEMP), aes(sample=acts[[11]]$TEMP)) + 
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("Accident Temperature QQ Plot")

#***************************
# 4.4 밀도 그래프 (Density plot)
#***************************

# 가우시안 커널을 사용한 밀도 추정 곡선 (기본 R 함수 사용)
d <- density(acts[[11]]$ACCDMG, kernel = "gaussian")
plot(d, main = "Gaussian Kernel Density", col = 'red')

# 히스토그램 위에 밀도 곡선을 겹쳐서 그리기
h <- hist(acts[[20]]$ACCDMG, breaks = "FD", plot = FALSE) # 구간 정보 계산
ggplot(data=acts[[20]], aes(ACCDMG)) + 
  geom_histogram(aes(y =..density..), # y축을 빈도가 아닌 밀도로 설정
                 breaks=h$breaks, col="green", fill="green", alpha=.2) +
  geom_density(fill="red", color=1, alpha=0.4) + # 빨간색 밀도 곡선 추가
  labs(title="Density and Histogram for ACCDMG")

#***************************
# 4.5 사고 유형별 막대 그래프 (Bar plot)
#***************************

# 사고 유형(TYPE) 빈도 막대 그래프
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")

# 디자인 개선 (색상 추가, 레이블 45도 회전)
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity", fill= "steelblue")+ 
  theme(axis.text.x = element_text(size = 8, angle = 45))

# 숫자로 된 사고 유형 코드에 의미 있는 이름(레이블) 붙이기
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment(탈선)", "HeadOn(정면충돌)", 
                                                "Rearend(후방충돌)", "Side(측면충돌)", "Raking", "BrokenTrain", "Hwy-Rail", 
                                                "GradeX", "Obstruction(장애물)", "Explosive(폭발)", "Fire(화재)","Other",
                                                "SeeNarrative" ))

# 이름이 적용된 막대 그래프 출력
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity", fill= "steelblue")+ 
  labs(x = "Type of Accident") +
  theme(axis.text.x = element_text(size = 8, angle = 45))

# 열차 구성 유형(TYPEQ) 정리 및 시각화
totacts$TYPEQ <- sub("\\.0$", "", totacts$TYPEQ) # 뒤에 붙은 .0 제거
totacts$TYPEQ[totacts$TYPEQ %in% c("O", "")] <- "" # 의미 없는 값 처리
# 실제 데이터 사전에 맞춰 레이블 부여 (화물열차, 여객열차 등)
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("NA", "Freight(화물)", "Passenger(여객)", "Commuter", 
                                                  "Work",  "Single", "CutofCars", "Yard", "Light", "Maint",
                                                  "MaintOfWay", "Passenger", "Commuter", "ElectricMulti", "ElectricMulti"))

ggplot(as.data.frame(table(totacts$TYPEQ)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(size = 8, angle = 45))

# 사고 원인(CAUSE) 데이터 정리
# 원인 코드의 첫 글자(M, T, S, H, E)만 추출하여 대분류 생성
totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M" # 기계적 결함
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T" # 선로 결함
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S" # 신호 결함
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H" # 인적 과실
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] # 기타

totacts$Cause <- factor(totacts$Cause) # 범주형으로 변환

# 사고 원인별 빈도 시각화
ggplot(as.data.frame(table(totacts$Cause)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  labs(x = "Cause of Accident")


#***************************
# 4.6 시계열 그래프 (Time series)
#***************************

# 연도별 총 사고 피해액의 변화 추이 계산
# tapply를 사용하여 연도별(YEAR)로 피해액(ACCDMG)의 합계(sum)를 구합니다.
df <- data.frame(year=2001:2025, damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))

# 연도에 따른 피해액 변화를 선과 점으로 표시
ggplot(data=df, aes(x=year, y=damages)) + 
  geom_line() +  # 선 그래프
  geom_point()   # 각 연도에 점 표시