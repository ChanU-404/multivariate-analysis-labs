# ===================================================================
# 1. 함수 정의 영역 (AccidentInput.R 핵심 내용)
# ===================================================================

# 폴더 내 CSV 파일을 모두 읽어 리스트로 저장하는 함수입니다.
file.inputl <- function(my.path) {
  my.dir <- getwd()
  setwd(my.path)
  # PDF 명세서 외에 다른 파일이 섞여 있어도 .csv만 골라냅니다.
  my.files <- list.files(pattern=".csv")
  acts <- lapply(my.files, read.csv)
  setwd(my.dir)
  return(acts)
}

# 리스트의 데이터들을 하나의 데이터 프레임으로 합치는 함수입니다.
combine.data <- function(Data.List, Vars) {
  # 첫 번째 파일에서 선택한 변수만 추출
  DF <- rbind(Data.List[[1]][, Vars])
  # 두 번째 파일부터 마지막까지 반복하며 아래로 붙임
  for(i in 2:length(Data.List)) {
    DF <- rbind(DF, Data.List[[i]][, Vars])
  }
  return(DF)
}

# ===================================================================
# 2. 과제 수행 및 실행 영역
# ===================================================================

# 1. 라이브러리 및 경로 설정
library(ggplot2)
library(psych)
path <- "~/Desktop/다변량_실습파일/Train Data"

# 2. 데이터 불러오기
accidents_list <- file.inputl(path)

# 3. 변수명을 대문자로 수정 (실제 파일 내 이름과 일치시킴)
# accidents_list의 첫 번째 파일에 들어있는 모든 컬럼 이름을 추출합니다.
all_vars <- colnames(accidents_list[[1]])

# 4. 데이터 통합 실행
combined_data <- combine.data(accidents_list, all_vars)

# 5. 결과 확인 및 통계 분석
print(head(combined_data, 10))
# 데이터셋에서 '숫자형(numeric)'인 모든 열만 골라내서 통계치를 보여줍니다.
describe(combined_data[, sapply(combined_data, is.numeric)])

# 8. TRNSPD(기차 속도) 히스토그램 그리기
library(ggplot2)

ggplot(combined_data, aes(x = TRNSPD)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Train Speed (HIGHSPD)",
       x = "Speed (MPH)",
       y = "Frequency") +
  # 10 MPH 지점에 가이드라인 추가 (문제 확인용)
  geom_vline(xintercept = 10, color = "red", linetype = "dashed")

library(ggplot2)

# 9. 연도별 장비 피해액(EQPDMG) 박스 플롯 그리기
ggplot(combined_data, aes(x = factor(YEAR4), y = EQPDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Equipment Damage by Year (2001-Present)",
       x = "Year",
       y = "Equipment Damage ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 연도가 많으므로 글자 회전

library(ggplot2)

# 10. 연도별 총 사망자 수(TOTKLD) 박스 플롯
ggplot(combined_data, aes(x = factor(YEAR4), y = TOTKLD)) +
  geom_boxplot(outlier.color = "red", fill = "lightgrey") +
  labs(title = "Total Killed by Year (2001-Present)",
       x = "Year",
       y = "Total Killed (Persons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# 11연도별 궤도 피해액(TRKDMG) 박스 플롯
ggplot(combined_data, aes(x = factor(YEAR4), y = TRKDMG)) +
  geom_boxplot(outlier.color = "red", fill = "lightcyan") +
  labs(title = "Track Damage by Year (2001-2025)",
       x = "Year",
       y = "Track Damage ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# 12연도별 부상자수(TOTINJ) 박스 플롯
ggplot(combined_data, aes(x = factor(YEAR4), y = TOTINJ)) +
  geom_boxplot(outlier.color = "red", fill = "lightcyan") +
  labs(title = "Track Damage by Year (2001-2025)",
       x = "Year",
       y = "Track Damage ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 13. 부상자 수(TOTINJ)가 최대인 행을 찾아 사건 번호(INCDTNO)를 출력
combined_data[which.max(combined_data$TOTINJ), "INCDTNO"]

library(ggplot2)

# 14. CARSDMG(전체 피해 차량 수) 박스 플롯
ggplot(combined_data, aes(x = factor(YEAR4), y = CARSDMG)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "General Cars Damaged by Year", x = "Year", y = "Cars Damaged")

library(ggplot2)

#15. 
# 1. Sturges 법칙에 따른 빈(bin) 개수 계산
n_bins <- nclass.Sturges(combined_data$TEMP)

# 2. 히스토그램 그리기
ggplot(combined_data, aes(x = TEMP)) +
  geom_histogram(bins = n_bins, fill = NA, colour = 'steelblue') +
  labs(title = "Histogram of TEMP (Sturges Breaks)",
       x = "Temperature (Fahrenheit)",
       y = "Frequency") +
  # 보기에 나온 온도들을 세로선으로 표시해 어느 구간이 가장 높은지 확인
  geom_vline(xintercept = c(53, 63, 73, 83), color = "red", linetype = "dashed", alpha = 0.5) +
  theme_minimal()

library(psych)

# 1. 분석할 변수 선택
vars <- c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")

# 2. PNG 파일로 저장 (RStudio 창보다 훨씬 선명하게 보입니다)
png("scatter_matrix.png", width = 1200, height = 1200)
pairs.panels(combined_data[, vars], 
             method = "pearson", # 피어슨 상관계수 사용
             hist.col = "#00AFBB",
             density = TRUE,      # 밀도 곡선 표시
             ellipses = TRUE)     # 상관 정도를 보여주는 타원 표시
dev.off()

# 3. 눈으로 확인하기 전, 수치로 정확하게 확인 (반올림 2자리)
cor_val <- cor(combined_data$TRKDMG, combined_data$ACCDMG, use = "complete.obs")
round(cor_val, 2)

# 19. ACCDMG(총 사고 피해액)가 최대인 행을 찾아 연도(YEAR4)를 출력
combined_data[which.max(combined_data$ACCDMG), "YEAR4"]

# 20. ACCDMG가 최대인 행을 찾아 주요 변수들을 확인합니다.
# TYPE, JOINTCD(공동운영 여부), TOTKLD(사망자) 등을 중점적으로 봅니다.
max_dmg_accident <- combined_data[which.max(combined_data$ACCDMG), ]

# 상세 정보 출력
print(max_dmg_accident[, c("YEAR4", "TYPE", "JOINTCD", "TOTKLD", "ACCDMG")])

# 23. TOTKLD(총 사망자 수)가 최대인 행을 찾아 연도(YEAR4)를 출력
combined_data[which.max(combined_data$TOTKLD), "YEAR4"]

# 24. 전체 데이터셋에서 TOTKLD(총 사망자 수)의 최댓값만 출력
max(combined_data$TOTKLD)

# 25. 전체 데이터셋에서 TOTINJ(총 부상자 수)의 최댓값만 출력
max(combined_data$TOTINJ)

# 26.ACCDMG가 1,500,000보다 큰 데이터의 개수를 합산합니다.
sum(combined_data$ACCDMG > 1500000)

# 27. TOTKLD(총 사망자 수)가 1 이상인 데이터의 개수를 합산합니다.
sum(combined_data$TOTKLD >= 1)

#27
# 1. TYPE 변수의 빈도표(Table) 생성
type_counts <- table(combined_data$TYPE)

# 2. 막대그래프 생성 (빈도가 높은 순서대로 정렬하면 더 보기 좋습니다)
barplot(sort(type_counts, decreasing = TRUE), 
        main = "Frequency of Accident Types",
        xlab = "Accident Type Code",
        ylab = "Number of Accidents",
        col = "steelblue")
#28
# CAUSE 변수의 첫 글자 빈도수 확인
cause_summary <- table(substr(combined_data$CAUSE, 1, 1))

# 결과 출력
print(cause_summary)

# 가장 큰 값을 가진 원인이 무엇인지 이름으로 확인
names(which.max(cause_summary))

#####################################

#PCA 9

# [1] 새로운 변수 'Casualty' 생성 (사망자 + 부상자)
combined_data$Casualty <- combined_data$TOTKLD + combined_data$TOTINJ

# [2] 박스 플롯 생성
# 대부분의 값이 0이라서 바닥에 선 하나만 보이고 위로 이상치 점들이 찍힐 것입니다.
boxplot(combined_data$Casualty, 
        main = "Box Plot of Total Casualties",
        ylab = "Number of Casualties",
        col = "orange")

# [3] 왜 '모든 설명이 정답(All statements are correct)'인지 수치로 확인
# 이 결과값의 1~4번째 값이 모두 0으로 나오면 정답입니다.
boxplot.stats(combined_data$Casualty)$stats

#PCA 10
# Casualty가 1 이상인 사고의 비율 계산
# (TRUE는 1, FALSE는 0으로 계산되어 평균을 내면 바로 비율이 됩니다)
mean(combined_data$Casualty >= 1)

#11
# 인명 피해(Casualty)가 1명 이상인 사고의 총 건수 확인
sum(combined_data$Casualty >= 1)

#12
# 1. Casualty가 1명 이상인 사고 리포트만 추출
totacts_posCas <- combined_data[combined_data$Casualty >= 1, ]

# 2. 중복을 판단할 기준 변수(컬럼) 리스트 만들기
vars <- c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")

# 3. 중복된 리포트가 몇 건인지 계산
# duplicated()는 앞에서 이미 나온 데이터와 동일한 데이터가 뒤에 또 나오면 TRUE를 반환합니다.
num_duplicates <- sum(duplicated(totacts_posCas[, vars]))
print(num_duplicates) # 여기서 92가 나와야 정답!

# 4. 중복이 제거된 새로운 데이터프레임 생성
totacts_posCas_nd <- totacts_posCas[!duplicated(totacts_posCas[, vars]), ]

#13
# 1. Casualty(인명 피해) 기준 내림차순으로 데이터 정렬
# '-' 부호는 내림차순(큰 것부터)을 의미합니다.
sorted_accidents <- totacts_posCas_nd[order(-totacts_posCas_nd$Casualty), ]

# 2. 상위 2개 사고의 연도(YEAR4)와 피해 규모(Casualty) 확인
# 1행이 1위, 2행이 우리가 찾는 2위입니다.
head(sorted_accidents[, c("YEAR4", "Casualty")], 2)

#14
# 방법 1: aggregate 함수 사용 (추천 - 데이터프레임 형태로 깔끔하게 나옵니다)
annual_casualties <- aggregate(Casualty ~ YEAR4, data = totacts_posCas_nd, FUN = sum)

# 결과 확인 (합계 기준 오름차순 정렬)
annual_casualties[order(annual_casualties$Casualty), ]

#15
# 1. 연도별 인명 피해 사고 건수 확인
accident_counts <- table(totacts_posCas_nd$YEAR4)

# 2. 제시된 후보 연도들(2012, 2011, 2003, 2004)만 따로 확인
accident_counts[c("2012", "2011", "2003", "2004")]

# 3. 전체에서 건수가 가장 적은 순서대로 정렬해서 보기
sort(accident_counts)

#16
# 인명 피해가 발생한 사고들의 유형별 빈도 확인
type_counts_posCas <- table(totacts_posCas_nd$TYPE)

# 빈도가 높은 순서대로 정렬
sort(type_counts_posCas, decreasing = TRUE)

#17
# 1. 인명 피해 데이터셋에서 CAUSE의 첫 글자만 추출
cause_cats_posCas <- substr(totacts_posCas_nd$CAUSE, 1, 1)

# 2. 빈도수 확인 및 정렬
# H, T, M, S, E 중 어떤 알파벳의 숫자가 가장 큰지 확인하세요.
sort(table(cause_cats_posCas), decreasing = TRUE)

#18
# 1. Casualty가 최대인 사고의 행(row)을 찾습니다.
worst_accident <- totacts_posCas_nd[which.max(totacts_posCas_nd$Casualty), ]

# 2. 그 사고의 원인(CAUSE) 첫 글자를 확인합니다.
# (T, H, M, S, E 중 하나가 출력됩니다)
substr(worst_accident$CAUSE, 1, 1)

#19
# 인명 피해 사고에 연루된 열차 종류별 빈도 확인
eq_counts <- table(totacts_posCas_nd$TYPEQ)

# 빈도가 높은 순서대로 정렬 (코드 1번이 압도적일 것입니다)
sort(eq_counts, decreasing = TRUE)

#20
# totacts_posCas_nd 데이터셋의 Casualty 컬럼 합계 계산
total_casualties <- sum(totacts_posCas_nd$Casualty)

# 결과 출력
print(total_casualties)

#21
# 1. 인명 피해가 가장 큰 사고 행 추출
extreme_acc <- totacts_posCas_nd[which.max(totacts_posCas_nd$Casualty), ]

# 2. 보기 내용 확인 (사망자 수, 열차 종류, 시간 등)
extreme_acc$TOTKLD   # 1명 사망 확인
extreme_acc$TYPEQ    # 1(Freight) 확인 -> Commuter(3)가 아니므로 거짓!
extreme_acc$AMPM     # "AM" 확인
extreme_acc$CARSHZD   # 유해물질 유출 차량 수 확인

#22
# which.max()를 사용해 해당 사고의 행 번호를 찾아 제외(-)합니다.
totacts_posCas_nd_noMax <- totacts_posCas_nd[-which.max(totacts_posCas_nd$Casualty), ]

# [2] 분석할 변수들만 선택
vars_to_check <- c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")

# [3] 상관행렬(Correlation Matrix) 계산
# 인명 피해(Casualty)와 나머지 변수들 간의 상관계수를 확인합니다.
cor_matrix <- cor(totacts_posCas_nd_noMax[, vars_to_check])
print(cor_matrix["Casualty", ])

# [4] 산점도 행렬 시각화
pairs(totacts_posCas_nd_noMax[, vars_to_check], 
      main = "Scatter Plot Matrix (without Max Outlier)",
      pch = 20, col = "darkgreen")

#23
# [1] 분석할 변수 선택
vars <- c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")

# [2] PCA 수행 (표준화 포함 -> 상관행렬 기반)
pca_rail <- prcomp(totacts_posCas_nd_noMax[, vars], scale. = TRUE)

# [3] 로딩값(Loadings/Rotation) 확인
# 여기서 PC1 열의 숫자들을 절대값(Magnitude) 기준으로 비교하면 됩니다.
pca_rail$rotation[, "PC1"]

# 절대값으로 정렬해서 보기
sort(abs(pca_rail$rotation[, "PC1"]), decreasing = TRUE)

#24
# [1] PC2의 로딩값들만 확인
pca_rail$rotation[, "PC2"]

# [2] 절대값(Magnitude) 기준으로 가장 큰 변수 찾기
# 부호(+/-)에 상관없이 0에서 가장 멀리 떨어진 변수를 찾습니다.
sort(abs(pca_rail$rotation[, "PC2"]), decreasing = TRUE)

#25
# PCA 결과 요약 출력
summary(pca_rail)

# --- 라이브러리 로드 ---
library(dplyr)
library(ggplot2)
# 폴더 내 CSV 파일을 안전하게 읽어오는 함수
file.inputl <- function(my.path) {
  # 1. 해당 경로의 모든 .csv 파일 목록을 전체 경로로 가져옵니다.
  # pattern="\\.csv$" 는 파일 이름이 .csv로 끝나는 것만 정확히 필터링합니다.
  my.files <- list.files(path = my.path, pattern = "\\.csv$", full.names = TRUE)
  
  # 2. 파일이 없는 경우를 대비한 예외 처리
  if (length(my.files) == 0) {
    stop("오류: 해당 경로에 CSV 파일이 존재하지 않습니다: ", my.path)
  }
  
  # 3. 각 파일을 read.csv로 읽어 리스트에 저장합니다.
  acts <- lapply(my.files, read.csv)
  return(acts)
}
