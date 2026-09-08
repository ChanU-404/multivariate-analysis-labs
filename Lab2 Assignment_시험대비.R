#********************************************************************************
#             
#					Lab2 Assignment 관련된 코드들 다 넣어두기! - PCA.R 관련련
#
#********************************************************************************
#*

#********************************************************************************
#9번부터 코드 짜서 푸는 문제 시작

#9번~11번 공통
# 시험장 응용 포인트: 교수님은 극단적 사고(xdmg)에 대해서만 변수를 만드셨지만, 문제에서는 "전체 데이터(totacts)"에 대해 만들라고 지시했습니다. 따라서 데이터 이름과 변수명(Casualty, 단수형)만 살짝 바꿔주면 됩니다.
# 1. 계산 전 확실하게 숫자형으로 변환 (에러 방지용)
totacts$TOTKLD <- as.numeric(as.character(totacts$TOTKLD))
totacts$TOTINJ <- as.numeric(as.character(totacts$TOTINJ))

# 2. Casualty 파생 변수 생성
totacts$Casualty <- totacts$TOTKLD + totacts$TOTINJ

#9번
#문제 목표: 전체 철도 사고의 총 사상자(Casualty)에 대한 상자 그림을 그리고 결과 해석하기.
# ggplot을 이용한 상자 그림 그리기
ggplot(totacts, aes(y=Casualty)) + geom_boxplot()

# (꿀팁) 시험장에서 더 빠르게 눈으로만 확인하고 싶다면 Base R 함수 사용
boxplot(totacts$Casualty, main="Boxplot of Casualties")
summary(totacts$Casualty) # 구체적인 4분위수 수치 확인

#10번 & 11번
# Q10: 사상자가 1명 이상인 사고의 **비율(proportion)**은 얼마인가?
# Q11: 사상자가 1명 이상인 사고의 **총 건수(count)**는 몇 건인가?

# Q11 대비 (건수 구하기): Casualty가 1 이상인 사고가 몇 개인지 세기
# TRUE는 1로 계산되므로, sum()을 쓰면 조건에 맞는 건수가 바로 튀어나옵니다.
count_casualties <- sum(totacts$Casualty >= 1, na.rm = TRUE)
print(count_casualties) 
# -> 콘솔에 4369라는 정답이 바로 출력됩니다.

# Q10 대비 (비율 구하기): 조건에 맞는 건수를 전체 행 개수(nrow)로 나누기
prop_casualties <- count_casualties / nrow(totacts)

# 보기 편하게 퍼센트(%)로 변환해서 출력
cat("사상자 발생 사고 비율:", round(prop_casualties * 100, 2), "%\n")
# -> 콘솔에 5.82%가 출력되므로, "5% 이상 6% 미만" 보기를 바로 고를 수 있습니다.
#********************************************************************************



#********************************************************************************
#12번부터
# 문제 목표: 사상자가 1명 이상인 사고(totacts_posCas)를 만들고, 특정 변수들을 기준으로 중복을 제거한 데이터(totacts_posCas_nd)를 만든 뒤, 제거된 중복 보고서 개수 구하기.
# PCA.R에서 Remove Duplicates 파트와 동일함
# 스크립트 원본: xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# 1. 1명 이상 사상자 데이터 생성
totacts_posCas <- subset(totacts, Casualty >= 1)

# 2. 교수님 코드 응용: 중복이 제거된 데이터 생성
totacts_posCas_nd <- totacts_posCas[!(duplicated(totacts_posCas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])), ]

# 3. 정답(제거된 개수) 도출
nrow(totacts_posCas) - nrow(totacts_posCas_nd)


#13번
# 문제 목표: totacts_posCas_nd를 사용해 연도별 박스플롯을 그리고, 사상자 수가 두 번째로 많은(2nd largest) 사고의 연도 찾기.
# 사상자(Casualty) 기준 내림차순 정렬하여 상위 3개 연도 바로 확인
head(totacts_posCas_nd[order(-totacts_posCas_nd$Casualty), c("YEAR", "Casualty")], 3)

#14번 & 15번
# 문제 목표: * Q14: 전체 사고를 통틀어 사상자 '총합(sum)'이 가장 낮은 연도는?
# Q15: 사상자가 발생한 사고의 '건수(fewest number of accidents)'가 가장 적은 연도는?
# 교수님 스크립트 위치: PCA 스크립트보다는 데이터 전처리를 다루었던 기본 스크립트의 table() 함수와 tapply() 로직을 사용해야 가장 빠릅니다.

# Q14 대비 (총합 최하위 찾기)
aggregate(Casualty ~ YEAR, totacts_posCas_nd, sum) 
# -> 출력된 결과를 보고 합계 숫자가 가장 작은 연도 찾기

# Q15 대비 (건수 최하위 찾기)
sort(table(totacts_posCas_nd$YEAR))
# -> 가장 앞쪽에 뜨는(숫자가 가장 작은) 연도 찾기

# Question 16, 17, 19: 범주형 변수 1등 찾기 (Categorical Counts)
# 문제 목표: 사상자가 발생한 사고 건수가 가장 많은 TYPE(Q16), Cause(Q17), TYPEQ(Q19) 찾기.
# 교수님 스크립트 위치: 범주형(Categorical) 변수들의 빈도수를 비교하는 문제이므로, table()과 sort(..., decreasing = TRUE)를 쓰면 1초 만에 1등을 찾을 수 있습니다.

# Q16 대비: TYPE 1등 찾기
sort(table(totacts_posCas_nd$TYPE), decreasing = TRUE)

# Q17 대비: Cause 1등 찾기 (주의: 이전 스크립트에서 substr()로 대분류를 나눈 변수 사용)
sort(table(substr(totacts_posCas_nd$CAUSE, 1, 1)), decreasing = TRUE)

# Q19 대비: TYPEQ 1등 찾기
sort(table(totacts_posCas_nd$TYPEQ), decreasing = TRUE)

# Question 18 & 20: 특정 사고 원인 및 전체 총합
# 문제 목표: Q18: 사상자가 가장 많은(1등) 사고의 원인(Cause)은? / Q20: 전체 연도에 걸친 사상자의 총합(sum)은?
# 교수님 스크립트 위치: Q18은 PCA 스크립트에서 이상치를 다룰 때 특정 점(예: 5065번)을 뽑아내던 which 논리를 사용합니다. Q20은 단순 합산입니다.

# Q18 대비: 최대 사상자를 낸 사고의 원인(CAUSE 대분류 알파벳) 찾기
max_index <- which.max(totacts_posCas_nd$Casualty)
substr(totacts_posCas_nd$CAUSE[max_index], 1, 1)
# -> 출력되는 단일 알파벳(T, M, H 등)이 정답!

# Q20 대비: 중복 제거된 데이터 기준 사상자 전체 총합 구하기
sum(totacts_posCas_nd$Casualty, na.rm = TRUE)
#********************************************************************************


#21번부터~26번까지
#21번
# 문제 목표: 사상자가 가장 극단적으로 많았던 사고에 대한 설명 중 틀린 것(not true) 찾기./ 극단적 사고(이상치) 팩트 체크
# 스크립트 위치: 앞서 사용했던 "3. Investigating particular accidents" 로직과, PCA 스크립트에서 특정 이상치를 찾아내는 방식의 결합입니다.
# 1. Casualty가 가장 높은(1등) 사고의 위치(행 번호) 찾기
max_idx <- which.max(totacts_posCas_nd$Casualty)
worst_acc <- totacts_posCas_nd[max_idx, ]

# 2. 보기 1~4번 팩트 체크를 위해 해당 변수들 출력
cat("사망자(TOTKLD):", worst_acc$TOTKLD, "\n")
cat("열차종류(TYPEQ):", worst_acc$TYPEQ, "(1:화물, 2:여객, 3:통근)\n")
cat("오전/오후(AMPM):", worst_acc$AMPM, "\n")
cat("유해물질방출차량(HAZREL):", worst_acc$HAZREL, "\n")

#22번
# 문제 목표: 최악의 사고(1건)를 제거한 새로운 데이터를 만들고, Casualty와 다른 예측 변수들 간의 가장 높은 상관계수 찾기.
# 스크립트 위치: PCA_w_SVD_시험대비.R 스크립트의 하단부 # Remove outlier 파트에서 [-c(5065),]를 써서 특정 점을 날려버리는 로직과 완벽하게 일치합니다!

# 1. 스크립트 응용: 최악의 사고(max_idx) 행만 쏙 빼고(-) 새로운 데이터 만들기
totacts_posCas_nd_noMax <- totacts_posCas_nd[-max_idx, ]

# 2. 확실한 숫자형 변환 (에러 방지)
totacts_posCas_nd_noMax$Casualty <- as.numeric(as.character(totacts_posCas_nd_noMax$Casualty))
totacts_posCas_nd_noMax$TRNSPD <- as.numeric(as.character(totacts_posCas_nd_noMax$TRNSPD))

# 3. 상관계수 확인 (Casualty와 다른 변수들 간의 관계)
cor_matrix <- cor(totacts_posCas_nd_noMax[, c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")], use="complete.obs")

# 결과 중 Casualty 줄만 반올림해서 보기
round(cor_matrix["Casualty", ], 2)


#23번&24번
# 문제 목표: 상관 행렬 기반 PCA를 수행하고, PC1(Q23)과 PC2(Q24)에서 가장 큰 가중치(Loading)를 가진 변수 찾기.
# 스크립트 위치: PCA_w_SVD_시험대비.R 스크립트의 # Principal Components with the Correlation Matrix (함수: prcomp(..., scale = T)) 파트와 # Loadings plots comparison 파트입니다.
# 주의사항: 문제에서 'Correlation matrix'를 쓰라고 했으므로, 교수님 말씀대로 절대 cor=T가 아닌 **scale=T**를 써야 합니다!

# 1. 상관 행렬 기반 PCA 돌리기 (scale = T 필수!)
pca_model <- prcomp(totacts_posCas_nd_noMax[, c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")], scale = T)

# 2. Q23 대비: PC1의 Loadings 크기(절댓값) 내림차순 정렬
# (prcomp 결과에서 loadings는 rotation이라는 이름으로 저장됩니다)
sort(abs(pca_model$rotation[, 1]), decreasing = TRUE)

# 3. Q24 대비: PC2의 Loadings 크기(절댓값) 내림차순 정렬
sort(abs(pca_model$rotation[, 2]), decreasing = TRUE)

#25번&26번
# 문제 목표: 첫 두 PC의 누적 분산(Q25) 및 80%를 넘기 위한 최소 PC 개수(Q26) 찾기.
# 스크립트 위치: PCA_w_SVD_시험대비.R의 # Scree plot and cumulative variance comparison 파트입니다. 교수님은 그림에서 $cumvar를 뽑아내는 고급 코드를 쓰셨지만, 시험장에서는 summary() 함수 단 한 줄이면 2문제를 3초 만에 풉니다.

# 요약 통계량으로 분산 및 누적 분산 한눈에 보기
summary(pca_model)