#********************************************************************************
#             
#					Lab1 Assignment 관련된 코드들 다 넣어두기! - Uni, MultivarViz.R
#
#********************************************************************************
#*

#********************************************************************************
#8번부터 코드 짜서 푸는 문제 시작
# 문제 목표: 전체 사고(totacts)에 대해 최대 속도(HIGHSPD)의 히스토그램을 그리고, 관찰된 사실 중 맞는 보기를 고르시오.
# 교수님 스크립트 스타일의 ggplot 히스토그램
ggplot(totacts, aes(x = HIGHSPD)) + 
  geom_histogram(fill = "steelblue", bins = 30) + # bins(막대 개수)는 보기 편하게 임의로 설정
  ggtitle("Histogram of Maximum Reported Speed (HIGHSPD)") + 
  labs(x = "Speed (MPH)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# (시험장 꿀팁) 만약 그래프만 빨리 보고 싶다면 Base R의 기본 함수가 훨씬 빠릅니다.
hist(totacts$HIGHSPD, main="Histogram of HIGHSPD", xlab="MPH", col="steelblue")

# 전체 데이터의 요약 통계량 쓱 훑어보기
summary(totacts$HIGHSPD)

# 중앙값(Median)만 딱 뽑아보기 (결측치 제거 옵션 na.rm=TRUE 추가)
median(totacts$HIGHSPD, na.rm = TRUE)

# 전체 사고 건수 확인 (기준점)
nrow(totacts)

# 보기 1 & 4 검증: 10 MPH 미만인 사고의 개수 확인
totacts %>% filter(HIGHSPD < 10) %>% nrow()

# 보기 2 검증: 30 MPH를 초과하는 사고의 개수 확인
totacts %>% filter(HIGHSPD > 30) %>% nrow()

# 보기 3 검증: 100 MPH를 초과하는 사고의 개수 확인
totacts %>% filter(HIGHSPD > 100) %>% nrow()
#********************************************************************************

#9번 -> 9번~12번 / 14번 동일한 코드 사용
# 문제 목표: 2001년부터 모든 연도에 대한 장비 피해액(EQPDMG) 상자 그림(Box plot)을 그리고, 가장 극단적인(피해액이 큰) 사고들이 언제 일어났는지 파악하기.
# 4.2절 응용: 연도별 EQPDMG 박스플롯 그리기
ggplot(totacts, aes(x = as.factor(YEAR), y = EQPDMG)) + 
  geom_boxplot(fill = "steelblue") + 
  ggtitle("Boxplots of Equipment Damage by Year") +
  labs(x = "Year", y = "Equipment Damage ($)") + 
  theme(plot.title = element_text(hjust = 0.5))

# 3절 응용: EQPDMG 기준 내림차순 정렬하여 상위 5개 사고의 연도 확인
totacts %>% 
  select(YEAR, EQPDMG) %>% 
  arrange(desc(EQPDMG)) %>% 
  head(5)

# 3절 응용: EQPDMG 기준 내림차순 정렬하여 상위 5개 사고의 연도 확인
totacts %>% 
  select(YEAR, EQPDMG) %>% 
  arrange(desc(EQPDMG)) %>% 
  head(5)
#********************************************************************************

#********************************************************************************
# 10번
# 문제 목표: 2001년부터 모든 연도에 대한 총 사망자 수(TOTKLD) 상자 그림(Box plot)을 그리고, 관찰된 사실 중 맞는 보기를 고르시오.
# 스크립트 4.2절 응용: 연도별 TOTKLD 박스플롯 그리기
ggplot(totacts, aes(x = as.factor(YEAR), y = TOTKLD)) + 
  geom_boxplot(fill = "steelblue") + 
  ggtitle("Boxplots of Total Killed by Year") +
  labs(x = "Year", y = "Total Killed") + 
  theme(plot.title = element_text(hjust = 0.5))

# 스크립트 3절 응용: TOTKLD 기준 내림차순 정렬하여 상위 5개 사고의 연도 확인
totacts %>% 
  select(YEAR, TOTKLD) %>% 
  arrange(desc(TOTKLD)) %>% 
  head(5)

# 2004년(YEAR==4)과 2013년(YEAR==13)의 최대 사망자 수 각각 구하기
totacts %>% filter(YEAR == 4) %>% summarise(Max_Killed = max(TOTKLD, na.rm = TRUE))
totacts %>% filter(YEAR == 13) %>% summarise(Max_Killed = max(TOTKLD, na.rm = TRUE))
#********************************************************************************

#********************************************************************************
# 13번
# "3. Investigating particular accidents" 파트 중 "부상자(TOTINJ)가 가장 많았던 사고 파헤치기" 부분을 100% 활용하는 문제
# 스크립트 3절: "worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ))" 부분 활용
# 전체 데이터 중 TOTINJ가 가장 높은 행을 찾고, 그 행의 INCDTNO를 출력하라
totacts %>% 
  filter(TOTINJ == max(TOTINJ, na.rm = TRUE)) %>% 
  select(INCDTNO, TOTINJ, YEAR)

# 스크립트 3절: "max_totinj = which(totacts$TOTINJ == max(totacts$TOTINJ))" 부분 활용
max_totinj <- which(totacts$TOTINJ == max(totacts$TOTINJ, na.rm = TRUE))
totacts$INCDTNO[max_totinj]
#********************************************************************************


#15번
# nclass는 R에서 히스토그램을 그릴 때 막대(bin)의 개수를 계산하는 함수들의 묶음
# 1. 문제 조건에 맞춰 정확하게 ggplot 히스토그램 그리기
ggplot(totacts, aes(x = TEMP)) + 
  geom_histogram(bins = nclass.Sturges(totacts$TEMP), fill = NA, color = "steelblue") +
  ggtitle("Histogram of Temperature") +
  labs(x = "Temperature", y = "Frequency")

# 2. (시험장 꿀팁) 가장 빈도가 높은(가장 막대가 높은) 온도의 정확한 숫자 찾기
# table()로 빈도를 세고 sort(decreasing=TRUE)로 내림차순 정렬해서 1등 확인
sort(table(totacts$TEMP), decreasing = TRUE) %>% head(5)




#여기서부터 MultivarViz.R와 관련됨
#********************************************************************************
# 16번~18번
# 스크립트의 "3.1 scatter plots" 파트
# 문제의 지시사항에 따라 psych 패키지를 이용해 산점도 행렬(Scatter plot matrix)을 그리고, 화면이 좁아 깨지는 것을 방지하기 위해 PNG 파일로 저장하는 코드입니다.
# 1. psych 패키지 로드 (없다면 install.packages("psych") 먼저 실행)
library(psych)

# 2. 문제에서 요구한 5개 변수만 쏙 뽑아서 새로운 데이터 프레임 만들기
pairs_data <- totacts %>% select(TRKDMG, EQPDMG, ACCDMG, TOTINJ, TOTKLD)

# 3. 해상도 높은 PNG 파일로 저장하기 (실행 후 작업 폴더에 이미지 파일이 생성됩니다)
png("scatter_plot_matrix.png", width = 1000, height = 1000)
pairs.panels(pairs_data, 
             method = "pearson", # 피어슨 상관계수
             hist.col = "steelblue", 
             density = TRUE,  # 밀도 곡선 표시
             ellipses = FALSE) # 보기 편하게 타원형 제거
dev.off() # 그림 그리기 종료 및 파일 저장

#********************************************************************************
#19번~24번
# 패턴 1. "가장 심한(최대) 사고의 정보 캐내기" (Q19, Q20, Q21, Q24)
# 다시 UnivarViz.R 관련
# 이 유형은 UnivarViz_시험대비.R의 "3. Investigating particular accidents" 파트에 있는 코드를 100% 활용합니다. 전체 데이터(totacts)에서 최댓값을 가진 1등 사고만 핀셋으로 집어내어 worst_dmg나 worst_inj라는 변수에 저장해두고, 그 사고의 세부 정보를 조회하는 방식입니다.
# 1. ACCDMG(피해액) 1등 사고만 뽑아서 'worst_dmg' 서랍에 넣기 (Q19, Q20, Q21 대비)
worst_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG, na.rm = TRUE))

# Q19: 그 사고 연도는?
worst_dmg$YEAR

# Q20: 그 사고의 종류(TYPE)와 세부 내역은? 
worst_dmg$TYPE
# (Q20 보기 중 "two different railroads" 관련 확인을 위해서는 worst_dmg 전체를 열어보거나 JOINTCD 같은 변수를 확인하면 됩니다.)

# Q21: 그 사고의 정확한 최대 피해액 숫자는?
worst_dmg$ACCDMG

# ---------------------------------------------------------
# 2. TOTINJ(부상자) 1등 사고만 뽑아서 'worst_inj' 서랍에 넣기 (Q24 대비)
worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ, na.rm = TRUE))

# Q24: 최대 부상자 숫자는?
worst_inj$TOTINJ
#********************************************************************************


#********************************************************************************
#25번~26번번
# 패턴 2. "특정 조건을 만족하는 사고 건수(Count) 세기" (Q25, Q26)
# UnivarViz.R의 3.Investigating particular accidents 파트 참고
# "피해액이 1.5M($1,500,000) 이상인 사고는 몇 건?", "사망자가 1명 이상인 사고는 몇 건?"처럼 조건을 주고 개수를 묻는 문제입니다. 교수님 스크립트 전반에 깔려있는 **dplyr 패키지의 파이프라인(%>%)과 filter()**를 조합하면 그래프를 그릴 필요 없이 1초 만에 숫자가 나옵니다.
# Q25: ACCDMG가 1,500,000 보다 큰 데이터만 필터링한 뒤, 행 개수(nrow) 세기
totacts %>% filter(ACCDMG > 1500000) %>% nrow()

# Q26: TOTKLD(사망자)가 1 이상인 데이터만 필터링한 뒤, 행 개수 세기
totacts %>% filter(TOTKLD >= 1) %>% nrow()
#********************************************************************************


#********************************************************************************
# 패턴 3. "카테고리별 순위 매기기 (Barplot)" (Q27, Q28)
# 이 문제들은 UnivarViz_시험대비.R의 **"4.5 Bar plot of Accident Types"**와 MultivarViz_시험대비.R의 "2. More Data Cleaning" 파트에 완벽하게 구현되어 있습니다. 데이터 클리닝으로 예쁘게 이름표(labels)를 붙인 TYPE(사고 종류)과 Cause(원인 대분류) 변수를 활용합니다.
 
# 문제에서는 barplot()을 그려서 눈으로 확인하라고 했지만, 시험장에서는 그래프 렌더링을 기다릴 필요 없이 **table()과 sort()**를 조합하는 것이 시간 단축의 핵심입니다.

# Q27 대비: 사고 종류(TYPE) 빈도수 내림차순 정렬
# (가장 많이 발생한 1등 TYPE이 맨 앞에 출력됨)
sort(table(totacts$TYPE), decreasing = TRUE)

# 원본 스크립트대로 그래프를 보고 싶다면:
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") + coord_flip()

# ---------------------------------------------------------
# Q28 대비: 사고 원인(Cause) 빈도수 내림차순 정렬
# (가장 많이 발생한 1등 Cause 알파벳이 맨 앞에 출력됨)
sort(table(totacts$Cause), decreasing = TRUE)

# 원본 스크립트대로 막대그래프를 보고 싶다면:
ggplot(as.data.frame(table(totacts$Cause)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")
#********************************************************************************