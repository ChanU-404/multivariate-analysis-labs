#************************************************************
#
#					세션 5
#				
#		특이값 분해(SVD)를 이용한 주성분 분석(PCA)
#
#************************************************************


#***********************************************************
#
#			필요한 라이브러리 설치 및 로드
#
#***********************************************************

# 시각화를 위한 ggplot2와 심리통계 분석을 위한 psych 라이브러리 로드
library(ggplot2)
library(psych)

# PCAplots.R 내의 ggbiplot 및 loadingsplot 함수 실행에 필요한 라이브러리들
library(data.table)
library(plyr)
library(scales)
library(grid)
library(ggpubr) # 여러 그래프를 한 화면에 배치하기 위해 필요

#***********************************************************
#
#			데이터 로드 및 형식 설정
#
#***********************************************************

# 분석 스크립트와 PCAplots.R 등이 있는 폴더 경로
sourcedir <- "~/Desktop/다변량_실습파일"

# 실제 사고 데이터 CSV 파일들이 들어있는 폴더 경로
traindir <- "~/Desktop/다변량_실습파일/Train Data"

# 작업 디렉토리 설정
setwd(sourcedir)
getwd() # 현재 경로 확인

# 보조 함수 스크립트들을 불러옵니다.
source("AccidentInput.R")
source("PCAplots.R")

# 이제 메모리에 두 가지 데이터 구조가 있어야 합니다.
# 1. acts: 각 연도별 사고 데이터프레임이 담긴 리스트
acts <- file.inputl(traindir)

# 2. totacts: 2001년부터 2019년(혹은 전체 연도)까지의 모든 사고를 합친 데이터프레임
totacts <- combine.data(acts)

#***********************************************************
#
#			극단적 사고 데이터 추출 (Extreme accident data)
#
#***********************************************************

# 사고 피해액(ACCDMG)의 분포를 박스플롯으로 확인합니다.
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# 박스플롯의 상단 수염(upper whisker)보다 위에 있는 극단적인 값들만 찾아냅니다.
# ggplot_build를 사용하면 그래프 계산 값에 접근할 수 있습니다.
upper <- ggplot_build(dmgbox)$data[[1]]$ymax
xdmg <- totacts[totacts$ACCDMG > upper,]

# 사상자 수(Casualties) 변수 생성 (부상자 + 사망자)
xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# [참고] 데이터 중 9/11 테러 관련 사고(특수 사례)를 분석에서 제외합니다.
xdmg <- xdmg[-102,]

#***********************************************************
#
#			중복 데이터 제거
#
#***********************************************************

# 사고 번호, 연, 월, 일, 시간 등이 완전히 동일한 중복 행을 제거합니다.
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# 행 이름을 다시 설정합니다. (중간에 행이 빠져서 번호가 어긋난 것을 1부터 순차적으로 재정렬)
rownames(xdmgnd) <- NULL

#***********************************************************
#
#		주성분 분석 (PCA) 실행
#
#***********************************************************
# prcomp 함수의 도움말을 확인합니다.
?prcomp


#***********************************************************
#
#		공분산 행렬(Covariance Matrix)을 이용한 PCA
#
#***********************************************************
# 데이터를 표준화하지 않고 원본 값 그대로 공분산을 사용합니다.
# 변수들의 단위나 규모가 다르면 결과가 왜곡될 수 있습니다.
xdmgnd.pca.cov <- prcomp(xdmgnd[,c("CARSDMG","EQPDMG","TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

#***********************************************************
#
#		상관 행렬(Correlation Matrix)을 이용한 PCA	
#
#***********************************************************

# scale = T 옵션을 주어 데이터를 표준화(평균 0, 분산 1)한 후 상관 행렬을 사용합니다.
# 단위가 다른 여러 변수를 함께 분석할 때 주로 사용하는 방식입니다.
xdmgnd.pca.corr <- prcomp(xdmgnd[,c("CARSDMG","EQPDMG","TRKDMG","ACCDMG","TOTKLD","TOTINJ")], 
                          scale = T)


#***********************************************************
#
#		바이플롯(Biplot) 비교
#
#***********************************************************

# 첫 2개의 주성분(PC1, PC2) 공간에 데이터를 시각화합니다.

# 기본 R 그래프 기능을 이용한 비교 (1행 2열 배치)
par(mfrow=c(1,2))
biplot(xdmgnd.pca.cov, main="공분산 행렬을 이용한 바이플롯")
biplot(xdmgnd.pca.corr, main="상관 행렬을 이용한 바이플롯")
par(mfrow=c(1,1)) # 설정 초기화

# ggbiplot 패키지를 이용한 더 깔끔한 시각화
cov_biplot <- ggbiplot(xdmgnd.pca.cov, varname.size = 5, labels=row(xdmgnd)[,1])
corr_biplot <- ggbiplot(xdmgnd.pca.corr, varname.size = 5, labels=row(xdmgnd)[,1])
ggarrange(cov_biplot, corr_biplot, ncol=2, nrow=1)

# 관측치(점)는 제외하고 변수 벡터(화살표)만 시각화하여 변수 간 관계 파악
cov_biplot <- ggbiplot(xdmgnd.pca.cov, varname.size = 1, labels=row(xdmgnd)[,1],
                       plot.obs=FALSE, xlim=c(-0.3,3), ylim=c(-1.5,1.5))
corr_biplot <- ggbiplot(xdmgnd.pca.corr, varname.size = 1, labels=row(xdmgnd)[,1],
                        plot.obs=FALSE, xlim=c(-0.3,2.5), ylim=c(-1.5,2))
ggarrange(cov_biplot, corr_biplot, ncol=2, nrow=1)


#***********************************************************
#
#		스크리 플롯(Scree plot) 및 누적 분산 비교
#
#***********************************************************
# 각 주성분이 전체 데이터의 분산을 얼마나 설명하는지 보여줍니다.

cov_scree <- ggscreeplot(xdmgnd.pca.cov)
corr_scree <- ggscreeplot(xdmgnd.pca.corr)

# 상관 행렬 PCA의 개별 분산 설명력 수치 확인
corr_scree$var

ggarrange(cov_scree$plot, corr_scree$plot, ncol=2, nrow=1)

# 누적 분산 그래프 (주성분을 추가할 때마다 설명력이 어떻게 늘어나는지 확인)

cov_cumsum <- cumplot(xdmgnd.pca.cov)
corr_cumsum <- cumplot(xdmgnd.pca.corr)

# 상관 행렬 PCA의 누적 분산 설명력 수치 확인
corr_cumsum$cumvar

ggarrange(cov_cumsum$plot, corr_cumsum$plot, ncol=2, nrow=1)

#***********************************************************
#
#		로딩 플롯(Loadings plots) 비교
#
#***********************************************************
# 각 주성분에 어떤 원래 변수가 강하게 기여(loading)하고 있는지 확인합니다.

loadplot.cov <- loadingsplot(xdmgnd.pca.cov)
loadplot.cov$loadings # 수치 확인

loadplot.corr <- loadingsplot(xdmgnd.pca.corr)
loadplot.corr$loadings # 수치 확인

# 첫 번째 열은 공분산 행렬 사용 시, 두 번째 열은 상관 행렬 사용 시의 결과입니다.
ggarrange(loadplot.cov$plot, loadplot.corr$plot, ncol=2, nrow=1)


#***********************************************************
#
#		사고 피해액의 잠재적 예측 변수 분석 (Possible predictors of damage)
#
#***********************************************************

# 산점도 행렬(SPM)을 통해 변수 간의 관계를 한눈에 파악합니다.
# 피해액, 열차 속도, 차량 수, 시간(Hour), 온도(Temp) 간의 관계
pairs.panels(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# 위 변수들로 PCA를 수행합니다 (단위가 다르므로 반드시 scale = T 사용).
pred.pca <- prcomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], scale = T )

# 기본 바이플롯
biplot(pred.pca)

# ggbiplot 시각화
ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])

# --- 이상치 제거 1단계 ---
# 위 그래프에서 매우 멀리 떨어진 5065번 관측치를 제거해 봅니다.
xdmgnd_no <- xdmgnd[-c(5065),]
rownames(xdmgnd_no) <- NULL
pred_no.pca <- prcomp(xdmgnd_no[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], scale = T )

biplot(pred_no.pca)
ggbiplot(pred_no.pca, varname.size = 2, labels=row(xdmgnd_no)[,1])

# [문제/질문] Do your conclusions change? 
# 번역: (이상치를 하나 제거한 후) 당신의 결론이 바뀌었나요?

# --- 이상치 군집 제거 2단계 ---
# 눈에 띄는 여러 개의 이상치들을 한꺼번에 제거해 봅니다.
xdmgnd_no <- xdmgnd[-c(5065,5496,5084,5897,6998,5497,6996),]
rownames(xdmgnd_no) <- NULL
pred_no.pca <- prcomp(xdmgnd_no[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], scale = T )

biplot(pred_no.pca)

# gg biplot 시각화
ggbiplot(pred_no.pca, varname.size = 5, labels=row(xdmgnd_no)[,1])

# [문제/질문] Do your conclusions change?
# 번역: (추가적인 이상치들을 제거한 후) 이제 당신의 결론이 바뀌었나요?