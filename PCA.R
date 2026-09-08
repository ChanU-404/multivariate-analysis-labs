#************************************************************
#
#					세션 5
#				
#				주성분 분석 (Principal Components)
#
#************************************************************


#***********************************************************
#
#			필요한 라이브러리 설치 및 로드
#
#***********************************************************

# 시각화를 위한 ggplot2와 심리 통계 및 PCA 분석 보조를 위한 psych 라이브러리 로드
library(ggplot2)
library(psych)

# PCAplots.R 스크립트 내의 ggbiplot, loadingsplot 함수 실행에 필요한 라이브러리들
library(data.table)
library(plyr)
library(scales)
library(grid)
library(ggpubr) # 여러 그래프를 한 화면에 배치(ggarrange)하기 위해 필요

#***********************************************************
#
#			데이터 로드 및 형식 설정
#
#***********************************************************

# 데이터가 저장된 폴더 경로 (사용자 환경에 맞춰 수정 필요)
traindir <- "/Users/seokhyunchung/Library/CloudStorage/Dropbox/Korea University/2. Teaching/2026S/IMEN415 Multivariate Analysis/2. R practice/data/Train Data"
sourcedir <-"/Users/seokhyunchung/Library/CloudStorage/Dropbox/Korea University/2. Teaching/2026S/IMEN415 Multivariate Analysis/2. R practice/2. PCA"

# 작업 디렉토리를 소스 파일 폴더로 설정
setwd(sourcedir)

# 데이터 입력 및 PCA 시각화 보조 함수들을 불러옵니다.
source("AccidentInput.R")
source("PCAplots.R")

# 현재 메모리에 두 가지 데이터 구조가 생성되어야 합니다:
# 1. acts: 연도별 사고 데이터프레임들이 담긴 리스트
acts <- file.inputl(traindir)

# 2. totacts: 2001년부터 2019년까지의 모든 데이터를 하나로 합친 데이터프레임
totacts <- combine.data(acts)

#***********************************************************
#
#			극단적 사고 데이터 추출 (Extreme accident data)
#
#***********************************************************

# 사고 피해액(ACCDMG)의 분포를 박스플롯으로 확인합니다.
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# 박스플롯의 상단 수염(upper whisker)보다 높은 곳에 위치한 극단적 피해 데이터만 추출합니다.
upper <- ggplot_build(dmgbox)$data[[1]]$ymax
xdmg <- totacts[totacts$ACCDMG > upper,]

# 사상자 수(Casualties) 변수 생성 (부상자수 + 사망자수)
xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# 9/11 테러 관련 특수 사고 데이터를 분석의 일관성을 위해 제외합니다.
xdmg <- xdmg[-102,]


#***********************************************************
#
#			중복 데이터 제거
#
#***********************************************************

# 사고 번호, 연, 월, 일, 시간 등이 겹치는 중복 관측치를 제거하여 데이터 정밀도를 높입니다.
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# 행 이름을 초기화하여 1부터 순차적으로 번호를 다시 매깁니다. (분석 중 행번호 참조를 위해 중요)
rownames(xdmgnd) <- NULL

#***********************************************************
#
#		공분산 행렬(Covariance Matrix)을 이용한 PCA
#
#***********************************************************

# 원본 데이터의 척도를 그대로 유지한 채 공분산 행렬로 PCA를 수행합니다.
# 변수들의 단위(예: 달러 vs 명수) 차이가 크면 값이 큰 변수가 주성분을 압도합니다.
xdmgnd.pca.cov <- princomp(xdmgnd[,c("CARSDMG","EQPDMG","TRKDMG",
                                     "ACCDMG", "TOTKLD", "TOTINJ")])

#***********************************************************
#
#		상관 행렬(Correlation Matrix)을 이용한 PCA	
#
#***********************************************************

# cor = T 옵션을 주어 데이터를 표준화(상관 행렬 사용)한 뒤 PCA를 수행합니다.
# 변수들의 단위가 다를 때(피해액과 부상자 수 등) 공정한 비교를 위해 사용합니다.
xdmgnd.pca.corr <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG",
                                      "ACCDMG", "TOTKLD", "TOTINJ")], 
                            cor = T)

#***********************************************************
#
#		바이플롯(Biplot) 비교
#
#***********************************************************

# 첫 2개의 주성분(PC1, PC2) 공간에 데이터와 변수를 시각화합니다.

# 기본 R 기능을 이용한 비교 (왼쪽: 공분산, 오른쪽: 상관행렬)
par(mfrow=c(1,2))
biplot(xdmgnd.pca.cov, main="공분산 행렬 바이플롯")
biplot(xdmgnd.pca.corr, main="상관 행렬 바이플롯")
par(mfrow=c(1,1)) # 그래프 설정 초기화

# ggbiplot 패키지를 사용해 더 깔끔하게 시각화
cov_biplot <- ggbiplot(xdmgnd.pca.cov, varname.size = 5, labels=row(xdmgnd)[,1])
corr_biplot <- ggbiplot(xdmgnd.pca.corr, varname.size = 5, labels=row(xdmgnd)[,1])
ggarrange(cov_biplot, corr_biplot, ncol=2, nrow=1)

# 데이터 점(obs)은 빼고 변수 화살표(vector)의 방향만 확인하여 변수 간 관계 파악
cov_biplot <- ggbiplot(xdmgnd.pca.cov, varname.size = 5, labels=row(xdmgnd)[,1],
                       plot.obs=FALSE, xlim=c(-0.3,3), ylim=c(-1.5,1.5))
corr_biplot <- ggbiplot(xdmgnd.pca.corr, varname.size = 5, labels=row(xdmgnd)[,1],
                        plot.obs=FALSE, xlim=c(-0.3,2.5), ylim=c(-1.5,2))
ggarrange(cov_biplot, corr_biplot, ncol=2, nrow=1)


#***********************************************************
#
#		스크리 플롯(Scree plot) 및 누적 분산 설명력 비교
#
#***********************************************************

# 각 주성분이 전체 데이터의 변동성(분산)을 얼마나 설명하는지 그래프로 확인합니다.
cov_scree <- ggscreeplot(xdmgnd.pca.cov)
corr_scree <- ggscreeplot(xdmgnd.pca.corr)

# 상관 행렬 PCA에서 각 주성분의 분산 비율 확인
corr_scree$var

ggarrange(cov_scree$plot, corr_scree$plot, ncol=2, nrow=1)

# 누적 분산 그래프 (주성분을 합칠수록 데이터 설명력이 어떻게 늘어나는지 확인)
cov_cumsum <- cumplot(xdmgnd.pca.cov)
corr_cumsum <- cumplot(xdmgnd.pca.corr)

# 누적 분산 값 수치 확인
corr_cumsum$cumvar

ggarrange(cov_cumsum$plot, corr_cumsum$plot, ncol=2, nrow=1)

#***********************************************************
#
#		로딩 플롯(Loadings plots) 비교
#
#***********************************************************

# 각 주성분에 어떤 변수가 얼마나 강하게 기여하고 있는지 확인합니다.
loadplot.cov <- loadingsplot(xdmgnd.pca.cov)
loadplot.cov$loadings # 공분산 기반 기여도

loadplot.corr <- loadingsplot(xdmgnd.pca.corr)
loadplot.corr$loadings # 상관행렬 기반 기여도

# 두 방식의 로딩 플롯을 한 번에 비교합니다.
ggarrange(loadplot.cov$plot, loadplot.corr$plot, ncol=2, nrow=1)


#***********************************************************
#
#		사고 피해의 잠재적 예측 변수 분석 (Predictors of damage)
#
#***********************************************************

# 산점도 행렬(SPM)을 통해 피해액, 속도, 차량 수, 시간, 온도 간의 관계 확인
pairs.panels(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# 상관 행렬 기반 PCA 수행 (예측 변수들의 단위가 다르므로 cor=T 권장)
pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

# 바이플롯 시각화
biplot(pred.pca)
ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])

# [이상치 제거 1단계]
# 바이플롯에서 극단적으로 떨어져 있는 5065번 데이터를 제거합니다.
xdmgnd_no <- xdmgnd[-c(5065),]
rownames(xdmgnd_no) <- NULL
pred_no.pca <- princomp(xdmgnd_no[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

# 이상치 제거 후 결과 다시 확인
biplot(pred_no.pca)
ggbiplot(pred_no.pca, varname.size = 5, labels=row(xdmgnd_no)[,1])

# [질문] Do your conclusions change? 
# 번역: (이상치 하나를 지운 후) 당신의 결론이 바뀌었나요?

# [이상치 제거 2단계 - 클러스터 제거]
# 영향을 크게 미치는 여러 개의 이상치들을 한꺼번에 제거해 봅니다.
xdmgnd_no <- xdmgnd[-c(5065,5496,5084,5897,6998,5497,6996),]
rownames(xdmgnd_no) <- NULL
pred_no.pca <- princomp(xdmgnd_no[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

# 제거 후 시각화
biplot(pred_no.pca)
ggbiplot(pred_no.pca, varname.size = 5, labels=row(xdmgnd_no)[,1])

# [질문] Do your conclusions change?
# 번역: (여러 이상치를 지운 후) 당신의 결론이 바뀌었나요?