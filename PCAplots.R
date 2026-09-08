# ======================================================
# PCA 시각화 개선을 위한 사용자 정의 함수 모음
# (ggplot2 기반의 고퀄리티 그래프 생성용)
# ======================================================

# 1. 필요 라이브러리 로드
library(ggplot2)    # 시각화 핵심 패키지
library(data.table) # 데이터 프레임 핸들링 (setDT 함수용)
library(plyr)       # 데이터 변형용
library(scales)     # 축 눈금 제어
library(grid)       # 그래프 레이아웃 제어
library(ggpubr)     # 여러 그래프를 하나로 합치는 용도 (ggarrange)

# ------------------------------------------------------
# 함수 1: loadingsplot
# 역할: 각 변수가 PC1, PC2를 만드는 데 얼마나 기여했는지 막대그래프로 보여줌
# ------------------------------------------------------
loadingsplot <- function(pca.obj) {
  # PCA 객체 타입(princomp vs prcomp)에 따라 로딩값 추출 방식 결정
  if ("loadings" %in% names(pca.obj)) { # princomp 객체일 경우
    m = ncol(pca.obj$loadings)
    df = as.data.frame(pca.obj$loadings[, 1:m])
    setDT(df, keep.rownames = TRUE)[]
    names(df)[names(df) == "rn"] <- "Variable"
    # PC1, PC2 막대그래프 생성
    PC1 <- ggplot(data = df, aes(x = Variable, y = Comp.1)) + geom_bar(stat = "identity") + ggtitle("PC1 Loadings")
    PC2 <- ggplot(data = df, aes(x = Variable, y = Comp.2)) + geom_bar(stat = "identity") + ggtitle("PC2 Loadings")
  } else { # prcomp 객체일 경우
    m = ncol(pca.obj$rotation)
    df = as.data.frame(pca.obj$rotation[, 1:m])
    setDT(df, keep.rownames = TRUE)[]
    names(df)[names(df) == "rn"] <- "Variable"
    PC1 <- ggplot(data = df, aes(x = Variable, y = PC1)) + geom_bar(stat = "identity") + ggtitle("PC1 Loadings")
    PC2 <- ggplot(data = df, aes(x = Variable, y = PC2)) + geom_bar(stat = "identity") + ggtitle("PC2 Loadings")
  }
  # 두 그래프를 위아래로 배치하여 리스트로 반환
  g <- ggarrange(PC1, PC2, ncol = 1, nrow = 2)
  return(list(loadings = df, plot = g))
}

# ------------------------------------------------------
# 함수 2: ggscreeplot
# 역할: 주성분별 분산 비율을 막대그래프로 시각화 (엘보우 지점 확인용)
# ------------------------------------------------------
ggscreeplot <- function(pca.obj, ...) {
  # 각 주성분의 분산 비율 계산 (고유값 / 전체 합)
  x <- pca.obj$sdev^2 / sum(pca.obj$sdev^2)
  x <- as.data.frame(x)
  setDT(x, keep.rownames = TRUE)[]
  names(x)[names(x) == "rn"] <- "Component"
  names(x)[names(x) == "x"] <- "Proportion"
  # 스크리 플롯 생성
  g <- ggplot(data = x, aes(x = factor(Component, level = x$Component), y = Proportion)) +
    geom_bar(stat = "identity") + ggtitle("Scree Plot")
  return(list(var = x, plot = g))
}

# ------------------------------------------------------
# 함수 3: cumplot
# 역할: 주성분 추가에 따른 누적 분산 설명력 시각화 (80% 지점 확인 등)
# ------------------------------------------------------
cumplot <- function(pca.obj, ...) {
  # 누적합(cumsum) 계산
  xc <- cumsum(pca.obj$sdev^2) / sum(pca.obj$sdev^2)
  xc <- as.data.frame(xc)
  setDT(xc, keep.rownames = TRUE)[]
  names(xc)[names(xc) == "rn"] <- "Component"
  names(xc)[names(xc) == "xc"] <- "Proportion"
  # 누적 그래프 생성
  g <- ggplot(data = xc, aes(x = factor(Component, level = xc$Component), y = Proportion)) +
    geom_bar(stat = "identity") + ggtitle("Cumulative Variance in the PCs")
  return(list(cumvar = xc, plot = g))
}

# ------------------------------------------------------
# 함수 4: ggbiplot
# 역할: 관측치(점)와 변수(화살표)를 동시에 그리는 고급 바이플롯
# ------------------------------------------------------
ggbiplot <- function (pcobj, choices = 1:2, scale = 1, plot.obs = TRUE, xlim = FALSE, ylim = FALSE,
                      pc.biplot = TRUE, obs.scale = 1 - scale, var.scale = scale, 
                      groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                      labels = NULL, labels.size = 3, alpha = 1, var.axes = TRUE, 
                      circle = FALSE, circle.prob = 0.69, 
                      varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE, ...) {
  
  # 1. 객체 타입에 따른 좌표(u) 및 회전값(v) 계산 로직
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  } else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  } # (이하 생략 - PCA, lda 타입 지원 로직)
  
  # 2. 축 이름 설정 (설명력 % 포함)
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  
  u.axis.labs <- paste("PC", choices, sep = "")
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2 / sum(pcobj$sdev^2)))
  
  # 3. 그래프 기본 레이아웃 및 텍스트/화살표 시각화 설정
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
    ylab(u.axis.labs[2]) + coord_equal()
  
  # 관측치(선수 번호 등) 표시
  if (plot.obs) {
    if (!is.null(labels)) {
      df.u$labels <- labels
      g <- g + geom_text(aes(label = labels), size = labels.size)
    } else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  
  # 변수 화살표 및 이름 표시
  if (var.axes) {
    df.v$varname <- rownames(v)
    df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
    
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, xend = xvar, yend = yvar), 
                          arrow = arrow(length = unit(1/2, "picas")), color = muted("red"))
    g <- g + geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
                       color = "darkred", size = varname.size)
  }
  return(g)
}

