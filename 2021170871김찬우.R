# 수정 전: sourcedir <-"****Change this to your file path****"

# 수정 후 (예시): 자신의 실제 계정 이름을 넣어주세요.
sourcedir <- "~/Desktop/다변량_실습파일" 

# 만약 한글 폴더명이 인식이 안 된다면 전체 경로를 써보세요.
# sourcedir <- "/Users/본인계정이름/Desktop/다변량_실습파일"

opts_knit$set(root.dir = sourcedir)
# 데이터 불러오기
class.data <- read.csv("Country-data.csv")

# 데이터 확인
summary(class.data)

# ggplot2 패키지 로드 (설치 안 되어 있을 시 install.packages("ggplot2") 실행)
library(ggplot2)

# 성별에 따른 키의 분포 시각화
ggplot(class.data, aes(x = country, y = child_mort, fill = country)) +
  geom_boxplot(alpha = 0.7) +          # 박스 플롯 생성 (투명도 0.7)
  geom_jitter(width = 0.2, alpha = 0.5) + # 실제 데이터 포인트를 흩뿌려 분포 확인
  labs(title = "child_mort Distribution by country",
       x = "country",
       y = "child_mort") +
  theme_minimal() 

library(ade4)

pca_result <- dudi.pca(class.data, scannf = FALSE, nf = 2)
