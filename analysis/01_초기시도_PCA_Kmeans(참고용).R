# ⚠️ 참고: 이 스크립트는 초기에 시도했던 PCA + K-means 기반 접근입니다.
# 클러스터별 자치구 개수가 1개/9개/15개로 심하게 불균형해 K-means를 기각했고,
# 최종적으로는 요인분석(Factor Analysis) + 클러스터링 비교 방식으로 전환했습니다.
# 최종 코드는 analysis/02_최종_요인분석_클러스터링비교.R 을 참고하세요.

house <- read.csv("data_house_combine.csv")
public <- read.csv("public.csv")
traffic <- read.csv("traffic.csv")

public <- public[ , -1]
house <- house[ , -1]

#통합 데이터
merged_df <- cbind(public, traffic, house)

# 필요한 패키지 설치 (한 번만 실행하면 됩니다)
install.packages("caret")
install.packages("factoextra")
install.packages("dplyr")
library(dplyr)
library(caret)
library(factoextra)

merged_df <- merged_df %>%
  mutate(across(where(is.factor), as.character))

# 스탠다드 스케일링 수행
df_scaled <- scale(merged_df)

# 스탠다드 스케일링 후 데이터 요약
summary(df_scaled)

# PCA 수행
pca_result <- prcomp(df_scaled)

# PCA 결과 요약
summary(pca_result)

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

# 주성분 별 누적 분산 비율
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
print(cumulative_variance)

pc_loading <- pca_result$rotation

pc1_formula <- paste("PC1 =", paste(pc_loading, colnames(df), sep = " * ", collapse = " + "))

# 결과 출력
print(pc1_formula)

# PC1과 PC2의 적재량 출력
print("PC1의 적재량:")
print(pc_loading[, 1])  # PC1의 적재량
print("\nPC2의 적재량:")
print(pc_loading[, 2])  # PC2의 적재량

# 적절한 주성분 수 결정
num_components <- which(cumulative_variance >= 0.80)[1]
print(num_components)


library(ggplot2)  # 시각화를 위한 패키지

pca_df <- data.frame(
  PC1 = pca_result$x[, 1],  # 첫 번째 주성분
  PC2 = pca_result$x[, 2],
  PC3 = pca_result$x[, 3] 
)

pca_df$Label <- sample(c("Group1", "Group2", "Group3"), nrow(pca_df), replace = TRUE)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Label)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Group1" = "blue", "Group2" = "green", "Group3" = "red")) +
  labs(x = "PC1", y = "PC2", title = "PCA Visualization") +
  theme_minimal()

# Biplot 시각화 (보강된 설정)
biplot <- fviz_pca_biplot(pca_result, 
                          geom = "point",  # 점의 형태 설정
                          label = "var",  # 변수 이름 표시
                          repel = TRUE,   # 라벨 겹침 방지
                          arrow.size = 0.75,  # 화살표 크기 조정
                          col.var = "black",  # 변수 점 색상
                          col.ind = "blue",   # 개체(데이터 점) 색상
                          pointshape = 21,    # 점의 모양 (마름모 모양)
                          habillage = "none"  # 개체(데이터 점) 그룹화 설정
)

# 그래프 출력
print(biplot)


# 주성분 점수 추출
pca_scores <- as.data.frame(pca_result$x[, 1:3])  # PC1, PC2, PC3만 추출

# 클러스터링 수행 (예시로 K-means 클러스터링 사용)
set.seed(123)
k <- 3  # 클러스터 개수 지정
cluster_results <- kmeans(pca_scores, centers = k)

# 클러스터링 결과 확인
print(cluster_results$cluster)

# 클러스터링 결과를 원래 변수 데이터프레임에 추가
df_clustered <- cbind(df, Cluster = cluster_results$cluster)

# 결과 출력
print(df_clustered)
ggplot(df_clustered, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "PCA Biplot with Clustering",
       x = "PC1", y = "PC2", color = "Cluster") +
  theme_minimal()
