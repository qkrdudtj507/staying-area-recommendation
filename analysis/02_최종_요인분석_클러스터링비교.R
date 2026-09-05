# 필요한 패키지 설치 및 로드
install.packages("psych")
install.packages("GPArotation")
install.packages("scales")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrplot")
install.packages("cluster")
library(ggplot2)
library(reshape2)
library(corrplot)
library(scales)
library(psych)
library(GPArotation)
library(cluster)
library(dplyr)

# 데이터 로드
data <- read.csv("data_pca_minmax.csv")
data <- data[,-c(1,2)]

# KMO 검정
print(KMO(data))

# 각 변수의 분산 계산 및 시각화
variances <- apply(data, 2, var)
variance_df <- data.frame(
  variable = names(variances),
  variance = variances
)
ggplot(variance_df, aes(x = variable, y = variance)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Variance of Each Variable",
       x = "Variable",
       y = "Variance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 변수 간의 상관 행렬 계산 및 히트맵 시각화
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
          tl.col = "black", tl.srt = 45, 
          title = "Correlation Matrix Heatmap")

# 요인 분석 수행
num_factors <- 4
fa_result <- fa(data, nfactors = num_factors, rotate = "varimax")

print(fa_result)
factor_scores <- as.data.frame(fa_result$scores)
print(fa_result)
print(fa_result$loadings)

# 요인 점수 계산
factor_scores <- as.data.frame(factor.scores(data, fa_result)$scores)
print(factor_scores)

# 요인 분석 결과 시각화
fa.diagram(fa_result, cut = 0.3, simple = FALSE)

# 최적의 클러스터 개수 결정 (엘보우 방법)
sse <- numeric()
k_range <- 1:10
for (k in k_range) {
  kmeans_result <- kmeans(factor_scores, centers = k, nstart = 25)
  sse[k] <- kmeans_result$tot.withinss
}
plot(k_range, sse, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", 
     ylab = "Total within-clusters sum of squares")


# K-means 클러스터링 수행
k <- 3
set.seed(123)
cluster_results <- kmeans(factor_scores, centers = k, nstart = 20)
print(cluster_results$cluster)
factor_scores_df <- data.frame(fa_result$scores)

# 클러스터링 결과를 원래 변수 데이터프레임에 추가
final_clusters <- cbind(factor_scores, Cluster = cluster_results$cluster)
factor_scores_df <- cbind(factor_scores_df, Cluster = cluster_results$cluster)
Name <- c("강남구", "강동구", "강북구", "강서구", "관악구", 
          "광진구", "구로구", "금천구", "노원구", "도봉구", 
          "동대문구", "동작구", "마포구", "서대문구", "서초구", 
          "성동구", "성북구", "송파구", "양천구", "영등포구", 
          "용산구", "은평구", "종로구", "중구", "중랑구")
ggplot(factor_scores_df, aes(x = MR3, y = MR1, color = factor(Cluster), label = Name)) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +  # 텍스트 레이블 추가
  labs(title = "Factor Biplot with Clustering",
       x = "MR3", y = "MR1", color = "Cluster") +
  theme_minimal()

# 계층적 클러스터링 수행
dist_matrix <- dist(factor_scores, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "", ylab = "Height")

clusters <- cutree(hc, k = 3)
factor_scores_df <- data.frame(factor_scores)
factor_scores_df <- cbind(factor_scores_df, Cluster = clusters)

ggplot(factor_scores_df, aes(x = MR3, y = MR1, color = factor(Cluster), label = Name)) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +  # 텍스트 레이블 추가
  labs(title = "Factor Biplot with Hierarchical Clustering",
       x = "MR3", y = "MR1", color = "Cluster") +
  theme_minimal()

# PAM 클러스터링 수행
pam_result <- pam(factor_scores, k = 3)
print(pam_result$clustering)
factor_scores_df <- data.frame(factor_scores)
factor_scores_df <- cbind(factor_scores_df, Cluster = pam_result$clustering)
ggplot(factor_scores_df, aes(x = MR3, y = MR2, color = factor(Cluster), label = Name)) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +  # 텍스트 레이블 추가
  labs(title = "Factor Biplot with PAM Clustering",
       x = "MR3", y = "MR2", color = "Cluster") +
  theme_minimal()


# 3D 시각화
install.packages("plotly")
library(plotly)

factor_scores_df$Name <- Name

# 3D 시각화
plot_ly(factor_scores_df, x = ~MR1, y = ~MR2, z = ~MR3, color = ~factor(Cluster), text = ~Name, 
        type = 'scatter3d', mode = 'markers+text', marker = list(size = 3), textposition = 'top right') %>%
  layout(scene = list(
    xaxis = list(title = 'MR1'),
    yaxis = list(title = 'MR2'),
    zaxis = list(title = 'MR3')),
    title = "3D Scatter Plot of Clusters"
  )

# 실루엣 점수 계산 및 최적의 클러스터링 방법 선택
silhouette_scores <- list()
methods <- c( "kmeans","pam", "hclust")

for (method in methods) {
  if (method == "kmeans") {
    km <- kmeans(factor_scores, centers = 3, nstart = 25)
    sil <- silhouette(km$cluster, dist(factor_scores))
  } else if (method == "pam") {
    pam_result <- pam(factor_scores, k = 3)
    sil <- silhouette(pam_result$clustering, dist(factor_scores))
  } else if (method == "hclust") {
    hclust_result <- cutree(hclust(dist(factor_scores)), k = 3)
    sil <- silhouette(hclust_result, dist(factor_scores))
  }
  silhouette_scores[[method]] <- mean(sil[, 3])
}

pam_result
# 최적의 클러스터링 방법 선택
best_method <- names(which.max(unlist(silhouette_scores)))
print(paste("Best clustering method:", best_method))
