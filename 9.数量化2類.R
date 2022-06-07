# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(MASS)

# 2 データの作成
set.seed(10)
N <- 30
講義 <- c(rep("講義A", N), rep("講義B", N), rep("講義C", N))
e1 <- rnorm(N, 0, 10) ; e2 <- rnorm(N, 0, 10) ; e3 <- rnorm(N, 0, 10)
合格 <- c("不合格", "合格")
合格 <- c(合格[rbinom(N,1,0.5)+1], 合格[rbinom(N,1,0.2)+1], 合格[rbinom(N,1,0.8)+1])
data <- data.frame(講義, 合格)
data %>% sample_n(5)

# 3 データの可視化
table(data)

# 4 データの変換
data %>% 
  mutate(講義B = ifelse(講義=="講義B", 1, 0)) %>%
  mutate(講義C = ifelse(講義=="講義C", 1, 0)) %>%
  sample_n(5)

# 5 分析結果
lda(data=data, 合格~講義)

