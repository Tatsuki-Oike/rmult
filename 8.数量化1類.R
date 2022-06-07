# 1 ライブラリ
library(dplyr)
library(ggplot2)

# 2 データの作成
set.seed(100)
N <- 30
講義 <- c(rep("講義A", N), rep("講義B", N), rep("講義C", N))
e1 <- rnorm(N, 0, 10)
e2 <- rnorm(N, 0, 10)
e3 <- rnorm(N, 0, 10)
data <- data.frame(講義) %>% 
  mutate(テスト=ifelse(講義=="講義A", 50+e1 , ifelse(講義=="講義B", 30+e2, 80+e3))) %>%
  mutate(テスト=round(テスト))
data %>% sample_n(5)

# 3 データの可視化
data %>%
  ggplot() +
  geom_point(aes(講義, テスト)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=30)) +
  labs(title="データプロット", x=NULL)

# 4 データの変換
data %>%
  mutate(講義B = ifelse(講義=="講義B", 1, 0)) %>%
  mutate(講義C = ifelse(講義=="講義C", 1, 0)) %>% 
  sample_n(5)

# 5 分析結果
model1 <- lm(data=data, テスト~講義)
summary(model1)
