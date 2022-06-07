# 1 ライブラリ
library(dplyr)
library(ggplot2)

# 2 2変量

## 2.1 データの作成
set.seed(10)
英語 <- rnorm(100, 50, 10) %>% round()
数学 <- rnorm(100, 英語, 10) %>% round()
data <- data.frame(英語, 数学)
data %>% head()

## 2.2 データの可視化
data %>%
  ggplot() +
  geom_point(aes(英語, 数学))+
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text=element_text(size=30)) +
  labs(title="データプロット")

## 2.3 分析結果
model <- prcomp(data, scale = TRUE)
summary(model)

## 2.4 主成分得点
z <- model$rotation[,1] %*% t(scale(data)) %>%
  round(2) %>% t() %>% as.data.frame()
colnames(z) <- "z"
z %>% head()

# 3 多変量

## 3.1 データの作成
set.seed(10)
英語 <- rnorm(100, 50, 10) %>% round()
数学 <- rnorm(100, 50, 10) %>% round()
国語 <- rnorm(100, 英語, 5) %>% round()
理科 <- rnorm(100, 数学, 5) %>% round()
社会 <- rnorm(100, 国語, 3) %>% round()
data <- data.frame(英語, 数学, 国語, 理科, 社会)
data %>% head()

# 3.2 分析結果
model <- prcomp(data, scale = TRUE)
summary(model)
biplot(model, family="HiraKakuPro-W3")

# 3.3 主成分得点
z <- t(model$rotation[,1:2]) %*% t(scale(data)) %>% 
  round(2) %>% t() %>% as.data.frame()
colnames(z) <- c("z1", "z2")
z %>% head()

