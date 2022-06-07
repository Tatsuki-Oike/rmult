# 1 ライブラリ
library(dplyr)
library(ggplot2)

# 2 データの作成
set.seed(10)
年齢 <- rnorm(100, 40, 20) 
e <- rnorm(100, 0, 50) 
年収 <- 300 + 5*年齢 + e
data <-data.frame(年齢, 年収) %>%
  filter(18<年齢&年齢<60) %>%
  mutate(年齢=round(年齢), 年収=round(年収))
data %>% head()

# 3 データの可視化
data %>% 
  ggplot()+
  geom_point(aes(年齢, 年収))+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=30)) +
  labs(x="年齢", y="年収", title="単回帰分析")

# 4 分析結果
model <- lm(data=data, 年収~年齢)
summary(model)

# 5 結果の可視化
beta <- model$coefficients
x <- seq(20,60,1)
ggplot()+
  geom_point(aes(data$年齢, data$年収))+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=30)) +
  geom_line(aes(x, beta[1]+beta[2]*x), col="blue") +
  labs(x="年齢", y="年収", title="単回帰分析")

