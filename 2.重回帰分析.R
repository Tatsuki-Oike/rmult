# 1 ライブラリ
library(dplyr)
library(ggplot2)

# 2 データの作成
set.seed(10)
年齢 <- rnorm(100, 40, 20) 
肩幅 <- rnorm(100, 40, 3)
身長 <- rnorm(100, 170, 10)
e <- rnorm(100, 0, 50) 
年収 <- 300 + 5*年齢 + rnorm(100,10*肩幅,70) + e
data <-data.frame(年齢, 肩幅, 身長, 年収) %>%
  filter(18<年齢&年齢<60) %>%
  apply(2, round) %>%
  as.data.frame()
data %>% head()

# 3 分析結果
model <- lm(data=data, 年収~年齢+肩幅+身長)
summary(model)

# 4 多重共線性
set.seed(1)
年齢 <- rnorm(100, 40, 20) 
肩幅 <- rnorm(100, 年齢, 1)
身長 <- rnorm(100, 170, 10)
e <- rnorm(100, 0, 50) 
年収 <- 300 + 5*年齢 + rnorm(100,10*肩幅,70) + e
data <-data.frame(年齢, 肩幅, 身長, 年収) %>%
  filter(18<年齢&年齢<60) %>%
  apply(2, round) %>%
  as.data.frame()
cor(data[,c(4,1:3)])

model <- lm(data=data, 年収~年齢+肩幅+身長)
model$coefficients

