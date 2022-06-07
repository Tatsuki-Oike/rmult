# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)

# 2 データの作成
set.seed(1)
線形代数 <- rnorm(100, 50, 10) %>% round()
微積分 <- rnorm(100, 50, 10) %>% round()
機械学習 <- rnorm(100, 0.7*線形代数+0.3*微積分, 5) %>% round()
NN <- rnorm(100, 0.2*線形代数+0.8*微積分, 5) %>% round()
data <- data.frame(線形代数, 微積分, 機械学習, NN)
data <- apply(data, 2, scale)
data %>% head()

# 3 分析結果
data.model <- '
機械学習 ~ 線形代数+微積分
NN ~ 線形代数+微積分'
model <- sem(data.model, data=data)
summary(model)
par(family= "HiraKakuProN-W3")
semPaths(model, "model", "est", rotation = 2,
         mar=c(5,8,5,14), # 余白の指定、下、左、上、右の順
         edge.label.cex=2, # 矢印の係数の文字の大きさ
         sizeMan = 15,
         sizeLat = 15,
         style = "lisrel",
         curve = 3,
         nCharNodes = 4
)
