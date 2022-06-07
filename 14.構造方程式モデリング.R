# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)

# 2 データの作成
set.seed(1)
数学力 <- rnorm(100, 50, 10)
プログラミング力 <- rnorm(100, 数学力, 5)
微積分 <- rnorm(100, 数学力, 5) %>% round()
統計学 <- rnorm(100, 数学力, 5) %>% round()
線形代数 <- rnorm(100, 数学力, 5) %>% round()
R言語 <- rnorm(100, プログラミング力, 5) %>% round()
Python <- rnorm(100, プログラミング力, 5) %>% round()
data <- data.frame(微積分, 統計学, 線形代数, R言語, Python)
data <- apply(data, 2, scale)
data %>% head()

# 3 分析結果
data.model <- '
数学=~微積分+統計学+線形代数
プログラミング力=~R言語+Python
数学=~プログラミング力
'
model <- sem(data.model, data)
summary(model)
par(family= "HiraKakuProN-W3")
semPaths(model, "model", "est", rotation = 2,
         mar=c(5,6,5,6), # 余白の指定、下、左、上、右の順
         edge.label.cex=1.0, # 矢印の係数の文字の大きさ
         sizeMan = 12,
         sizeLat = 12,
         style = "lisrel",
         curve = 3,
         nCharNodes = 7
)
