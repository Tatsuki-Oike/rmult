# 1 ライブラリ
install.packages("levaan")
install.packages("semPlot")
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)

# 2 データの作成
set.seed(1)
理系 <- rnorm(100, 50, 10)
文系 <- rnorm(100, 50, 10)
英語 <- rnorm(100, 文系, 10) %>% round()
数学 <- rnorm(100, 理系, 10) %>% round()
国語 <- rnorm(100, 文系, 10) %>% round()
理科 <- rnorm(100, 理系, 10) %>% round()
社会 <- rnorm(100, 文系, 10) %>% round()
data <- data.frame(英語, 数学, 国語, 理科, 社会)
data <- apply(data, 2, scale)
data %>% head()

# 3 分析結果(cfa)
data.model <- '理系=~数学+理科
                文系=~英語+国語+社会'
model <- cfa(data.model, data=data)
summary(model,fit.measures=TRUE)

# 4 分析結果(sem)
model2 <- sem(data.model, data=data)
summary(model2,fit.measures=TRUE)
par(family= "HiraKakuProN-W3")
semPaths(model2, "model", "est", rotation = 2,
         mar=c(3,5,3,7), # 余白の指定、下、左、上、右の順
         edge.label.cex=1.5, # 矢印の係数の文字の大きさ
         sizeMan = 15,
         sizeLat = 15,
         style = "lisrel",
         curve = 3
         )

