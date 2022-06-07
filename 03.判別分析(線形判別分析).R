# 1 ライブラリ
install.packages("MASS")
library(dplyr)
library(ggplot2)
library(MASS)

# 2 データの作成
set.seed(1)
売り上げ <- c(rnorm(50, 2000, 200), rnorm(50, 1700, 200)) %>% round(1)
利用回数 <- c(rpois(50, 30), rpois(50, 20))
顧客 <- c(rep("顧客", 50), rep("退会", 50))
data <- data.frame(売り上げ, 利用回数, 顧客)
data %>% sample_n(5)

# 3 データの可視化
data %>%
  ggplot()+
  geom_point(aes(売り上げ, 利用回数, col=顧客))+
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text=element_text(size=30)) +
  labs(title="散布図")

# 4 分析結果
model <- lda(data=data, 顧客~売り上げ+利用回数)
model

# 5 結果の可視化
a0 <- - apply(model$means %*% model$scaling,2,mean)
x <- seq(1400, 2200,1)
y <- -model$scaling[1]/model$scaling[2]*x - a0/model$scaling[2]
mu <- apply(model$means, 2, mean)
ggplot()+
  geom_point(aes(data$売り上げ, data$利用回数, col=顧客))+
  geom_point(aes(mu[1], mu[2]), size=3) +
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text=element_text(size=30)) +
  labs(title="散布図", x="売り上げ", y="利用回数") +
  geom_line(aes(x,y))

