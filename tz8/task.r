#1. Изучить основы дисперсионного анализа, выражения для меж- и внутригрупповых дисперсий.
#2. На собственных данных проверить использование методов одно- и двухфакторного дисперсионного анализа.
#3. Изучить функции cor() и cor.test() для оценивания корреляций: Пирсона (по умолчанию), Спирмена (в cor.test() параметр method=”spearmen” – для переменных, чье распределение отличается от нормального, либо есть нелинейная связь) и ранговой корреляции Кендалла (в cor.test() параметр method=”kendall” – используется для проверки согласованности результатов измерений, полученных разными приборами, результатов голосования экспертов по одному и тому же вопросу и т.д.).

library(HSAUR2)
library(dplyr)
library(doBy)

#
#
#
#main
head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date)
data
#data <- mutate_each(data, "factor", num, num2)
glimpse(data)
mean(data$Close)
#tapply(data$Close, data$Volume, mean)
aov(data$Close ~ data$num, data) 
aov(data$Close ~ data$num + data$num2, data) 
aov(data$Close ~ data$num + data$num2 + data$num:data$num2, data) 
stripchart(data$Close ~ data$num, data, pch = 17, col = c("blue", "green", "black"))
summary(data$Close ~ data$num + data$num2 + data$num:data$num2, data)
stripchart(data$Close ~ data$num + data$num2 + data$num:data$num2, data, pch = 17, col = c("blue", "green", "black"))
cor(data$Close, data$num)
cor.test(data$Close, data$num)
cor.test(data$Close, data$num, method = "spearman")
cor.test(data$Close, data$num, method = "pearson")
cor.test(data$Close, data$num, method = "kendall")
