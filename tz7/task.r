#1. Изучить теоретические выражения для рассмотренных на занятиях критериев (Стьюдента и ранговые), а также их свойства.
#2. Рассмотреть односторонние варианты t-теста, когда проверяемая нулевая гипотеза заключается в том, что одно из сравниваемых средних значений больше (или меньше) другого: использовать t.test() в сочетании с аргументом alternative (может принимать одно из трех значений – «two.sided» (по умолчанию), «greater» или «less».
#3. Найти выражения для критериев Фишера, Левене, Бартлетта, Флигнера-Килина и изучить проверку гипотез об однородности дисперсий в R ( на примерах).


library(dplyr)
library(HSAUR2)
library(nortest) 
library(car)

#
#
#
#main
head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date)
data
data <- mutate_each(data, "factor", num)
glimpse(data)
mean(data$Close)
tapply(data$Close, data$Volume, mean)
t.test(data$Close, mu = 44)
t.test(data$Close ~ num)
t.test(data$Close ~ num, var.equal = TRUE)
t.test(data$Close ~ num, alternative = "greater")
t.test(data$Close ~ num, alternative = "less")
t.test(data$Close ~ num, alternative = "two.sided")
var.test(data$Close ~ num, alternative = c("two.sided"), conf.level = 0.95) 
leveneTest(data$Close ~ num, data, location="median")
bartlett.test(data$Close ~ num)
fligner.test(data$Close ~ num, data = InsectSprays)
aov(data$Close ~ num, data) 
lm(data$Close ~ num)
