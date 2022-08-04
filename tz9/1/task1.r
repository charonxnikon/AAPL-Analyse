#2. На собственных данных проверить использование методов хи-квадрат, точного теста Фишера, теста МакНемара, Кохрана-Мантеля-Хензеля.

library(MASS)
library(dplyr)

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data <- mutate_each(data, "factor", Volume, Date)
#data$Date <- as.POSIXct(data$Date, format = "%Y")
chisq.test(data$Close)
fisher.test(data$num, data$num2)
mcnemar.test(data$num, data$num2, correct = TRUE)
mantelhaen.test(data$Close, data$num, data$num2)
