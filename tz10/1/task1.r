#2. Проверить наличие мультиколлинеарности в собственных данных. Изучить фактор инфляции дисперсии VIF.

library("dplyr")
library("ggplot2")
library("car")

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data <- mutate_each(data, "factor", Date)

ggplot(data = data, aes(x = Date, y = Close)) + geom_point()
poly <- lm(Volume ~ Close + I(Close^2) + I(Close^3), data)
summary(poly)
vif(poly)
