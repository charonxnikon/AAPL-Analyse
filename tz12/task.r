#2. Для собственных данных построить доверительные интервалы для модели регрессии и интервал предсказаний.
#3. Модифицировать функцию regr() для расчета возраста Вселенной с помощью бутстрепа.
#4. Изучить коэффициент детерминации (R-squared и adjusted R-squared) как критерий качества аппроксимации моделью.

library(ggplot2)
library(dplyr)
library(car)
library(arm)
library(corrplot)
library(gamair)
library(boot)
data(hubble)
library(gridExtra)

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d")

state <- as.data.frame(data[,c("Close", "Volume", "num", "num2")])
M <- cor(state)
col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#000000"))
corrplot(M, method="pi", col=col4(10), cl.length = 11,
         order = "AOE", addCoef.col = "red")

ggplot(data, aes(x = data$Close, data$Volume)) + geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    ylab("Volume") + xlab("Close")
model <- lm(data$Volume ~ data$Close, data)
summary(model)
confint(model)
confint(model, level = 0.99)
coefplot(model, parm = -2)

ggplot(data, aes(x = data$Date, data$Volume)) + geom_point() + 
	geom_smooth(method = "lm", se = FALSE) + 
	ylab("Volume") + xlab("Date")
model <- lm(data$Volume ~ data$Date, data)
summary(model)
confint(model)
confint(model, level = 0.99)
coefplot(model, parm = -2)
ggplot(data, aes(x = data$Date, data$Close)) + geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    ylab("Close") + xlab("Date")
model <- lm(data$Close ~ data$Date, data)
summary(model)
confint(model)
confint(model, level = 0.99)
coefplot(model, parm = -2)

BD <- read.table("https://raw.githubusercontent.com/rasbt/python-machine-learning-book-2nd-edition/master/code/ch10/housing.data.txt")
state <- as.data.frame(BD)
M <- cor(state)
corrplot(M, method="pi", col=col4(10), cl.length = 11,
         order = "AOE", addCoef.col = "red")

ggplot(BD, aes(x = BD$V13, BD$V14)) + geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    ylab("MEDV") + xlab("LSTAT")
model <- lm(BD$V14 ~ BD$V13, data)
summary(model)
confint(model)
confint(model, level = 0.99)
coefplot(model, parm = -2)

hubble$x <- hubble$x*3.09e19
hubble$y <- hubble$y*60^2*24*365.25

regr <- function(data, indices)
{
	BD <- data[indices, ]
	fit <- lm(x ~ -1 + y, data = BD)
	return(summary(fit)$coefficients[1])
}

results <- boot(data = hubble, statistic = regr, R = 1000)
results
h.lower <- quantile(results$t, 0.025)
h.upper <- quantile(results$t, 0.975)
c(h.lower, h.upper)
boot.ci(results, type = "bca")

M <- lm(y ~ x - 1, data = hubble)
summary(M)
hubble$fit = fitted(M)

p1 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_hline(aes(yintercept=mean(hubble$y)), color = "red") +
  geom_segment(aes(x = x, y = y, xend = x, yend = mean(hubble$y))) +
  ggtitle("TSS")

p2 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_segment(aes(x = x, y = y, xend = x, yend = fit)) +
  ggtitle("RSS")
grid.arrange(p1, p2, ncol = 2)



