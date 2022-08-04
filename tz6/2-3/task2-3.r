#Сгенерировать данные из нормального распределения с различными параметрами и провести анализ с помощью графиков квантилей, метода огибающих, а также стандартных процедур проверки гипотез о нормальности, рассмотренных на семинаре (6 тестов). Рассмотреть выборки малого (не более 50-100 элементов) и умеренного (1000-5000 наблюдений) объемов.
#Сгенерировать данные из комбинаций, реализованных в R распределений, а затем провести анализ с помощью графиков квантилей, метода огибающих, а также стандартных процедур проверки гипотез о нормальности. Рассмотреть выборки малого и умеренного объемов. Сравнить эффективность методов

library(ggplot2)
library(gridExtra)
library(nortest)

small_data = data.frame(
Factor = rep(c("1", "2"), each = 50),
Variable = c(rnorm(50, 5, 2),
rnorm(50, 4, 3)))
big_data = data.frame(
Factor = rep(c("1", "2"), each = 2500),
Variable = c(rnorm(2500, 5),
rnorm(2500, 4)))
set.seed(123)
lambda <- 0.2
n <- 5000
data_expBig <- data.frame(x = rexp(n, lambda))
q <- 100
data_expSmall <- data.frame(x = rexp(q, lambda))
graf_small <- ggplot(small_data, aes(Variable, group = Factor, fill = Factor)) + geom_density(alpha = 1/2)
graf_big <- ggplot(big_data, aes(Variable, group = Factor, fill = Factor)) + geom_density(alpha = 1/2)
ExponentialBig <- (ggplot(data_expBig, aes(x = x)) + ylab("") +
geom_histogram(aes(y=..density..), binwidth=0.5, color="black", fill="white") +
ggtitle("
exponential distribution") + geom_density(alpha = 0.2, fill = "#102C60"))
ExponentialSmall <- (ggplot(data_expSmall, aes(x = x)) + ylab("") +
geom_histogram(aes(y=..density..), binwidth=0.5, color="black", fill="white") + ggtitle("
exponential distribution") + geom_density(alpha = 0.2, fill = "#102C60"))
graf_small + facet_grid()
graf_big + facet_grid()
plot(ExponentialBig) 
plot(ExponentialSmall)
grid.arrange(graf_small, ExponentialSmall, graf_big, ExponentialBig, ncol = 2)
qqnorm(small_data$Variable)
qqline(small_data$Variable) 
qqnorm(big_data$Variable)
qqline(big_data$Variable) 
qqnorm(data_expBig$x)
qqline(data_expBig$x) 
qqnorm(data_expSmall$x)
qqline(data_expSmall$x) 
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))
SmallV <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(small_data$Variable), sd(small_data$Variable))
shapiro.test(small_data$Variable)
ad.test(small_data$Variable)
cvm.test(small_data$Variable)
lillie.test(small_data$Variable)
sf.test(small_data$Variable)
shapiro.test(big_data$Variable)
ad.test(big_data$Variable)
cvm.test(big_data$Variable)
lillie.test(big_data$Variable)
sf.test(big_data$Variable)
shapiro.test(data_expBig$x)
ad.test(data_expBig$x)
cvm.test(data_expBig$x)
lillie.test(data_expSmall$x)
sf.test(data_expSmall$x)
shapiro.test(data_expBig$x)
ad.test(data_expBig$x)
cvm.test(data_expBig$x)
lillie.test(data_expBig$x)
sf.test(data_expBig$x)
