#3. Изучить возможности функции power.prop.test() и разобраться с вычислением статистической мощности при сравнении частот.


library(reshape2)
library(pwr)
library(dplyr)

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data <- mutate_each(data, "factor", Volume, Date)

power.prop.test(n = 20000, p1 = 0.6, p2 = 0.3, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE, tol = .Machine$double.eps^0.25)

pwr.p.test(h = ES.h(p1 = 0.65, p2 = 0.50),
           sig.level = 0.05,
           power = 0.80)

v <- matrix(c(25, 70, 7, 80), ncol = 2, byrow = T)
v
chisq.test(v)

prop.power <- function(n1, n2, p1, p2) {
  twobytwo=matrix(NA, nrow = 10000, ncol = 4)
  twobytwo[,1] = rbinom(n = 10000, size = n1, prob = p1)
  twobytwo[,2] = n1-twobytwo[,1]
  twobytwo[,3] = rbinom(n = 10000, size = n2, prob = p2)
  twobytwo[,4] = n1 - twobytwo[, 3]
  a = rep(NA, 10000)
  chisq.test.v = function(x) 
  as.numeric(chisq.test(matrix(x, ncol = 2), correct = FALSE)[3])
  a = apply(twobytwo, 1, chisq.test.v)
  power = sum(ifelse(a < 0.05, 1, 0))/10000
  return(power)
}

prop.power(50, 50, 0.50, 0.30)
