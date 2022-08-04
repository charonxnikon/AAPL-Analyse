#Ознакомиться с процедурой бутстреп (bootstrap) процедурой

library(boot)
library(sm)
library(dplyr)

#
#
#
#main
head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
head <- cbind(head, num)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date)
data <- mutate_each(data, "factor", Date, Close)
glimpse(data)
rsq <- function(formula, data, indices)
{
	d <- data[indices,]
	fit <- lm(formula, data = d)
	return(summary(fit)$r.square)
}
bootobject <- boot(data = data, statistic=rsq,
R = 1000, formula = Volume~Date+Close)
bootobject
plot(bootobject)
boot.ci(bootobject, type="bca")
