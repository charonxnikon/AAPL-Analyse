#На примере собственных данных продемонстрировать применение cdplot и boxplot.

library(dplyr)
library(sm)

#
#
#
#main
head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
		header = TRUE)
num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
head <- cbind(head, num)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date, format = "%Y")
data <- mutate_each(data, "factor", Date)
data <- mutate_each(data, "factor", Volume)
glimpse(data)
summary(data)
cdplot(data$Close, data$Date, col = c("green", "blue","yellow",
	"firebrick2", "red", "brown", "purple","lawngreen"),
	ylab = "Date", xlab = "Price", main = "Price by Date")
cdplot(data$Volume, data$Date, col = c("green", "blue","yellow",
        "firebrick2", "red", "brown", "purple","lawngreen"),
        ylab = "Date", xlab = "Volume", main = "Volume by Date")
boxplot(data$Close ~ data$Date, data=mtcars, subset=cyl %in% c(4,6))


