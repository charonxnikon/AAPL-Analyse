#1. В примере с гистограммой и плотностью воспользоваться функцией legend для отображения подписей к графикам ядерных оценок плотностей.
#Реализовать аппроксимацию с помощью ядерных оценок для своих данных.

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
data$Date <- as.POSIXct(data$Date)
data <- mutate_each(data, "factor", Date)
glimpse(data)
hist(data$Close, col = "black",
	breaks = 20, freq = FALSE,
	xlab = "close", main = "Hist with kernel density")
lines(density(data$Close), col = "green", lwd = 2)
lines(density(data$Close, bw = 1.5), col = "red", lwd = 2)

legend("bottomleft", 
	legend = c("all data", "with step = 1.5"), 
	col = c("green", "red"),
	pch = c(19,17),
	bty = "n", 
	pt.cex = 1, 
	cex = 1.5, 
	text.col = "black", 
	horiz = F, 
	inset = c(0.5, 0.5))



