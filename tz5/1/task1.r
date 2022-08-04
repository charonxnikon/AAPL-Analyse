#С помощью функций пакета outliers с помощью формальных критериев для идентификации выбросов (Шовене, Граббса, Пирса, Q-тест Диксона и др.) проверить в данных, является ли максимальное наблюдение выбросом.

library(outliers)
library(dplyr)

mytest <- function(data)
{
	max <- max(data$Close)
	blow_out <- outlier(data$Close, opposite = FALSE ,logical = FALSE)
	if(max == blow_out)
		print("yes, it is: ")
		return (max(data$Close))
	return
}

qtest <- function(data)
{
	sort(data$Close)
	tmp <- abs(min-data$Close/dif)
	res = sd(data$Close)/sqrt(length(data$close))
	print("by q-test:")
	return(res)
}

grabs <- function(data)
{
	k = 0
	for ( i in data$Close )
		k = k + (i - average)^2
	
	tmp <- sqrt(k/(1550 - 1))
	res <- abs(max - average)/tmp
	print("by grabs:")
	return (res)
}

showen <- function(data)
{ 
	tmp <- abs(max - average)/dispersion
	res = (1 - tmp)*2
	print("by showen:")
	if (res<0.5)
		print("data Error")
		return 
	return (res)
}

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
data <- mutate_each(data, "factor", Date, Volume)
glimpse(data)
hist(data$Close, breaks = 20, freq = FALSE, col = "black",
	xlab = "Price", ylab = "Density", main = "Price")
lines(density(data$Close), col = "green", lwd = 2)

min <- min(data$Close)
max <- max(data$Close)
dif <- max-min
average <- mean(data$Close)
dispersion <- var(data$Close)

print(mytest(data))
print(showen(data))
print(grabs(data))
print(qtest(data))


