#Нарисовать график и гистограмму для полученных данных

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
		header = TRUE)
#print(head)
num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
head <- cbind(head, num)
data <- head[-c(2,3,4,6)]
data
names(data)
#
#
#
data$Date <- as.POSIXct(data$Date)
format(data$Date,"%Y")
plot(data$Date, data$Close,
	main = "APPLE INC TIME-CLOSE", xlab = "date", ylab = "close",
	col.main = "blue",type = "l", lwd = 0.5, col = "black", 
	panel.first = lines(stats::lowess(data$Date,data$Close),lty = "dashed"),
	axes = TRUE, frame.plot = TRUE)
plot(data$Date, data$Volume,
	main = "APPLE INC TIME-VOLUME", xlab = "date", ylab = "volume",
	col.main = "blue", type = "l", lwd = 0.5, col = "black", 
	axes = FALSE, frame.plot = TRUE)
e.y = c(1:6); my.at <- e.y*10^8
axis(1,at = data$Date, format(data$Date, "%Y"))
axis(2,at = my.at,col.axis = "black",las = 1,
     labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))
plot(data$Close,data$Volume,
	main = "APPLE INC CLOSE-VOLUME", xlab = "close", ylab = "volume",
	col.main = "blue",type = "p", lwd = 0.5, col = "brown",
	axes = FALSE, frame.plot = FALSE)
axis(1,at = seq(20,140,20))
axis(2,at = my.at, col.axis = "black", las = 1,
     labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))
#
#
#
hist(data$Volume, main = "APPLE INC HIST VOLUME", col = "black",
	 axes = FALSE, xlab = "volume")
axis(2,at = seq(0,500,100))
axis(1,at = my.at, col.axis = "black", las=1,
	labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))
hist(data$Close, main = "APPLE INC HIST CLOSE", col = "black", xlab = "close")
