#Написать функцию, которая выполняет загрузку данных и их рисование (аналогично заданию с прошлого семинара)

create_table <- function(data2)
{
	num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
	data2 <- cbind(data2, num)
	data_all <- data2[-c(2,3,4,6)]
	return (data_all)
}

print_names_and_data <- function(data)
{
	print(data)
	print(names(data))
}


plot_data_time_close <- function(data)
{
	plot(data$Date, data$Close,
        	main = "APPLE INC TIME-CLOSE", xlab = "date", ylab = "close",
        	col.main = "blue",type = "l", lwd = 0.5, col = "black",
        	panel.first = lines(stats::lowess(data$Date, data$Close),lty = "dashed"),
        	axes = TRUE, frame.plot = TRUE)
}

plot_data_time_volume <- function(data)
{
	plot(data$Date, data$Volume,
        	main = "APPLE INC TIME-VOLUME", xlab = "date", ylab = "volume",
        	col = "black", type = "l", lwd = 0.5,
        	axes = FALSE, frame.plot = TRUE)
	axis(1, at = data$Date, format(data$Date, "%Y"))
	axis(2, at = my.at, col.axis = "black", las = 1,
     	labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))	
}

plot_data_close_volume <- function(data)
{
	plot(data$Close, data$Volume,
        	main = "APPLE INC CLOSE-VOLUME", xlab = "close", ylab = "volume",
        	col.main = "blue", type = "p", lwd = 0.5, col = "brown",
        	axes = FALSE, frame.plot = FALSE)
	axis(1, at = seq(20,140,20))
	axis(2, at = my.at, col.axis = "black", las = 1,
     	labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))
}

hists_data <- function(data)
{
	hist(data$Volume, main = "APPLE INC HIST VOLUME", 
		col = "black", axes = FALSE, xlab = "volume")
	axis(2, at = seq(0,500,100))
	axis(1, at = my.at, col.axis = "black", las = 1,
        labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^8))))
	hist(data$Close, main = "APPLE INC HIST CLOSE", col = "black", xlab = "close")
}

#
#
#
#main

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
		header = TRUE)
data = create_table(head)
data$Date <- as.POSIXct(data$Date)
e.y = c(1:6); my.at <- e.y*10^8
print_names_and_data(data)
plot_data_time_close(data)
plot_data_time_volume(data)
plot_data_close_volume(data)
hists_data(data)


