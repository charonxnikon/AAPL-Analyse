#Реализовать векторную обработку данных с помощью функций apply / lapply / sapply / vapply / mapply / rapply / tapply. 


create_table <- function( data2)
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

#
#
#
#main

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
		header = TRUE)
data = create_table(head)
data$Date <- as.POSIXct(data$Date)
names(data)
data <- data[,-c(1,4)]
print_names_and_data(data)
apply(data, 2, mean)
apply(data, 2, function(x) length(x[x<0]))
apply(data, 2, function(x) is.matrix(x))
apply(data, 2, is.vector) 
sapply(data[c(1,500,1000),2], function(x) x^2)
lapply(data[c(1,500,1000),2], function(x) x^2)
