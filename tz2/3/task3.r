#С помощью условного оператора if-else и циклов for, while, repeat проверить условия неотрицательности данных, а также попадания в некоторый диапазон для всех элементов списка и таблицы.

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
		header = TRUE)
data <- head[-c(2,3,4,6)]
data
for (elem in data){
	if (elem > 0 && is.numeric(elem) && elem<200){
			print(elem)
	}
}
