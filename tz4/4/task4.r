#Используя функцию pie, нарисовать пример круговой диаграммы для собственных данных

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
head <- cbind(head, num)
data <- head[-c(2,3,4,6)]
names(data)
tmp <- data
j = 1
i = 1480
tmp <- data[-c(j:i),]
res <- tapply(tmp$Volume, as.factor(tmp$Close), length)
pie(res, labels = names(res), edges = 300, radius = 1, 
	col = c("blue","green","red","yellow"))

#tmp_vector <- rep(0, 1500)
#tmpp <- levels(tmp$Volume)
#for (i in 1:length(tmp$Volume))
#	for (j in length(levels(tmp$Volume)))
#		if (as.character(tmp$Volume)[i] == tmpp[j])
#			tmp_vector[j] <- tmp_vector[j]+1
#names(tmp_vector) <- tmpp
#pie(tmp_vector)
	

