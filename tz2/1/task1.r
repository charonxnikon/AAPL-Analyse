#Используя команды read.table() и read.csv(), а также file.choose(), загрузить данные из TXT и CSV-файлов и создать из некоторого их подмножества список и таблицу

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
#print(head)
num <- read.table("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/table.txt")
head <- cbind(head,num)
data <- head[-c(2,3,4,6)]
data

