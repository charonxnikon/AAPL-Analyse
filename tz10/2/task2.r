#3. Изучить форму связи между переменными для выбранных данных.
#4. Изучить взаимодействие между предикторами для выбранных данных.

library(faraway)
library("dplyr")


head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date, format = "%Y")
data <- mutate_each(data, "factor", Date, Close)
mk <- lm(data$Volume ~ data$Date + data$Close)
interaction.plot(data$Close, data$Date, data$Volume)
interaction.plot(data$Date, data$num, data$Volume)


