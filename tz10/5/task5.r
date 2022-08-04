#Построить график автокорреляцоионной функции

library("dplyr")
library(psych)
library(corrplot)

head <- read.csv("/Users/Nikon/Desktop/CMC MSU/MC/5 sem/Data/AAPL.csv",
                header = TRUE)
num <- random <- sample(1:2, nrow(head), replace=TRUE)
num2 <- random <- sample(1:2, nrow(head), replace=TRUE)
head <- cbind(head, num)
head <- cbind(head, num2)
data <- head[-c(2,3,4,6)]
data$Date <- as.POSIXct(data$Date, format = "%Y")
data <- mutate_each(data, "factor", Date)
state <- as.data.frame(data[,c("Close", "Volume", "num", "num2")])
mk <- lm(data$Volume ~ data$Date + data$Close, state)
M <- cor(state)
col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#000000"))
corrplot(M, method = "color", col=col4(10), cl.length = 11,
         order = "AOE", addCoef.col = "red")
corrplot(M, method="shade", col=col4(10), cl.length = 11,
         order = "AOE", addCoef.col = "red")
corrplot(M, method="pi", col=col4(10), cl.length = 11,
         order = "AOE", addCoef.col = "red")

pairs.panels(data, 
             method = "pearson", 
             hist.col = "#00AFBB",
             density = TRUE,  
             ellipses = TRUE 
)
