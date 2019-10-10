data<- read.csv(file.choose(),header=T)
library(ggplot2)
data2<- data[c(1,10,47:55)]
data3< data[]
library(rpart)
library(datasets)

pairs(data2)
plot(data2$A8.Total.Incident, data2$B1..Mobile.Security)


