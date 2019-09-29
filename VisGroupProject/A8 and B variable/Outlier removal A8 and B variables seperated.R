data<- read.csv(file.choose(),header=T)
MD <- mahalanobis(data[, c(10:23)], colMeans(data[c(10:23)]), cov(data[,c(10:23)])) #create mahalanobis value
data$MD <- round(MD,3) #roudning MD values to 3 decimal places
boxplot(data$MD, ylab= "Mahalanobis Distance") #seeing outliers based on MD value
title(" Total Incident and Cyber Security Measures in Place") # adding title to boxplot
CyberSecurity.Measure <- data[c(10:23,56)] #creating new dataset for only total incident and Cyber Security Measures in place
CyberSecurity.Measure = CyberSecurity.Measure[CyberSecurity.Measure$MD < 30,] #removing outliers

