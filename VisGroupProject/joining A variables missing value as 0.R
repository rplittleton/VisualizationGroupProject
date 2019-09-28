data<- read.csv(file.choose(),header=T)
total.data <- data[grep("Total, all enterprises", data$Size.of.enterprise),]
total.data[is.na(total.data)] <- 0
total.data$A8.Total.Incident <- total.data$A1..Disrupt.or.Deface + total.data$A2..Steal.Information + total.data$A3..Steal.Money + total.data$A4..Steal.Intellectual.Property + total.data$A5..Access.Unauthorized.Area + total.data$A6..Monitor.and.Track.Activity + total.data$A7..Unknown.Motive
