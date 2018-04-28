Raw_data <- read.csv("NASDAQ Composite data(Apr16 - Apr18).csv")
Raw_data$Date <- as.Date(Raw_data$Date)
Last_one_year_data <- Raw_data[(Raw_data$Date >= "2017-04-18"),]
price_diff <- (-1* diff(Last_one_year_data$Adj.Close[length(Last_one_year_data$Adj.Close):1]))
Price_less_one <- Last_one_year_data$Adj.Close[(length(Last_one_year_data$Adj.Close)-1):1]
daily_return <- (price_diff/Price_less_one)*100
daily_return[length(daily_return)+1]<- NA
New_data <- cbind.data.frame(Last_one_year_data,daily_return)
plot(Last_one_year_data$Date, Last_one_year_data$Adj.Close, type = "l",
     main = "NASDAQ Composite Price (2017-2018)", xlab = "Time", ylab = "Price($)")
colnames(New_data)[ncol(New_data)] <- "%Change"
bin <- as.data.frame(table(cut((New_data$`%Change`),seq(-4,4,1),right = FALSE)))
colnames(bin) <- c("Daily Return", "Frequency")
View(bin)
hist(New_data$`%Change`,breaks=seq(-4,4,1), right = FALSE, main = "Daily Return For NASDAQ Composite (2017 - 2018)"
     , xlab = "Daily Return", ylab = "Frequency")
abline(v = mean(New_data$`%Change`, na.rm = TRUE), col = "red", lwd = 2)
abline(v = median(New_data$`%Change`, na.rm = TRUE), col = "Yellow", lwd = 2)
abline(v = sd(New_data$`%Change`, na.rm = TRUE), col = "blue", lwd = 2)
legend(x = "topright", c("Mean", "Median", "SD"),
       col = c("red", "yellow", "blue"),
       lwd = c(2, 2, 2))
