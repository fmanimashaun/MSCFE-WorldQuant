Google_raw_data <-  read.csv("GOOG.csv")
Microsoft_raw_data <-  read.csv("MSFT.csv")
Ebay_raw_data <-  read.csv("EBAY.csv")
Cisco_raw_data <-  read.csv("CSCO.csv")
Oracle_raw_data <-  read.csv("ORCL.csv")

##Calculation Monthly return of Cisco

Cisco_price_diff <- (-1* diff((as.numeric(as.matrix(Cisco_raw_data$Adj.Close)))[
    length(Cisco_raw_data$Adj.Close):1]))
Cisco_price_diff <- Cisco_price_diff[1:(length(Cisco_price_diff)-1)]
Cisco_Previous <- as.numeric(as.matrix(Cisco_raw_data$Adj.Close[
    (length(Cisco_raw_data$Adj.Close)-1):2]))
Cisco_monthly_return <- (Cisco_price_diff/Cisco_Previous)*100

##Calculation Monthly return of Ebay

Ebay_price_diff <- (-1* diff((as.numeric(as.matrix(Ebay_raw_data$Adj.Close)))[
    length(Ebay_raw_data$Adj.Close):1]))
Ebay_price_diff <- Ebay_price_diff[1:(length(Ebay_price_diff)-1)]
Ebay_Previous <- as.numeric(as.matrix(Ebay_raw_data$Adj.Close[
    (length(Ebay_raw_data$Adj.Close)-1):2]))
Ebay_monthly_return <- (Ebay_price_diff/Ebay_Previous)*100

##Calculation Monthly return of Google

Google_price_diff <- (-1* diff((as.numeric(as.matrix(Google_raw_data$Adj.Close)))[
    length(Google_raw_data$Adj.Close):1]))
Google_price_diff <- Google_price_diff[1:(length(Google_price_diff)-1)]
Google_Previous <- as.numeric(as.matrix(Google_raw_data$Adj.Close[
    (length(Google_raw_data$Adj.Close)-1):2]))
Google_monthly_return <- (Google_price_diff/Google_Previous)*100

##Calculation Monthly return of Microsoft

Microsoft_price_diff <- (-1* diff((as.numeric(as.matrix(Microsoft_raw_data$Adj.Close)))[
    length(Microsoft_raw_data$Adj.Close):1]))
Microsoft_price_diff <- Microsoft_price_diff[1:(length(Microsoft_price_diff)-1)]
Microsoft_Previous <- as.numeric(as.matrix(Microsoft_raw_data$Adj.Close[
    (length(Microsoft_raw_data$Adj.Close)-1):2]))
Microsoft_monthly_return <- (Microsoft_price_diff/Microsoft_Previous)*100

##Calculation Monthly return of Oracle

Oracle_price_diff <- (-1* diff((as.numeric(as.matrix(Oracle_raw_data$Adj.Close)))[
    length(Oracle_raw_data$Adj.Close):1]))
Oracle_price_diff <- Oracle_price_diff[1:(length(Oracle_price_diff)-1)]
Oracle_Previous <- as.numeric(as.matrix(Oracle_raw_data$Adj.Close[
    (length(Oracle_raw_data$Adj.Close)-1):2]))
Oracle_monthly_return <- (Oracle_price_diff/Oracle_Previous)*100

##-----Combining all the Monthly returns-----------##
Date <- sort((as.Date(as.character(Cisco_raw_data$Date[3:(length(Cisco_raw_data$
                                                   Date))]))),decreasing = TRUE)

Monthly_returns <- cbind.data.frame(Date, Cisco_monthly_return,Ebay_monthly_return,
         Google_monthly_return,Microsoft_monthly_return,Oracle_monthly_return)
colnames(Monthly_returns) <- c("Date", "CSCO", "EBAY", "GOOG", "MSFT", "ÖRCL")

## Building the Portfolio using combination function
StockList <- c("CSCO", "EBAY","GOOG", "MSFT", "ÖRCL")
PortFolio <- combn(StockList, 3)
PortFolio_List <- list()

for (x in 1:10) 
{
    PortFolio_List[[paste0("Portfolio_",x)]] <-  cbind(Date,Monthly_returns[PortFolio[,x]])
}

##Order The date in Portforlio list in increasing order


