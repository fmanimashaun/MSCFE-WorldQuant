Monthly_returns <-  read.csv("Monthly_returns.csv")
StockList <- c("CSCO", "EBAY","GOOG", "MSFT", "ÖRCL")
PortFolio <- combn(StockList, 3)
PortFolio_List <- list()

for (x in 1:10) 
{
    PortFolio_List[[paste0("Portfolio_",x)]] <-  Monthly_returns[PortFolio[,x]]
}
filenames <- names(PortFolio_List)
for (x in 1:10) {
    write.csv(PortFolio_List[x], paste0(filenames[x], ".csv"))
}

