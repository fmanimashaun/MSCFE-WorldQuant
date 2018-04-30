Google_raw_data <-  read.csv("Stocks/GOOG.csv")
Microsoft_raw_data <-  read.csv("Stocks/MSFT.csv")
Ebay_raw_data <-  read.csv("Stocks/EBAY.csv")
Cisco_raw_data <-  read.csv("Stocks/CSCO.csv")
Oracle_raw_data <-  read.csv("Stocks/ORCL.csv")

##Calculation Monthly return of Cisco##
Cisco_price_diff <- (-1* diff((as.numeric(as.matrix(Cisco_raw_data$Adj.Close)))
                                         [length(Cisco_raw_data$Adj.Close):1]))
Cisco_price_diff <- Cisco_price_diff[1:(length(Cisco_price_diff)-1)]
Cisco_Previous <- as.numeric(as.matrix(Cisco_raw_data$Adj.Close
                     [(length(Cisco_raw_data$Adj.Close)-1):2]))
Cisco_monthly_return <- (Cisco_price_diff/Cisco_Previous)*100

##Calculation Monthly return of Ebay##
Ebay_price_diff <- (-1*diff((as.numeric(as.matrix(Ebay_raw_data$Adj.Close)))
                                        [length(Ebay_raw_data$Adj.Close):1]))
Ebay_price_diff <- Ebay_price_diff[1:(length(Ebay_price_diff)-1)]
Ebay_Previous <- as.numeric(as.matrix(Ebay_raw_data$Adj.Close
                    [(length(Ebay_raw_data$Adj.Close)-1):2]))
Ebay_monthly_return <- (Ebay_price_diff/Ebay_Previous)*100

##Calculation Monthly return of Google##
Google_price_diff <- (-1*diff((as.numeric(as.matrix(Google_raw_data$Adj.Close)))
                                         [length(Google_raw_data$Adj.Close):1]))
Google_price_diff <- Google_price_diff[1:(length(Google_price_diff)-1)]
Google_Previous <- as.numeric(as.matrix(Google_raw_data$Adj.Close
                      [(length(Google_raw_data$Adj.Close)-1):2]))
Google_monthly_return <- (Google_price_diff/Google_Previous)*100

##Calculation Monthly return of Microsoft
Microsoft_price_diff <- (-1*diff((as.numeric(as.matrix(Microsoft_raw_data$
                        Adj.Close)))[length(Microsoft_raw_data$Adj.Close):1]))
Microsoft_price_diff <- Microsoft_price_diff[1:(length(Microsoft_price_diff)-1)]
Microsoft_Previous <- as.numeric(as.matrix(Microsoft_raw_data$Adj.Close
                         [(length(Microsoft_raw_data$Adj.Close)-1):2]))
Microsoft_monthly_return <- (Microsoft_price_diff/Microsoft_Previous)*100

##Calculation Monthly return of Oracle
Oracle_price_diff <- (-1*diff((as.numeric(as.matrix(Oracle_raw_data$Adj.Close)))
                                         [length(Oracle_raw_data$Adj.Close):1]))
Oracle_price_diff <- Oracle_price_diff[1:(length(Oracle_price_diff)-1)]
Oracle_Previous <- as.numeric(as.matrix(Oracle_raw_data$Adj.Close
                      [(length(Oracle_raw_data$Adj.Close)-1):2]))
Oracle_monthly_return <- (Oracle_price_diff/Oracle_Previous)*100

##-----Combining all the Monthly returns-----------##
Date <- sort((as.Date(as.character(Cisco_raw_data$Date[3:(length(Cisco_raw_data$
                                                   Date))]))),decreasing = TRUE)
Monthly_returns <- cbind.data.frame(Date, Cisco_monthly_return,Ebay_monthly_return,
         Google_monthly_return,Microsoft_monthly_return,Oracle_monthly_return)
colnames(Monthly_returns) <- c("Date", "CSCO", "EBAY", "GOOG", "MSFT", "ÖRCL")

## Building the Portfolios using combination function##
StockList <- c("CSCO", "EBAY","GOOG", "MSFT", "ÖRCL")
PortFolio <- combn(StockList, 3)
PortFolio_List <- list()
PortFolio_Stock_list <- list()
C.Returns <- c()
WPR.Mean <- c()
WPR.Median <- c()
WPR.STD <- c()
Var_CoV_Matrix <- list()
PortFolio_Var <- c()
Stock_weight <- as.matrix(c(rep(1/3,3)))

for (x in 1:10) 
{
    PortFolio_List[[paste0("Portfolio_",x)]] <-  cbind(Date,Monthly_returns
                                                       [PortFolio[,x]])
    ##Order The date in Portforlio list in increasing order
    PortFolio_List[[x]] <- PortFolio_List[[x]][order(PortFolio_List[[x]]$Date),]
    W.Returns <- rowSums(PortFolio_List[[x]][,2:4], na.rm = TRUE)/3
    PortFolio_List[[x]] <- cbind(PortFolio_List[[x]],W.Returns)
    PortFolio_Stock_list[[paste0("Portfolio_",x)]] <- colnames(PortFolio_List[[x]]
                                                                           [,2:4])
    for (i in 1: nrow(PortFolio_List[[x]])) {
        C.Returns[i] <- sum(PortFolio_List[[x]]$W.Returns[1:i])
        WPR.Mean[i] <- mean(PortFolio_List[[x]]$W.Returns)
        WPR.Median[i] <- median(PortFolio_List[[x]]$W.Returns)
        WPR.STD[i] <- sd(PortFolio_List[[x]]$W.Returns)
    }
    PortFolio_List[[x]] <- cbind(PortFolio_List[[x]],C.Returns, WPR.Mean,
                                 WPR.Median, WPR.STD )
    Stocks_Returns_less_Avg <- list()
    for (i in 1:3) {
        Stocks_Returns_less_Avg[[paste0(PortFolio_Stock_list[[x]][[i]],"_less_avg")]] <- PortFolio_List[[
                                         x]][[PortFolio_Stock_list[[x]][[i]]]] - mean(PortFolio_List[[x]]
                                                        [[PortFolio_Stock_list[[x]][[i]]]])
        PortFolio_List[[x]] <- cbind(PortFolio_List[[x]],Stocks_Returns_less_Avg[[i]])
    }
    Var_CoV_Matrix[[paste0("Portfolio_",x)]] <- (t(as.matrix(PortFolio_List[[x]]
                [,(ncol(PortFolio_List[[x]])-2):ncol(PortFolio_List[[x]])])) 
                %*% (as.matrix(PortFolio_List[[x]][,(ncol(PortFolio_List[[x]])-2)
                    :ncol(PortFolio_List[[x]])])))/(nrow(PortFolio_List[[x]])-1)
    colnames(Var_CoV_Matrix[[x]]) <- PortFolio_Stock_list[[x]]
    rownames(Var_CoV_Matrix[[x]]) <- PortFolio_Stock_list[[x]]
    PortFolio_Var[x] <- t(Stock_weight) %*% (as.matrix(Var_CoV_Matrix[[x]]) %*% Stock_weight)
}
names(PortFolio_Var) <- c(paste0("Portfolio 1"),paste0("Portfolio 2"),
                          paste0("Portfolio 3"),paste0("Portfolio 4"),
                          paste0("Portfolio 5"),paste0("Portfolio 6"),
                          paste0("Portfolio 7"),paste0("Portfolio 8"),
                          paste0("Portfolio 9"),paste0("Portfolio 10"))
PortFolio_Var <- as.matrix(PortFolio_Var)
colnames(PortFolio_Var) <- c("Overall Portfolio Variance")
PortFolio_Var <- as.data.frame(PortFolio_Var)
View(PortFolio_Var)

plot(PortFolio_List[[x]]$Date,PortFolio_List[[x]]$C.Returns, type = "l", 
     main = paste0("Portfolio ", x, " Cummulative Monthly Return 2017 - 2018"), 
     xlab = "Date", ylab = "Monthly Return")
abline(h = mean(PortFolio_List[[x]]$W.Returns, na.rm = TRUE), col = "red", lwd = 2)
abline(h = median(PortFolio_List[[x]]$W.Returns, na.rm = TRUE), col = "Yellow", lwd = 2)
abline(h = sd(PortFolio_List[[x]]$W.Returns, na.rm = TRUE), col = "blue", lwd = 2)
legend(x = "topright", c("Mean", "Median", "SD"),
       col = c("red", "yellow", "blue"),
       lwd = c(2, 2, 2))



