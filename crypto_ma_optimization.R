#Libraries
library(data.table)
library(zoo)
library(ggplot2)
library(lubridate)
library(nloptr)

#Setting the work directory
rm(list = ls())
setwd("/home/ubuntu/drive2/data2/ganji_kiran/Bitcoin")

#Profit calculation function
GenerateProfit <- function(Df,shortmove,longmove,DfOpen,Datemap=NULL){
  Df[,CrossOver:=ifelse(get(paste0("ma_",shortmove)) >= get(paste0("ma_",longmove)),1,-1),]
  Df[,CrossOverMain:=c(rep(0,1),diff(CrossOver)),]
  
  #Getting the options
  Df[,Trade:="No Trade",]
  Df[CrossOverMain == -2,Trade:="BUY",]
  Df[CrossOverMain == 2,Trade:="SELL",]
  
  #Get the next day date
  Df[,nextday:=c(Df[2:nrow(Df),get("timeseries"),],rep(NA,1)),]
  Df[is.na(nextday),nextday:=(timeseries+1),]#Automate this?
  
  #Merging with open contracts
  Df <- merge(Df,DfOpen, by.x = c("nextday"), by.y = c("timeseries"),all.x = T)
  
  colsorder <- c("timeseries",depvar,paste0("ma_",longmove),paste0("ma_",shortmove),
                 "CrossOver","CrossOverMain","Trade","nextday","Close_Open")
  Df <- Df[,(colsorder),with=F]
  
  #Calculating the profits
  ProfitFrame <- Df[Trade %in% c("SELL","BUY"),,]
  ProfitFrame[,Profit_from_long:=c(rep(NA,1),diff(Close_Open)),]
  ProfitFrame[Trade=="BUY",Profit_from_long:=0,]
  
  ProfitFrame[,Profit_from_short:=c(rep(NA,1),diff(Close_Open)),]
  ProfitFrame[,Profit_from_short:= Profit_from_short*(-1),]
  ProfitFrame[Trade=="SELL",Profit_from_short:=0,]
  
  
  ProfitFrame[,Total_profit:=(Profit_from_long + Profit_from_short),]
  ProfitFrame[is.na(Total_profit),Total_profit:=0,]
  
  colsreqd <- c("timeseries","Profit_from_long","Profit_from_short","Total_profit")
  ProfitFrame <- ProfitFrame[,(colsreqd),with=F]
  
  
  Df <- merge(Df,ProfitFrame,by=c("timeseries"),all.x = T)
  
  for(j in names(Df))
    set(Df,which(is.na(Df[[j]])),j,0)
  
  profit <- sum(Df$Total_profit, na.rm = T)
  totaltrades <- nrow(Df[Trade %in% c("SELL","BUY"),,])
  return(list(profit,totaltrades))
}


#============== Doing for non-imputed data
DataDf <- fread("Processed/AllCrypto_FullHist_cleaned.csv")
DataDfOpen <- DataDf[,c("Name","Date","Open"),with=F]
DataDf <- DataDf[,c("Name","Date","Close"),with=F]
setnames(DataDf,"Date","timeseries")
setnames(DataDfOpen,"Date","timeseries")

Filteredcryptos <- fread("Processed/FilteredCryptos.csv")
reqdcrypto <- unique(Filteredcryptos$Name)

ResultDf <- data.frame()

for(con in reqdcrypto){
  print(con)
  Contract3 <- DataDf[Name == con,,]
  Contract3 <- na.omit(Contract3)
  Contract3[,timeseries:=ymd(timeseries),]
  Contract3[,Name:=NULL,]
  
  Contract3Open <- DataDfOpen[Name==con,,]
  Contract3Open <- na.omit(Contract3Open)
  Contract3Open[,timeseries:=ymd(timeseries),]
  Contract3Open[,Name:=NULL,]
  
  depvar <- "Close"
  
  #Calculating the moving averages
  for(i in seq(2,50,1)){
    newvar <- paste0("ma_",i)
    Contract3[,(newvar):=c(rep(NA,(i-1)),rollmean(Contract3[,get(depvar),],k = i)),]
  }
  
  Contract3 <- na.omit(Contract3)
  Contract3Open <- na.omit(Contract3Open)
  colnames(Contract3Open)[2] <- paste0(depvar,"_Open")
  
  #Doing a greedy search
  eg <- expand.grid(seq(2,50,1),seq(2,50,1))
  eg <- data.table(eg)
  colnames(eg) <- c("shortmove","longmove")
  eg <- eg[shortmove!=longmove,,]
  eg <- eg[shortmove >= longmove,,]
  eg[,Profit:=0,]
  eg[,TotalTrades:=0,]
  
  for(i in 1:nrow(eg)){
    # print(i)
    sm <- eg[i,shortmove,]
    lm <- eg[i,longmove,]
    pf <- GenerateProfit(Contract3,sm,lm,Contract3Open)
    eg[i,Profit:=pf[[1]],]
    eg[i,TotalTrades:=pf[[2]],]
  }
  eg[,Name:=con,]
  setorder(eg,-Profit)
  ResultDf <- rbind(ResultDf,eg)
}


write.csv(ResultDf,"TechnicalInd/AllMas_results.csv", row.names = F)

ResultDf2 <- data.frame()
for(con in reqdcrypto){
  tempDf <- ResultDf[Name==con,,]
  setorder(tempDf,-Profit)
  ResultDf2 <- rbind(ResultDf2,tempDf[1,,])
}

write.csv(ResultDf2,"TechnicalInd/MaxProfits.csv", row.names = F)






































