#Libraries
library(data.table)
library(lubridate)
library(zoo)
library(ggplot2)

#Setting the work directory
rm(list = ls())
setwd("/home/ubuntu/drive2/data2/ganji_kiran/Bitcoin")

#Main code
DataDf <-fread("Raw_Data/2018_04_04_AllCryptoCurrenciesOHLCNum.csv")
DataDf[,Date:=ymd(Date),]


DataMain <- fread("Raw_Data/AllCrypto.csv")
DataMain <- DataMain[`Market Cap` >0,,]

#Cleaning the AllCrypto file
colstodelte <- c("#","Volume (24h)","% 1h","% 24h","% 7d","URLS")
DataMain[,(colstodelte):=NULL,]

#Removing the $ and , from Market cap, Price and Circulting supply columns
DataMain[,`Market Cap`:=gsub("\\?",NA,`Market Cap`),]
DataMain[,`Market Cap`:=gsub("\\$","",`Market Cap`),]
DataMain[,`Market Cap`:=gsub(",","",`Market Cap`),]
DataMain[,`Market Cap`:= as.numeric(`Market Cap`),]

DataMain[,Price:=gsub("\\?",NA,Price),]
DataMain[,`Circulating Supply`:=gsub("\\?",NA,`Circulating Supply`),]
DataMain[,`:=`(Price=as.numeric(Price), `Circulating Supply`=as.numeric(`Circulating Supply`)),]

#Filtering out the cryptos with market capital greater than zero
DataMain <- DataMain[!is.na(`Market Cap`),,]

#Making changes to the duplicate cryptos
DataMain[Symbol =="ENG",Name:="Enigma_1",]
DataMain[Symbol =="XNG",Name:="Enigma_3",]

DataMain[Symbol =="THC",Name:="HempCoin_2",]
DataMain[Symbol =="HMP",Name:="HempCoin_4",]

#Filtering out the Data with positive market capital
DataDf <- DataDf[Name %in% DataMain$Name,,]
DataDf2 <- DataDf[,colnames(DataDf)[c(1,2,3,4,5,6,9)],with=F]
DataDf2 <- na.omit(DataDf2)

#Initial Aggregation of the data
AggDf <- DataDf2[,list(MinDate=min(Date),HistricalDataPoints = .N),by=list(Name)]
greaterthan90 <- AggDf[HistricalDataPoints > 90,Name,]

#Getting the latest all market capital of 
# DataMain <- DataMain[Name %in% greaterthan90,,]
# sum(DataMain$`Market Cap`)

#Number of currencies being added every year
# AggDf <- AggDf[Name %in% greaterthan90,,]
# AggDf[,MinDate:=ymd(MinDate),]
# AggDf[,Year:=year(MinDate),]
# write.csv(AggDf,"Data/AggData.csv", row.names = F)
# write.csv(DataMain,"Data/AllCryptoReqd.csv", row.names = F)


#Getting the 10,20,30,40,50 day moving averages
ResultDf <- data.frame()
counts <- 1
for(i in greaterthan90){
  print(i)
  ResultDf[counts,"Name"] <- i
  tempDf <- DataDf2[Name==i,,]
  setorder(tempDf,Date)
  for(j in seq(10,50,10)){
    newvar <- paste0("SMA_",j)
    tempDf[,(newvar):=c(rep(NA,j-1),rollmean(tempDf$Close,k = j)),]
    ResultDf[counts,paste0("Price_greater_",j)] <- ifelse(
      tempDf[nrow(tempDf),Close,] >= tempDf[nrow(tempDf),get(newvar),],"Yes","No")
    ResultDf[counts,paste0("Std_",j)] <- sd(tempDf[(nrow(tempDf)-(j-1)):(nrow(tempDf)),Close,])
  }
  counts <- counts +1
}

colsorder <- colnames(ResultDf)[c(1,2,4,6,8,10,3,5,7,9,11)]
ResultDf <- data.table(ResultDf)
ResultDf <- ResultDf[,(colsorder),with=F]


ResultDf2 <- ResultDf[Price_greater_10 == "Yes" &
                       Price_greater_20 == "Yes" &
                       Price_greater_30 == "Yes" &
                       Price_greater_40 == "Yes" & Price_greater_50 == "Yes" ,,]


#Merging with the DataMain to get the prices
ResultDf2 <- merge(ResultDf2,DataMain, by=c("Name"), all.x=T)
ResultDf2 <- ResultDf2[Price>0.25,,]

write.csv(ResultDf2,"Processed/FilteredCryptos.csv", row.names = F)
write.csv(ResultDf,"Processed/Price_variations.csv", row.names = F)
write.csv(DataDf2,"Processed/AllCrypto_FullHist_cleaned.csv", row.names = F)














