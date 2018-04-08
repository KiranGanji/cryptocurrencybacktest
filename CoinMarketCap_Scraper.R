#Libraries
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)
library(stringr)
library(rvest)
library(httr)
library(lubridate)

#Setting the work directory
rm(list = ls())
setwd("/home/ubuntu/drive2/data2/ganji_kiran/Bitcoin")

#Main Code
urlmain <- "https://coinmarketcap.com/all/views/all/"
SOURCE <-  getURL(urlmain,encoding="UTF-8")
SourceParsed <- htmlParse(SOURCE)

CrpytoDetails <- html_table(read_html(SOURCE))
AllCrypto <- CrpytoDetails[[1]]
AllCrypto <- data.table(AllCrypto)

#Cleaning the AllCrypto files
AllCrypto[,Name:=str_split(Name,"\n")[[1]][2],by=1:nrow(AllCrypto)]
AllCrypto[,`Circulating Supply`:=gsub("\n","",`Circulating Supply`),]
AllCrypto[,`Circulating Supply`:=gsub(" ","",`Circulating Supply`),]
AllCrypto[,`Circulating Supply`:=gsub("\\*","",`Circulating Supply`),]
AllCrypto[,`Circulating Supply`:=gsub(",","",`Circulating Supply`),]
AllCrypto[,Price:=gsub("\\$","",Price),]


#Getting the links for each of names
listofurls <- xpathSApply(SourceParsed, "//a[@class='currency-name-container']",xmlGetAttr,"href")
AllCrypto[,URLS:=paste0("https://coinmarketcap.com",listofurls),]
write.csv(AllCrypto,"Raw_Data/AllCrypto.csv", row.names = F)

#Task 2 get the details of all the cryto currencies
urls <- AllCrypto[,c("Name","URLS"),with=FALSE]
pastescript <- "historical-data/?start=20130428&end=20180308"
AllData <- data.frame()
j <- 1

for(i in 1:nrow(AllCrypto)){
  print(i)
  name <- urls$Name[i]
  if(name %in% c("Enigma","HempCoin")){
    name <- paste0(name,"_",j)
    j <- j+1
  }
  url <- urls$URLS[i]
  url <- paste0(url,pastescript)
  SOURCE <-  getURL(url,encoding="UTF-8")
  Table <- data.table(html_table(read_html(SOURCE))[[1]])
  Table[,Name:=name,]
  AllData <- rbind(AllData,Table)
}

AllData <- AllData[!duplicated(AllData),,]
AllData[,Volume:=gsub(",","",Volume),]
AllData[,`Market Cap`:=gsub(",","",`Market Cap`),]
AllData[,Year:=substr(Date,nchar(Date)-4,nchar(Date)),]
AllData[,Month:=substr(Date,1,3),]
AllData[,Day:=substr(Date,4,6),]
AllData[,Month:=match(Month,month.abb),]

AllData <- AllData[!(is.na(Year)),,]

AllData[,`:=`(Year=as.numeric(Year), Day=as.numeric(Day),Month=as.numeric(Month)),]
AllData[,Date:=NULL,]
AllData$Date <- as.Date(with(AllData, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

AllData[,Date:=ymd(Date),]
AllData <- AllData[!is.na(Date),,]

colsorder <- c("Name","Date","Year","Month","Day","Open","High","Low","Close","Volume","Market Cap")
AllData <- AllData[,(colsorder),with=F]

#Converting character to numeric
charcols <- c("Open","High","Low","Close","Volume","Market Cap")

for(i in charcols){
  print(i)
  AllData[,(i):=as.numeric(get(i)),]
}

# sum(is.na(AllData$Close))
# AllData[is.na(Close),,]
AllData <- AllData[!is.na(Close),,]

write.csv(AllData,"Raw_Data/2018_04_04_AllCryptoCurrenciesOHLCNum.csv", row.names = F)






















