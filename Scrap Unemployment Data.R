library(rvest)
library(dplyr)
library(reshape2)
library(lubridate)

UEData = read_html("https://data.bls.gov/timeseries/LNS14000000") %>% html_nodes("table") %>% .[[2]]  %>% html_table()
UEData = melt(UEData, id.vars = c("Year"),
              variable.name = "Month", 
              value.name = "unemploy.rate")

UEData$Month = month(as.Date(paste(UEData$Month,"-01-2017",sep=""),"%b-%d-%Y"))
UEData = UEData[order(UEData$Year,UEData$Month),]

UEData = UEData[-which(is.na(UEData$unemploy.rate)),]
