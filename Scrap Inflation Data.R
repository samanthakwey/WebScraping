library(rvest)
library(dplyr)
library(reshape2)
library(lubridate)

InflationData = read_html("http://www.usinflationcalculator.com/inflation/current-inflation-rates/") %>% html_nodes("table") %>% .[[1]]  %>% html_table()
colnames(InflationData) = InflationData[1,]
InflationData = InflationData[-1,-length(InflationData)]
InflationData = melt(InflationData, id.vars = c("Year"),
              variable.name = "Month", 
              value.name = "inflation.rate")

InflationData$Month = month(as.Date(paste(InflationData$Month,"-01-2017",sep=""),"%b-%d-%Y"))
InflationData = InflationData[order(InflationData$Year,InflationData$Month),]

InflationData = InflationData[-which(InflationData$inflation.rate==""),]
















