# Packages
require("httr")
library("jsonlite")
require("dygraphs")
require("xts")
require("IRdisplay")
require("htmlwidgets")
require("repr")
require("lubridate")

# All relevant params
currency_vec = c("ETH","XRP","XMR","LTC")
params <- list("start"=toString(as.numeric(as.POSIXct("2017-05-01 4:00:00"))),
                    "end"=toString(as.numeric(as.POSIXct("2018-05-01 4:00:00"))),
                    "period"="14400")
base <- "https://poloniex.com/public?command=returnChartData"

# Function that gets the chart data using api
get_data <- function(curr,start,end,period,length,display){
    s <- paste(base,"&currencyPair=BTC_",curr,"&start=",start,"&end=",end,"&period=",period,sep="")
    prices <- data.frame(fromJSON(content(GET(s),"text"),flatten=TRUE))
    prices$date <- as.POSIXct(as.numeric(prices$date), origin="1970/01/01")
    prices$date <- ymd_hms(prices$date)
    series <- xts(prices[,-1], order.by=prices$date)

    if(display){
    	m <- dygraph(tail(series[,c(1,2,3,4)],n=length), main=paste("BTC_",curr," Exchange",sep="")) %>% 
	    dyCandlestick() %>% 
	    dyOptions(sigFigs=8) 
	    htmlwidgets::saveWidget(m, "m.html")
	    display_html('<iframe src="m.html" width=100% height=450></iframe>')
    }
    return(series)
}