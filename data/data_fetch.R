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
get_data <- function(curr,start,end,period,length){
    s <- paste(base,"&currencyPair=BTC_",curr,"&start=",start,"&end=",end,"&period=",period,sep="")
    prices <- data.frame(fromJSON(content(GET(s),"text"),flatten=TRUE))
    prices$date <- as.POSIXct(as.numeric(prices$date), origin="1970/01/01")
    prices$date <- ymd_hms(prices$date)
    series <- xts(prices[,-1], order.by=prices$date)
    
    m <- dygraph(tail(series[,c(1,2,3,4)],n=length), main=paste("BTC_",curr," Exchange",sep="")) %>% 
    dyCandlestick() %>% 
    dyOptions(sigFigs=8) 
    htmlwidgets::saveWidget(m, "m.html")
    display_html('<iframe src="m.html" width=100% height=450></iframe>')
    
    return(series)
}

ETH <- get_data("ETH",params$start,params$end,params$period,52)
XRP <- get_data("XRP",params$start,params$end,params$period,52)
XMR <- get_data("XMR",params$start,params$end,params$period,52)
LTC <- get_data("LTC",params$start,params$end,params$period,52)

high <- cbind(ETH$high,XRP$high,XMR$high,LTC$high)
low <- cbind(ETH$low,XRP$low,XMR$low,LTC$low)
open <- cbind(ETH$open,XRP$open,XMR$open,LTC$open)
close <- cbind(ETH$close,XRP$close,XMR$close,LTC$close)

# Rename all the columns
colnames(high)  <- currency_vec
colnames(low) <- currency_vec
colnames(open) <- currency_vec
colnames(close) <- currency_vec
prices_vec <- list(high,low,open,close)

# function that returns price relative vector (yt)
# params: previous vector and current vector
getPriceRelativeVec <- function(prev_v,curr_v)
    return(c(1,curr_v)/c(1,prev_v))

# function that returns normalized price matrix
# params: prices = dataframe (hi,lo,close), vt = closing prices, n = number of timesteps in the past
# prices dataframe must end at time t
getPriceMatrix <- function(prices,vt,n){
    mat <- matrix(tail(prices,n),ncol=4)
    vec <- as.vector(vt)
    return(t(sweep(mat,2,vec,'/')))
}

# function that returns log returns given price-relative vector and weights vector
# params: yt = price-relative vector at time t, wt = weight vector at time t
getLogReturns <- function(yt,wt)
    return(log(dot(yt,wt)))