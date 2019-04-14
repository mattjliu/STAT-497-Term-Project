#Packages
install.packages(c("httr","jsonlite","dygraphs","xts","IRdisplay","htmlwidgets","repr","lubridate"))
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
coin.names=c("BTC","ETH","XRP","XMR","LTC")
coin.colours=c("blue","red","green","black","violet")
params <- list("start"=toString(as.numeric(as.POSIXct("2017-05-01 4:00:00"))),
               "end"=toString(as.numeric(as.POSIXct("2019-05-01 4:00:00"))),
               "period"="7200")
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
getLogReturns <- function(yt,wt){
  return(log(yt%*%wt))}


ETH <- get_data("ETH",params$start,params$end,params$period,52,FALSE)
XRP <- get_data("XRP",params$start,params$end,params$period,52,FALSE)
XMR <- get_data("XMR",params$start,params$end,params$period,52,FALSE)
LTC <- get_data("LTC",params$start,params$end,params$period,52,FALSE)

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



GradPolControl=function(init_weights, theSeed,length_Epis, alphasteptheta, startingpointepisode=0){ 
  
  actionseq=c()     #which crypto you select at each step
  stateseq=matrix(ncol = 5,nrow=length_Epis)  #the return of every crypto at every step
  colnames(stateseq)=c("Bitcoin","ETH","XRP","XMR","LTC")
  MaxReturn=c()             #the numerical value of the maximum return of the cryptos for every time step
  stateseq[1,]=c(0,0,0,0,0)  #no return at time 0
  actionseq[1]=1    #first period action is to keep bitcoin 
  MaxReturn[1]=0    #first period return is 0
  TotalReturn=c()
  RebalPort=matrix(nrow=(length_Epis-1),ncol=5)  #the rebalancing of the protfolio at each time step
  piaThetaPrev=matrix(ncol=5,nrow=length_Epis)  #storing the values of piaTHeta at each value
  piaThetaPrev[1,]=c(1,0,0,0,0)               #we start with only Bitcoins
  xsa=diag(x=1,nrow = 5,ncol = 5)             
  colnames(xsa)=c("a=0","a=1","a=2","a=3","a=4")
  weightedReturn=c()                      #weighted return of the portfolio at each time step
  weightedReturn[1]=0
  weighttest=matrix(ncol=5,nrow=length_Epis) #the weights of the port at each time step
  avmarkt=c()
  avmarkt[1]=sum(close[1+startingpointepisode,]/open[1+startingpointepisode,],1)/5 #average market return at each time step
  truevalue=c()    #for debug only
  gradlogpi=c()
  for(i in 2:length_Epis){
    CurrentRelativePrice=getPriceRelativeVec(open[i+startingpointepisode,],close[i+startingpointepisode,]) #calculates the current price relative to the price of the previous period
    statewithBitcoin=c(1,close[i+startingpointepisode,]/open[i+startingpointepisode,])
    stateseq[i,]=log(statewithBitcoin)
    MaxReturn[i]=max(stateseq[i,])            #tells you the value of the return of the crypto that had the max return for that time step
    actionseq[i]=which.max(stateseq[i,])      #tells you which stock had the max return
    hsatheta=xsa%*%init_weights
    piaTheta=(exp(hsatheta)/sum(exp(hsatheta))) #soft max
    piaThetaPrev[i-1,]=piaTheta 
    
    weightedReturn[i]=getLogReturns(statewithBitcoin,piaThetaPrev[i-1,])   #calulates weighted log return of the portfolio
    RebalPort[i-1,]=piaTheta-piaThetaPrev[i-1,] #calculates the rebalancing
    
    
    gradlogpi[1]=ifelse(actionseq[i]==1,1- piaTheta[1],-piaTheta[1])
    gradlogpi[2]=ifelse(actionseq[i]==2,1- piaTheta[2],-piaTheta[2])
    gradlogpi[3]=ifelse(actionseq[i]==3,1- piaTheta[3],-piaTheta[3])
    gradlogpi[4]=ifelse(actionseq[i]==4,1- piaTheta[4],-piaTheta[4])
    gradlogpi[5]=ifelse(actionseq[i]==5,1- piaTheta[5],-piaTheta[5])  #this result has been proven in the report
    init_weights=init_weights+alphasteptheta*MaxReturn[i]*gradlogpi  #using the REINFORCE algorithm
    weighttest[i,]=init_weights
    truevalue[i]=length(init_weights)==length(piaTheta)  #for debug only
    avmarkt[i]=sum(close[i+startingpointepisode,]/open[i+startingpointepisode,],1)/5  #calculates the average market return for that time step
  }
  
  Totalreturn=prod(exp(weightedReturn))
  AverageMarket=cumprod(avmarkt)[length_Epis]
  
  return_list=list(Totalreturn,AverageMarket,actionseq,piaThetaPrev, cumprod(avmarkt),cumprod(exp(weightedReturn)),weighttest,truevalue)
  names(return_list) <- c("AgentRetEpisode","MarketAverage","actions","prefs","MarketVec", "AgentVec","init_Weight","truevalue")
  return(return_list)
  
}
EPISODE1=GradPolControl(c(1,0,0,0,0),1980,8000,2^-4,startingpointepisode =0 ) 
EPISODE2=GradPolControl(c(1,0,0,0,0),1980,8000,2^-12,startingpointepisode = 0) 
EPISODE3=GradPolControl(c(1,0,0,0,0),1980,8000,2^-8,startingpointepisode = 0) 

(nrow(open)-2000)

plot(EPISODE1$MarketVec,col="red",type = "l",main="Agent vs Market",sub="Policy Gradient")
lines(EPISODE1$AgentVec, col="blue")
lines(EPISODE2$AgentVec, col="green")
lines(EPISODE3$AgentVec, col="orange")
legend("topright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"),cex=0.75)
grid(0,7,"black")


EPISODE4=GradPolControl(c(1,0,0,0,0),1980,3000,2^-4,startingpointepisode =5000 ) 
EPISODE5=GradPolControl(c(1,0,0,0,0),1980,3000,2^-12,startingpointepisode = 5000) 
EPISODE6=GradPolControl(c(1,0,0,0,0),1980,3000,2^-8,startingpointepisode = 5000) 
plot(EPISODE4$MarketVec,col="red",type = "l",main="Agent vs Market after the spike",sub="Policy Gradient")
lines(EPISODE4$AgentVec, col="blue")
lines(EPISODE5$AgentVec, col="green")
lines(EPISODE6$AgentVec, col="orange")
legend("bottomright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"), cex=0.75)
grid(0,7,"black")

EPISODE7=GradPolControl(c(1,0,0,0,0),1980,4000,2^-4,startingpointepisode =0) 
EPISODE8=GradPolControl(c(1,0,0,0,0),1980,4000,2^-12,startingpointepisode =0 ) 
EPISODE9=GradPolControl(c(1,0,0,0,0),1980,4000,2^-8,startingpointepisode = 0) 
plot(EPISODE7$MarketVec,col="red",type = "l",main="Agent vs Market before the spike",sub="Policy Gradient")
lines(EPISODE7$AgentVec, col="blue")
lines(EPISODE8$AgentVec, col="green")
lines(EPISODE9$AgentVec, col="orange")
legend("bottomright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"),cex=0.75)
grid(0,7,"black")




plot(EPISODE1$init_Weight[,1],col="orange", type="l",ylim=c(-.5,1.5), main="Preference", sub="Policy Gradient")
lines(EPISODE1$init_Weight[,2],col="blue")
lines(EPISODE1$init_Weight[,3],col="red")
lines(EPISODE1$init_Weight[,4],col="violet")
lines(EPISODE1$init_Weight[,5],col="gray")
legend("topright", c("Bitcoin", "ETH","XRP","XRM", "LTC"),fill=c("orange", "blue", "red","violet","gray"))

learningstep=c(2^-4,2^-8,2^-12)
episodesimul=array(NA,dim=c(41,2,3))
for(alphasize in 1:3){
  for(startingpoint in 1:41){
    episodesimul[startingpoint,1,alphasize]=GradPolControl(c(1,0,0,0,0),1980,200,alphasteptheta=learningstep[alphasize],startingpointepisode = (startingpoint-1)*200)$AgentRetEpisode 
    episodesimul[startingpoint,2,alphasize]=GradPolControl(c(1,0,0,0,0),1980,200,alphasteptheta=learningstep[alphasize],startingpointepisode = (startingpoint-1)*200)$MarketAverage
  }
}

counter1=0
counter2=0
counter3=0
for(i in 1:3){
  for(j in 1:41){
    if(i==1){
      if(episodesimul[j,1,i]>episodesimul[j,2,i]){
        counter1=counter1+1
      }
      
    } else if(i==2){
      if(episodesimul[j,1,i]>episodesimul[j,2,i]){
        counter2=counter2+1
      } 
      
    } else if(i==3){
      if(episodesimul[j,1,i]>episodesimul[j,2,i]){
        counter3=counter3+1
      }
    }
  }
}

PercentageOfSuccess=c(counter1/41,counter2/41,counter3/41)
PercentageOfSuccess
counter1
