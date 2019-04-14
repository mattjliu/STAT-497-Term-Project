# Define dataframes
ETH <- get_data("ETH",params$start,params$end,params$period,52)
XRP <- get_data("XRP",params$start,params$end,params$period,52)
XMR <- get_data("XMR",params$start,params$end,params$period,52)
LTC <- get_data("LTC",params$start,params$end,params$period,52)
close <- cbind(ETH$close,XRP$close,XMR$close,LTC$close)
colnames(close) <- currency_vec
#Defining Parameters####
n=8000#     In practice the number of rows would be nrow(close) but for consistency of results 8000 is used
n_curren=5
DF=0.8
epsilon=0.9
alphavec=seq(0.1,0.4,0.05)
windowvec=6:12
theseed=100
#Running Contextual Simulation####
context.sim1=simulate_contextual1(n_curren,n,alpha = 0.4,window_size = 12,DF,close)
context.sim2=simulate_contextual2(n_curren,n,alpha = 0.1,window_size = 6,DF)
context.sim3=simulate_contextual3(n_curren,n,alpha = 0.3,window_size = 10,DF,epsilon,theseed,close)
context.sim1$AgentRt
Market.Returns=context.sim1$mktVec
YTD_Returns=get_ytd_return(close[1:n,])
#Agent Returns####
context.sim1$AgentRt ; context.sim2$AgentRt ; context.sim3$AgentRt
#Agents vs Mkts####
plot.agent.mkt(context.sim1$AgentVec,Market.Returns,subtitle[1],c(0,8))
plot.agent.mkt(context.sim2$AgentVec,Market.Returns,subtitle = subtitle[2],c(0.5,3.0))
plot.agent.mkt(context.sim3$AgentVec,Market.Returns,subtitle = subtitle[3],c(0,3))
#currency plots####
plot.currency(ETH$close,"ETH",coin.colours[2])
plot.currency(XRP$close,"XRP",coin.colours[3])
plot.currency(XMR$close,"XMR",coin.colours[4])
plot.currency(LTC$close,"LTC",coin.colours[5])
#Prefs Plots####
plot.contextual.list(context.sim1$prefs,coin.names,coin.colours,Pref.title,subtitle[1],"Preferences",c(-0.5,0.7))
plot.contextual.list(context.sim2$prefs,coin.names,coin.colours,Pref.title,subtitle[2],"Preferences",c(-0.3,0.2))
plot.contextual.list(context.sim3$prefs,coin.names,coin.colours,Pref.title,subtitle[3],"Preferences",c(-0.2,0.4))
#Comparing Results####
plot.context.compare(context.sim1$AgentVec,context.sim2$AgentVec,context.sim3$AgentVec,Market.Returns,"Returns",c(0,8))
#No Spike######
close.nospike<-close[6000:8000]
no.spike.sim1<-simulate_contextual1(5,2000,alpha = 0.4,12,0.8,close.nospike)
no.spike.sim2<-simulate_contextual2(5,2000,alpha = 0.4,12,0.8,close.nospike)
no.spike.sim3<-simulate_contextual3(5,2000,alpha = 0.3,10,0.8,0.9,99,close.nospike)
Market.returns.ns=no.spike.sim1$mktVec
#Comparing Results####
plot.context.compare(no.spike.sim1$AgentVec,no.spike.sim2$AgentVec,no.spike.sim3$AgentVec,Market.returns.ns,"Returns with no spike",c(0.8,1.5))
plot.agent.mkt(no.spike.sim1$AgentVec,Market.returns.ns, "method 1 no spike",c(0.9,1.4) )
plot.agent.mkt(no.spike.sim2$AgentVec,Market.returns.ns, "method 2 no spike",c(0.9,1.4) )
plot.agent.mkt(no.spike.sim3$AgentVec,Market.returns.ns, "method 3 no spike",c(0.9,1.5) )
no.spike.sim1$AgentRt;no.spike.sim2$AgentRt; no.spike.sim3$mktAve
######Getting Optimal Parameters for contextual Simulations(Grid Search)
####Each run takes a long time to run about 6 minutes each
#Context1.Param=get_param_mat(5,0.8,simulate_contextual1,alphavec,windowvec)
#Context2.Param=get_param_mat(5,0.8,simulate_contextual2,alphavec,windowvec)



####################      POLICY GRADIENT METHOD    #####################

EPISODE1=GradPolControl(c(1,0,0,0,0),1980,8000,2^-4,startingpointepisode =0 ) 
EPISODE2=GradPolControl(c(1,0,0,0,0),1980,8000,2^-12,startingpointepisode = 0) 
EPISODE3=GradPolControl(c(1,0,0,0,0),1980,8000,2^-8,startingpointepisode = 0)

###EPISODES 1-3 vs MKT####
plot(EPISODE1$MarketVec,col="red",type = "l",main="Agent vs Market",sub="Policy Gradient")
lines(EPISODE1$AgentVec, col="blue")
lines(EPISODE2$AgentVec, col="green")
lines(EPISODE3$AgentVec, col="orange")
legend("topright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"),cex=0.75)
grid(0,7,"black")

###EPISODES 4-6 vs MKT(after spike)####

EPISODE4=GradPolControl(c(1,0,0,0,0),1980,3000,2^-4,startingpointepisode =5000 ) 
EPISODE5=GradPolControl(c(1,0,0,0,0),1980,3000,2^-12,startingpointepisode = 5000) 
EPISODE6=GradPolControl(c(1,0,0,0,0),1980,3000,2^-8,startingpointepisode = 5000) 
plot(EPISODE4$MarketVec,col="red",type = "l",main="Agent vs Market after the spike",sub="Policy Gradient")
lines(EPISODE4$AgentVec, col="blue")
lines(EPISODE5$AgentVec, col="green")
lines(EPISODE6$AgentVec, col="orange")
legend("bottomright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"), cex=0.75)
grid(0,7,"black")


###EPISODES 7-9 vs MKT(before spike)####

EPISODE7=GradPolControl(c(1,0,0,0,0),1980,4000,2^-4,startingpointepisode =0) 
EPISODE8=GradPolControl(c(1,0,0,0,0),1980,4000,2^-12,startingpointepisode =0 ) 
EPISODE9=GradPolControl(c(1,0,0,0,0),1980,4000,2^-8,startingpointepisode = 0) 
plot(EPISODE7$MarketVec,col="red",type = "l",main="Agent vs Market before the spike",sub="Policy Gradient")
lines(EPISODE7$AgentVec, col="blue")
lines(EPISODE8$AgentVec, col="green")
lines(EPISODE9$AgentVec, col="orange")
legend("bottomright",c("Average Market Return", "Agent Return with alpha=2^-4","Agent Return with alpha=2^-8","Agent Return with alpha=2^-12"),fill=c("red","blue","orange", "green"),cex=0.75)
grid(0,7,"black")



####Preference Plots###
plot(EPISODE1$init_Weight[,1],col="orange", type="l",ylim=c(-.5,1.5), main="Preference", sub="Policy Gradient")
lines(EPISODE1$init_Weight[,2],col="blue")
lines(EPISODE1$init_Weight[,3],col="red")
lines(EPISODE1$init_Weight[,4],col="violet")
lines(EPISODE1$init_Weight[,5],col="gray")
legend("topright", coin.names,fill=c("orange", "blue", "red","violet","grey"))

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


