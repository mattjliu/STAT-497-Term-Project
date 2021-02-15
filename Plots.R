###Plotting functions#####
coin.names=c("BTC","ETH","XRP","XMR","LTC")
coin.colours=c("blue","red","green","black","violet")
subtitle=c("Method 1","Method 2","Method 3")
Pref.title=c("Preferences Over Time")
Pref.lim=c(-1,1)
Action.title=c("Actions Over Time")
Action.lim=c(-0.1,1)
Changes.title=c("Changes Over Time")
Changes.lim=c(0,2)
plot.contextual.list=function(run,name.vec,col.vec,title,subtitle,ylabel,ylimit){
  plot(run[,1],type = "l",col=c("blue"),ylim=ylimit,main = title,ylab=ylabel,xlab="Time Steps",sub=subtitle)
  lines(run[,2],type = "l",col="red")
  lines(run[,3],type = "l",col="green")
  lines(run[,4],type = "l",col="black")
  lines(run[,5],type = "l",col="violet")
  grid(0,7,"black")
  legend("topright",name.vec,fill = col.vec,cex = 0.8)
}
plot.currency=function(data,name.vec,col.vec){
  plot(data,type="l",col=col.vec,ylab= "Price",xlab="Period",main = name.vec)
}
plot.agent.mkt=function(AgentVec,MktVec,subtitle,ylimit){
  plot(AgentVec,type="l",col="blue",main = "Agent Return Vs Market Return",ylim = ylimit,ylab = "Returns",sub = subtitle)
  lines(MktVec,type="l",col="red")
  grid(0,7,"black")
  legend("topright",c("Agent Returns","Market Returns"),fill=c("blue","red"))
  
}
plot.context.compare=function(context1,context2,context3,mkt,title,ylimit){
  plot(context1,type = "l",col="blue",main = title,ylab = title,ylim=ylimit)
  lines(context2,type = "l",col = "red" )
  lines(context3,type = "l",col = "black" )
  lines(mkt,type = "l",col="green")
  grid(0,8,"black")
  legend("topright", c(subtitle,"Market") , fill=c("blue","red","black","green"),cex = 0.6)

}


