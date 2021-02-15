#####Contextual Bandit 1#####
# simulates contextual bandits problem
# params
# n_curren: number of currencies in proble (including cash)
# n_steps: number of timesteps in the episode
# alpha: learning rate of the action preference functions
# window_size: size of rolling window of asset prices to consider in the action preferences
# discount: discount factor of previous price changes
simulate_contextual1 <- function(n_curren,n_steps,alpha,window_size,discount,data){
  
  Ht <- rep(0,5) # initialize preference vector
  
  # Initialize weight vector for moving average window
  weight_vec <- c()
  for(k in 0:(window_size-1))
    weight_vec <- c(weight_vec,discount^k)
  weight_vec <- rev(weight_vec)
  
  price <- head(data,window_size+1) # initializes the price dataframe as the first 2 price vectors
  #st <- getPriceRelativeVec(price[1,],price[2,]) # get first price relative vector (first state)
  
  # history vector of price changes
  history <- getPriceRelativeVec(price[1,],price[2,])
  for(h in 2:window_size){
    history <- rbind(history,getPriceRelativeVec(price[h,],price[h+1,]))
  }
  
  # Define state st as vector of discounted previous relative changes
  st <- weight_vec %*% history
  
  prev_v <- tail(price,1) # initializes first price vector v
  
  # Define reward vecs and random action reward vectors
  Rvec <- c()
  Rvec_random <- c()
  Rvec_market <- c()
  market_average <- rep(1/5,5)
  
  # initialize matrices
  piMat <- matrix(c(1,0,0,0,0),nrow=1,byrow=T)
  hMat <- matrix(Ht,nrow=1,byrow=T)
  yMat <- matrix(rep(0,n_curren),nrow=1,byrow=T)
  
  for(i in window_size:n_steps){
    # get price dataframe of current time step
    price <- head(data,i+2)
    # get the current v
    curr_v <- tail(price,1)
    # get price change
    yt <- getPriceRelativeVec(prev_v,curr_v)
    
    # update history matrix
    history <- rbind(tail(history,window_size-1),yt)
    
    # preference vector for state s (element-wise multiplication of Ht and st)
    Ht_s <- st * Ht
    
    # Compute pivec (softmaxes for each currency)
    piVec <- c()
    for(a in 1:n_curren)
      piVec <- c(piVec,get_softmax(a,Ht_s))
    
    # get the prefered action
    action <- which.max(piVec)
    
    # get the log returns for our action (in this case our action is the softax)
    rt <- exp(getLogReturns(yt,piVec))
    Rvec <- c(Rvec,rt)
    
    # Reward for random action
    Rvec_random <- c(Rvec_random,exp(getLogReturns(yt,random_action(4))))
    
    # Reward for market average
    Rvec_market <- c(Rvec_market,exp(getLogReturns(yt,market_average)))
    
    # Update preference vector
    Ht <- get_update(rt,Rvec,Ht,action,alpha)

    prev_v <- curr_v
    st <- weight_vec %*% history
    
    # append to matrices
    piMat <- rbind(piMat,piVec)
    hMat <- rbind(hMat,Ht)
    yMat <- rbind(yMat,yt)
    
  }
  
  return_list <- list(prod(Rvec),prod(Rvec_market),piMat,hMat,yMat,cumprod(Rvec),cumprod(Rvec_market))
  names(return_list) <- c("AgentRt","mktAve","actions","prefs","changes","AgentVec","mktVec")

  return(return_list)
  
}



#####Contextual Bandit 2#####
# simulates contextual bandits problem
# This time action preferences are updated as if we pull all bandits and each timestep
# This is because we have full information of the environment
# params
# n_curren: number of currencies in proble (including cash)
# n_steps: number of timesteps in the episode
# alpha: learning rate of the action preference functions
# window_size: size of rolling window of asset prices to consider in the action preferences
# discount: discount factor of previous price changes
simulate_contextual2 <- function(n_curren,n_steps,alpha,window_size,discount,data){
  Ht <- rep(0,5) # initialize preference vector
  # initialize matrices
  piMat <- matrix(c(1,0,0,0,0),nrow=1,byrow=T)
  hMat <- matrix(Ht,nrow=1,byrow=T)
  yMat <- matrix(rep(0,n_curren),nrow=1,byrow=T)
  # Initialize weight vector for moving average window
  weight_vec <- c()
  for(k in 0:(window_size-1))
    weight_vec <- c(weight_vec,discount^k)
  weight_vec <- rev(weight_vec)
  
  price <- head(data,window_size+1) # initializes the price dataframe as the first 2 price vectors
  
  # history vector of price changes
  history <- getPriceRelativeVec(price[1,],price[2,])
  for(h in 2:window_size){
    history <- rbind(history,getPriceRelativeVec(price[h,],price[h+1,]))
  }
  
  # Define state st as vector of discounted previous relative changes
  st <- weight_vec %*% history
  
  prev_v <- tail(price,1) # initializes first price vector v
  
  # Define reward vecs and random action reward vectors
  Rvec <- c()
  Rvec_random <- c()
  Rvec_market <- c()
  market_average <- rep(1/5,5)
  R_all <- c(0)
  
  for(i in window_size:n_steps){
    # get price dataframe of current time step
    price <- head(data,i+2)
    # get the current v
    curr_v <- tail(price,1)
    # get price change
    yt <- getPriceRelativeVec(prev_v,curr_v)
    
    # update history matrix
    history <- rbind(tail(history,window_size-1),yt)
    
    # preference vector for state s (element-wise multiplication of Ht and st)
    Ht_s <- st * Ht
    
    # Compute pivec (softmaxes for each currency)
    
    piVec <- c()
    for(a in 1:n_curren)
      piVec <- c(piVec,get_softmax(a,Ht_s))
    
    # get the log returns for our action (in this case our action is the softax)
    rt <- exp(getLogReturns(yt,piVec))
    Rvec <- c(Rvec,rt)
    
    # get the log returns for pulling each arm
    Rvec_bandits <- c()
    for(a in 1:n_curren)
      Rvec_bandits <- c(Rvec_bandits,log(yt[a]))
    
    # update preference vector
    Ht <- get_update_all_bandits(Rvec_bandits,R_all,Ht,alpha)
       
    # check if we are at first timestep, and if so, initialize R_all vector, else append to vector
    if(i == window_size)
      R_all <-  Rvec_bandits
    else
      R_all <- c(R_all,Rvec_bandits)
    
    # Append to R_all vector
    R_all <- c(R_all,Rvec_bandits)
    
    # Reward for random action
    Rvec_random <- c(Rvec_random,exp(getLogReturns(yt,random_action(4))))
    
    # Reward for market average
    Rvec_market <- c(Rvec_market,exp(getLogReturns(yt,market_average)))
    
    # update prevsous price and state
    prev_v <- curr_v
    st <- weight_vec %*% history
    # append to matrices
    piMat <- rbind(piMat,piVec)
    hMat <- rbind(hMat,Ht)
    yMat <- rbind(yMat,yt)
    
  }
  
  return_list <- list(prod(Rvec),prod(Rvec_market),piMat,hMat,yMat, cumprod(Rvec),cumprod(Rvec_market))
  names(return_list) <- c("AgentRt","mktAve","actions","prefs","changes","AgentVec","MktVec")
  
  return(return_list)
}

#####Contextual Bandit 3#####
# simulates contextual bandits problem strategy 3
# This is the all-in method, at each step we put all our assets into one cryptocrreuncy, effectively 
# making this a contextual k-armed bandits problem where we pull one arm and one arm only
# params
# n_curren: number of currencies in proble (including cash)
# n_steps: number of timesteps in the episode
# alpha: learning rate of the action preference functions
# window_size: size of rolling window of asset prices to consider in the action preferences
# discount: discount factor of previous price changes
# epsilon: epsilon value for epsilon greedy
simulate_contextual3 <- function(n_curren,n_steps,alpha,window_size,discount,epsilon=0.5,theseed=100,data){
  set.seed(theseed)
  Ht <- rep(0,5) # initialize preference vector
  
  # Initialize weight vector for moving average window
  weight_vec <- c()
  for(k in 0:(window_size-1))
    weight_vec <- c(weight_vec,discount^k)
  weight_vec <- rev(weight_vec)
  
  price <- head(data,window_size+1) # initializes the price dataframe as the first 2 price vectors
  #st <- getPriceRelativeVec(price[1,],price[2,]) # get first price relative vector (first state)
  
  # history vector of price changes
  history <- getPriceRelativeVec(price[1,],price[2,])
  for(h in 2:window_size){
    history <- rbind(history,getPriceRelativeVec(price[h,],price[h+1,]))
  }
  
  # Define state st as vector of discounted previous relative changes
  st <- weight_vec %*% history
  
  prev_v <- tail(price,1) # initializes first price vector v
  
  # Define reward vecs and random action reward vectors
  Rvec <- c()
  Rvec_random <- c()
  Rvec_market <- c()
  market_average <- rep(1/5,5)
  
  # initialize matrices
  piMat <- matrix(c(1,0,0,0,0),nrow=1,byrow=T)
  hMat <- matrix(Ht,nrow=1,byrow=T)
  yMat <- matrix(rep(0,n_curren),nrow=1,byrow=T)
  aMat <- matrix(c(1,0,0,0,0),nrow=1,byrow=T)
  
  for(i in window_size:n_steps){
    # get price dataframe of current time step
    price <- head(data,i+2)
    # get the current v
    curr_v <- tail(price,1)
    # get price change
    yt <- getPriceRelativeVec(prev_v,curr_v)
    
    # update history matrix
    history <- rbind(tail(history,window_size-1),yt)
    
    # preference vector for state s (element-wise multiplication of Ht and st)
    Ht_s <- st * Ht
    
    # Compute pivec (softmaxes for each currency)
    piVec <- c()
    for(a in 1:n_curren)
      piVec <- c(piVec,get_softmax(a,Ht_s))
    
    if(runif(1) < epsilon)
      # do random action
      action <- sample(seq(1,n_curren),1)
    else
      # get the prefered action
      action <- which.max(piVec)
    
    # define action vec (1 if action is argmax of softmax)
    action_vec <- rep(0,n_curren)
    action_vec[action] <- 1
    
    # get the log returns for our action (in this case our action is the argmax over a)
    rt <- exp(getLogReturns(yt,action_vec))
    Rvec <- c(Rvec,rt)
    
    # Reward for random action
    Rvec_random <- c(Rvec_random,exp(getLogReturns(yt,random_action(4))))
    
    # Reward for market average
    Rvec_market <- c(Rvec_market,exp(getLogReturns(yt,market_average)))
    
    # Update preference vector
    Ht <- get_update(rt,Rvec,Ht,action,alpha)

    prev_v <- curr_v
    st <- weight_vec %*% history
    
    # append to matrices
    piMat <- rbind(piMat,piVec)
    hMat <- rbind(hMat,Ht)
    yMat <- rbind(yMat,yt)
    aMat <- rbind(aMat,action_vec)
    
  }
  
  # create returrn list
  return_list <- list(prod(Rvec),prod(Rvec_market),piMat,hMat,yMat,cumprod(Rvec),cumprod(Rvec_market),aMat)
  names(return_list) <- c("AgentRt","mktAve","softmax","prefs","changes","AgentVec","mktVec","actions")
  
  return(return_list)
  
}
####POLICY GRADIENT METHOD######
GradPolControl=function(init_weights, length_Epis, alphasteptheta, startingpointepisode=0){ 
  
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

