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
  return(log(yt%*%wt))
### Some more functions
# function that initializes weight vector given the number of assets (excluding cash)
init_weights <- function(n_assets){
  # element in first position (cash) is always initialized to 1
  return(c(1,rep(0,n_assets)))
}
# function that returns price tensor Xt given current vt and history n
# the tensor is returned as a feature vector where the columns of each matrix are listed in order
# i.e. first column of V_lo until last column of V_t
get_Xt <- function(vt,n){
  # initializes all three matrices
  V_lo <- getPriceMatrix(low,vt,n)
  V_hi <- getPriceMatrix(high,vt,n)
  V_t <- getPriceMatrix(close,vt,n)
  return(c(V_lo,V_hi,V_t))
}
# function that updates average returns (performance of policy) given counter and previous average
# this is done so that we don't need to store all returns
update_returns <- function(counter,returns,yt,wt){
  return((counter-1)/(counter)*returns + getLogReturns(yt,wt)/counter)
}
# generates random action (portfolio vector)
# params: number of assets in portfolio
random_action <- function(n_assets){
  x <- runif(n_assets+1)
  return(x/sum(x))
}
get_softmax <- function(a,Ht){
  if(sum(exp(Ht)) == 0) return(0)
  return(exp(Ht[a]) / sum(exp(Ht)))
}
# Does gradient ascent on expected reward and returns updated preference vector
get_update <- function(Rt,Rvec,Ht,a,alpha){
  
  pi_t <- get_softmax(a,Ht)
  
  if(length(Rvec) == 0){
    mean_r_vec <- 0
  } else {
    mean_r_vec <- mean(Rvec)
  }
  for(i in 1:length(Ht)){
    if(i == a){
      Ht[i] <- Ht[i] + alpha * (Rt - mean_r_vec) * (1 - pi_t)
    } else {
      Ht[i] <- Ht[i] - alpha * (Rt - mean_r_vec) * pi_t
    }
  }
  return(Ht)
} 
get_update_all_bandits <- function(Rt,R_all,Ht,alpha){
  
  for(a in 1:length(Ht)){
    pi_t <- get_softmax(a,Ht)
    
    Ht[a] <- Ht[a] + alpha * (Rt[a] - mean(R_all)) * (1 - pi_t)
  }
  return(Ht)
}
get_ytd_return=function(data){
  data=as.matrix(data)
  return(data[nrow(data),]/data[1,])
}
#Get the grid for optimal parameters
get_param_mat=function(n_currency,DF,input.function,alpha,window,epsilon=0){
  
  param=array(0,dim=c(length(alpha),length(window),length(epsilon)))
  for(z in 1:length(epsilon)){
    for(i in 1:length(alpha)){
      for(j in 1:length(window)){
        x=input.function(n_currency,n,alpha[i],window[j],DF,epsilon[z])
        param[i,j,z]=x$AgentRt
      }
    }
  }
  
  return(param)
}


