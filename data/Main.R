source("functions/data.R")
source("functions/process.R")
require(neuralnet)

# Define dataframes
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

# simulation function, the agent randomizes the weight vector at each step
# params: n_hist, number of timesteps in the past for price history
# params: n_episodes, number of episodes in the simulation
simulate_random <- function(n_hist,n_episodes){
    prev_v <- tail(head(close,n_hist-1),1) ## initialize first previous price vector
    returns <- 0

    # simulation
    for(i in 0:n_episodes){

        # generates random action (portfolio vector)
        random_action <- function(n_assets){
        x <- runif(n_assets+1)
        return(x/sum(x))
        }

        # get price dataframes based on current time steps
        hi <- head(high,i+n_hist)
        lo <- head(low,i+n_hist)
        price <- head(close,i+n_hist)

        wt <- random_action(4)
        curr_v <- tail(price,1)
        yt <- getPriceRelativeVec(prev_v,curr_v)
        rt <- getLogReturns(yt,wt)

        print(paste0("episode ",i))
        print(cat("wt: ",wt))
        print(cat("yt: ",yt))
        print(cat("rt: ",rt))
        print("=================================")

        prev_v <- curr_v # set previous price as current price
        returns <- update_returns(i+1,returns,yt,wt)
    }

    print(paste("average returns for this simulation: ", returns))
}

simulate_random(10,100)

# Function that gives actions table for n samples and n_curren currencies
get_actions_table <- function(n_samples,n_curren,state_vec){
    action_mat <- matrix(NA,nrow=n_samples,ncol=n_curren+1)
    for(i in 1:n_samples)
        action_mat[i,] <- random_action(n_curren)
    return(action_mat)
}

# Simulation function where the agent randomly samples actions at each step and observes next rewards
simulate_samples <- function(n_samples,n_episodes){
    price <- head(close,2) # initializes the price dataframe as the first 2 price vectors
    st <- getPriceRelativeVec(price[1,],price[2,]) # get first price relative vector (first state)
    prev_v <- tail(price,1) # initializes first price vector v
    training_mat <- matrix(nrow=1,ncol=11) # initialize training matrix for training data
    
    for(i in 0:n_episodes){
        # get price dataframe of current time step
        price <- head(close,i+1)
        
        # get the current v
        curr_v <- tail(price,1)
        
        # get price change
        yt <- getPriceRelativeVec(prev_v,curr_v)
        
        # sample actions
        action_mat <- get_actions_table(n_samples,4)
        
        # get rewards for each sampled action
        reward_vec <- c()
        for(j in 1:n_samples)
            reward_vec <- c(reward_vec,getLogReturns(yt,action_mat[j,]))
        
        # append rewards to matrix
        action_mat <- cbind(action_mat,reward_vec)
        
        # Add state columns to matrix
        for(val in rev(st))
            action_mat <- cbind(rep(val,n_samples),action_mat)
        
        # Concatenate matrix to training data
        training_mat <- rbind(training_mat,action_mat)
        
        print(paste0("episode ",i))
        print(action_mat)
        print(cat("st: ",st))
        print(cat("yt: ",yt))
        print("=================================")
        
        # update price vector
        prev_v <- curr_v
        
        # update state vector
        st <- yt
    }
    return(training_mat[-1,])
}

simulate_samples(10,10)