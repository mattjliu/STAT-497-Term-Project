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

# simulates contextual bandits problem
# params
# n_curren: number of currencies in proble (including cash)
# n_steps: number of timesteps in the episode
# alpha: learning rate of the action preference functions
# window_size: size of rolling window of asset prices to consider in the action preferences
# discount: discount factor of previous price changes
simulate_contextual <- function(n_curren,n_steps,alpha,window_size,discount){
    
    Ht <- rep(0,5) # initialize preference vector
    
    # Initialize weight vector for moving average window
    weight_vec <- c()
    for(k in 0:(window_size-1))
        weight_vec <- c(weight_vec,discount^k)
    weight_vec <- rev(weight_vec)
    
    price <- head(close,window_size+1) # initializes the price dataframe as the first 2 price vectors
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
    
    for(i in window_size:n_steps){
        # get price dataframe of current time step
        price <- head(close,i+2)
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
        
        # Update preference vector
        Ht <- get_update(rt,Rvec,Ht,action,alpha)
        
#         print(paste0("episode",i))
#         print(cat("Ht: ", Ht))
#         print(cat("Ht_s: ", Ht_s))
        # print(cat("piVec: ", piVec))
#         print(cat("rt: ", rt))
        # print("==============================")
        
        prev_v <- curr_v
        st <- weight_vec %*% history
            
    }
    return(c(mean(Rvec),mean(Rvec_random)))
    
}