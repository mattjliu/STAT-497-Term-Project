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

# generates random action (portfolio vector)
# params: number of assets in portfolio
random_action <- function(n_assets){
    x <- runif(n_assets+1)
    return(x/sum(x))
}

# Returns the softmax for a given action in preference vector Ht
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

get_update_all_bandits <- function(Rt,Rmat,Ht,alpha){
    
    for(a in 1:length(Ht)){
        mean_r_vec <- mean(Rmat[,a])
        pi_t <- get_softmax(a,Ht)

    Ht[a] <- Ht[a] + alpha * (Rt[a] - mean_r_vec) * (1 - pi_t)
    }
    return(Ht)
}