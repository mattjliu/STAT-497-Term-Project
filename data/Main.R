source("functions/data.R")
source("functions/process.R")
source("functions/simulations.R")

################ Define dataframes ################
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
###################################################

# simulate random actions
print("Simulate random actions")
simulate_random(10,100)

# simulate contextual 1
print("Simulate contextual version 1")
simulate_contextual1(5,100,0.3,10,0.8)

# simulate contextual 2
print("simulate contextual version 2")
simulate_contextual2(5,100,0.6,10,0.8)