library("reshape2")
library("akima")
library("tidyverse")
library("plotly")

step <- 1/25
players <- 3
iterations <- 100
auctionType <- "fpsb"

player_valuations <- assign("player_valuations", seq(0, 1, by = step), envir = .GlobalEnv)
player_bids <- assign("player_bids", seq(0, 1, by = step), envir = .GlobalEnv)

data_grid <- expand.grid(player_valuations, player_bids)
names(data_grid) <- c("valuation", "bid")

player_ex_interim <- assign("player_ex_interim", 
                            matrix(nrow = length(player_valuations) * length(player_bids), ncol = 1), 
                            envir = .GlobalEnv)

player_utilities <- assign("player_utilities", matrix(nrow = iterations, ncol = 1), envir = .GlobalEnv)

for (i in 1: (length(player_bids) * length(player_valuations))){
  player_utilities <- execute_matrix_auction(players, iterations, data_grid[i,])
  # Compute the ex-interim, i.e. the mean of all utilities for the given auction setting
  player_ex_interim[i, 1] <- mean(player_utilities)
}

data_grid$utility <- player_ex_interim

interp_grid <- akima::interp(data_grid$valuation, data_grid$bid, data_grid$utility)
griddf <- data.frame(valuation = rep(interp_grid$x, ncol(interp_grid$z)), 
                     bid = rep(interp_grid$y, each = nrow(interp_grid$z)), 
                     utility = as.numeric(interp_grid$z))

# ggplot visualisation
ggplot(data = griddf,
         aes(x = valuation,
             y = bid,
             z = utility)) + 
    geom_contour_filled() + 
    scale_fill_viridis_d(drop = FALSE)

matrix <- as.matrix(griddf)

# plotly

fig <- plot_ly(z = ~xtabs(utility ~ valuation + bid, data = griddf)) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "valuation"),
    yaxis = list(title = "bid"),
    zaxis = list(title = "utility"),
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

# -----------------LLG setting-----------------
local1_valuations <- seq(0, 1, by = 1/50)
local1_bids <- local1_valuations

# Grid matrix for valuations and bids for local player 1
l1_data_grid <- expand.grid(local1_valuations, local1_bids)
names(l1_data_grid) <- c("valuation", "bid")

# Utility matrix for local player 1
local1_utilities <- matrix(nrow = 5, ncol = 1)

# Ex-interim utility matrix for local player 1
local1_ex_interim <- matrix(nrow = length(local1_valuations) * length(local1_bids), ncol = 1)

# v2 = w * s + (1 - w) * z2
s <- l1_data_grid[1, 1]
z2 <- runif(1)
w <- rbinom(n = 1, size = 1, prob = 0)

v2 <- w * s + (1 - w) * z2

print(v2)

# /////////////////Functions/////////////////

execute_matrix_auction <- function(n_players, iterations, player_data){
  if(auctionType == "fpsb"){
    beta = bid_fpsb_bne
    auction = fpsb_auction
  } else {
    beta = bid_truthfully
    auction = spsb_auction
  }
  # Assign the values to the global variables
  valuation_matrix <- matrix(draw_uniform_valuations(n_players, iterations), iterations, n_players)
  # Set valuations for player 1 equal to valuation from player_data
  valuation_matrix[, 1] <- rep(player_data[1,1], iterations)
  
  bids_matrix <- beta(valuation_matrix, n_players)
  # Set bids for player 1 equal to bid from player_data
  bids_matrix[, 1] <- rep(player_data[1,2], iterations)
  
  utility_matrix <- auction(valuation_matrix, bids_matrix)
  # Save utilities from the auction iterations in the global variable
  return(utility_matrix[, 1])
  
  # utility_vector <- as.vector(utility_matrix)
  # utility_vector <- utility_vector[utility_vector != 0]
  
  # mean(unlist(utility_vector))
}

# Draw Valuations (priors for v)--------------------------------------------
# functions that draw valuation profiles

draw_uniform_valuations <- function(n_players, iterations, lo=0, hi=1){
  runif(n_players*iterations, lo, hi)
}


# Bidding Strategies (\beta)------------------------------------------------
## input: valuation profile v
## output: bid profile b


# truthful bidding: b = v
bid_truthfully <- function(v, ...){
  v
}

# equilibrium in first_price auction
bid_fpsb_bne <- function(v, n_players){
  (n_players - 1) / n_players * v
}


# Auction Mechanism (u)-------------------------------------------------------
## input: bid profile b, valuation profile v
## output: valuation profile u

## (Note: we only need v here because we return u directly. we would only 
##        need b to find the allocation and payment)

fpsb_auction <- function(v,b){
  ## make a mask about who is winning
  ## (True/1 if bidder has highest bid, 0/FALSE otherwise)
  
  winners <- apply(b, 1, assign_winner)
  winners <- t(winners)
  
  ## winner gets v-b, all others get 0
  u <- winners * (v-b)
}

spsb_auction <- function(v,b){
  ## make a mask about who is winning
  ## (True/1 if bidder has highest bid, 0/FALSE otherwise)
  
  winners <- apply(b, 1, assign_winner)
  winners <- t(winners)
  
  ## winner gets v-2nd highest b, all others get 0
  unique_bids <- apply(b, 1, unique)
  unique_bids <- t(unique_bids)
  
  secondB <- Rfast::rownth(unique_bids, rep(2, nrow(b)), descending = T)
  secondB <- t(secondB)
  
  u <- winners * sweep(v, 1, secondB, '-')
}

# The function assigns the winner of any given auction
# In case of 2 or more players with the same highest bid for an item,
# the function randomly selects a winner for that item
assign_winner <- function(b){
  highest_bids <- which(b == Rfast::nth(b, 1, descending = T))
  if(length(highest_bids) == 1){
    winner <- b==max(b)
  } else {
    winner_pos <- sample(1:length(highest_bids), 1)
    winner <- rep(0, length(b))
    winner[winner_pos] <- 1
  }
  winner
}