## Libraries-------------------------------------------------------
library("reshape2")
library("akima")
library("tidyverse")
library("plotly")
library("viridis")
library("shiny")
library("shinyjs")
library("shinythemes")
library("triangle")
library("splines")

## Auxiliary variables-------------------------------------------------------
minAuctionIterations <- 1
maxAuctionIterations <- 1000000

auctionType <- c("First-priced sealed bid auction",
                 "Second-priced sealed bid auction")

priorDistributions <- c("Uniform",
                        "Log-normal",
                        "Triangle")

LLGPayment <- c("Proxy Rule (Nearest-Zero Rule)",
                "Nearest-VCG Rule",
                "Nearest-Bid Rule")

strategyType <- c("Equilibrium",
                  "Truthful",
                  "Function")

## User interface-------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("AuctionVisualizer"),
  h4("This app provides the framework for visualising the ex-interim utility of certain players in a vast range of theoretical environments. 
      The current selection offers the symmetric, asymmetric, LLG and k-double auction settings. For more information about 
      the user input options of this tool, please click the 'About this Page' button. After the execution of the auctions, 
      the app will provide a 2D and a 3D graph of the ex-interim utility for a number of players, depending on the type of auction. 
      The titles of the graphs will guide the user towards his graph of interest."),
  br(),
  
  useShinyjs(),
  # Symmetric auction panel
  tabsetPanel(
    id = "auction_tabs",
    tabPanel("Symmetric auction",
             sidebarLayout(
               sidebarPanel(
                 actionButton("symmetricAbout", "About this Page"),
                 hr(),
                 
                 selectInput("symmetricAuctionType", "Auction type", auctionType),
                 
                 selectInput("symmetricPriors", "Prior distribution", priorDistributions),
                 
                 selectInput("strategyType", "Strategy type", strategyType),
                 conditionalPanel(
                   condition = "input.strategyType == 'Function'",
                   plotlyOutput("player_plot_strategy")
                 ),
                  
                 sliderInput("step", "Valuations and Bids step", value = 1/20, 
                             min = 1/100, max = 1/10),
                 
                 numericInput("symmetricIterations", "Auction Iterations", value = 50, 
                              min = minAuctionIterations, max = maxAuctionIterations),
                 
                 
                 sliderInput("players", "Number of Players", value = 2, 
                             min = 2, max = 100),
                 
                 actionButton(inputId = "submitButtonSymmetric", label = "Submit"),
                 width = 4),
               
               mainPanel(
                 plotOutput("ggplot", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotly", width = "800px", height = "800px"),
               )
             )
    ),
    # Asymmetric auction panel
    tabPanel("Asymmetric auction",
             sidebarLayout(
               sidebarPanel(
                 actionButton("asymmetricAbout", "About this Page"),
                 hr(),
                 
                 selectInput("asymmetricAuctionType", "Auction type", auctionType),
                 
                 selectInput("asymmetricPriors", "Prior distribution", priorDistributions),
                      
                 # Button for setting the intervals for the players with the values of the intervals from the 
                 # paper https://link.springer.com/article/10.1007/s40505-014-0049-1
                 actionButton(inputId = "multipleEquilibriaDefault", label = "Multiple Equilibria Bounds"),
                 br(), br(),
                 
                 sliderInput("player1_range", "Player 1 valuation range", min = 0, max = 10, value = c(0, 1), step = 0.1),
                   
                 selectInput("player1_strategy", "Second player strategy (Player 2)", strategyType),
                 conditionalPanel(
                   condition = "input.player1_strategy == 'Function'",
                   plotlyOutput("player1_plot_strategy")
                 ),
                 hr(),
                 
                 sliderInput("player2_range", "Player 2 valuation range", min = 0, max = 10, value = c(0, 1), step = 0.1),
                 
                 selectInput("player2_strategy", "Second player strategy (Player 1)", strategyType),
                 conditionalPanel(
                   condition = "input.player2_strategy == 'Function'",
                   plotlyOutput("player2_plot_strategy")
                 ),
                 hr(),
                 
                 sliderInput("steps", "Increment steps for valuations and bids", value = 50, 
                             min = 1, max = 100),
                 
                 numericInput("asymmetricIterations", "Auction Iterations", value = 50, 
                              min = minAuctionIterations, max = maxAuctionIterations),
                 
                 actionButton(inputId = "submitButtonAsymmetric", label = "Submit"),
                 width = 4),
               
               mainPanel(
                 plotOutput("ggplot1", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotly1", width = "800px", height = "800px"),
                 hr(),
                 plotOutput("ggplot2", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotly2", width = "800px", height = "800px"),
               )
             )
    ),
    # Local-local-global auction panel
    tabPanel("LLG auction",
             sidebarLayout(
               sidebarPanel(
                 actionButton("LLGAbout", "About this Page"),
                 hr(),
                 
                 selectInput("LLGPriors", "Prior distribution", priorDistributions),
                 
                 selectInput("PaymentRule", "Payment rule", LLGPayment),
                 # paper https://link.springer.com/article/10.1007%2Fs00182-019-00691-3
                 
                 sliderInput("Gamma", "Gamma (γ)", value = 0, min = 0, max = 1),
                 
                 sliderInput("LLGSteps", "Increment steps for valuations and bids", value = 50, 
                             min = 1, max = 100),
                 numericInput("LLGIterations", "Auction Iterations", value = 50, 
                              min = minAuctionIterations, max = maxAuctionIterations),
                 actionButton(inputId = "submitButtonLLG", label = "Submit"),
                 width = 4),
               
               mainPanel(
                 plotOutput("ggplotL1", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotlyL1", width = "800px", height = "800px"),
                 hr(),
                 plotOutput("ggplotL2", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotlyL2", width = "800px", height = "800px"),
                 hr(),
                 plotOutput("ggplotG", width = "800px", height = "400px"),
                 hr(),
                 plotlyOutput("plotlyG", width = "800px", height = "800px")
               )
             )
    ),
    # Double auction panel
    tabPanel("K-double auction",
             sidebarLayout(
                 sidebarPanel(
                   actionButton("DoubleAbout", "About this Page"),
                   hr(),
                   
                   selectInput("DoublePriors", "Prior distribution", priorDistributions),
                   
                   sliderInput("K", "K (Influence of buyer's and seller's bids)", value = 0, min = 0, max = 1, step = 0.05),
                   
                   sliderInput("DoubleSteps", "Increment steps for valuations and bids", value = 50, 
                               min = 1, max = 100),
                   numericInput("DoubleIterations", "Auction Iterations", value = 50, 
                                min = minAuctionIterations, max = maxAuctionIterations),
                   actionButton(inputId = "submitButtonDouble", label = "Submit"),
                   width = 4),
                 
                 mainPanel(
                   plotOutput("ggplotBuyer", width = "800px", height = "400px"),
                   hr(),
                   plotlyOutput("plotlyBuyer", width = "800px", height = "800px"),
                   hr(),
                   plotOutput("ggplotSeller", width = "800px", height = "400px"),
                   hr(),
                   plotlyOutput("plotlySeller", width = "800px", height = "800px")
                 )
             )
    )
  )
)
## Application logic-------------------------------------------------------
server <- function(input, output, session) {
  ## About section-------------------------------------------------------
  observeEvent(input$symmetricAbout, {
    showModal(modalDialog(
      title = "About this page",
      h4("Auction type:"), 
      "The current selection enables a choice between 2 options: First-priced sealed bid auction and 
      Second-priced sealed bid auction.",
      
      h4("Prior distribution:"), 
      "Even though in some of the analyzed auction types the joint prior probability distribution takes is uniform, 
      further theoretical insights might be discovered when simulating auctions with different types. 
      Therefore, this user-input feature enlarges said option through the selection of one of the following 
      prior distributions: Normal, Log-normal, or Triangle.",
      
      h4("Strategy type:"),
      "The current selection enables a choice between 3 options: Equilibrium, Truthful, Function. In case the user
      chooses the 'Function', a pop-up with a 2D graph will appear. Here the user can model his own function of choice, that will
      be used as a bidding strategy for all participating players, apart from player 1.",
      
      h4("Valuations and Bids step:"),
      "At the beginning of every auction the app will generate the valuation and bid data for the player 1. Both of 
      these take the form of vectors, and are created in the same way. Starting from 0 and going to 1, we will generate a vector, 
      where at each position, the value is computed as: vector[i] = vector[i - 1] + step, with i in {2,...,n}. The value at the first 
      position in the vector is always 0. The valuations for the other players are drawn uniformly in the interval [0, 1].",
      
      h4("Auction Iterations:"),
      "We define an auction setting as a row in the values matrix of player 1. The values matrix of player 1 is generated 
      as evenly spaced points of the valuations and bids vectors (see 'Valuations and Bids step'). For every setting we execute #iterations 
      auctions. After the auctions have been executed, we average the obtained utilities, and save the value as the ex-interim 
      utility for that specific setting.",
      
      h4("Number of Players:"),
      "This feature enables the user to select the number of participating players in the auction.",
      easyClose = T
    ))
  })

  observeEvent(input$asymmetricAbout, {
    showModal(modalDialog(
      title = "About this page",
      h4("Auction type:"), 
      "The current selection enables a choice between 2 options: First-priced sealed bid auction and 
      Second-priced sealed bid auction.",
      
      h4("Prior distribution:"), 
      "Even though in some of the analyzed auction types the joint prior probability distribution takes is uniform, 
      further theoretical insights might be discovered when simulating auctions with different types. 
      Therefore, this user-input feature enlarges said option through the selection of one of the following 
      prior distributions: Normal, Log-normal, or Triangle.",
      
      h4("Multiple Equilibria Bounds:"),
      "Set the valuation interval bounds of player 1 to [0, 5] and of player 2 to [6, 7]. Through these 
      values we can then apply the equilibria from https://link.springer.com/article/10.1007/s40505-014-0049-1 .",
      
      h4("Player 1 valuation range:"),
      "Through this feature the user can model the desired valuation range for player 1.",
      
      h4("Second player strategy:"),
      "The current selection enables a choice between 3 options: Equilibrium, Truthful, Function or 
      Multiple Equilibrium 2, Multiple Equilibrium 3, Truthful, Function through the selection of the multiple equilibria option. 
      In case the user chooses the 'Function', a pop-up with a 2D graph will appear. Here the user can model his own function 
      of choice, that will be used as a bidding strategy for player 2.",
      
      h4("Player 2 valuation range:"),
      "Through this feature the user can model the desired valuation range for player 2.",
      
      h4("Second player strategy:"),
      "The current selection enables a choice between 3 options: Equilibrium, Truthful, Function or 
      Multiple Equilibrium 2, Multiple Equilibrium 3, Truthful, Function through the selection of the multiple equilibria option. 
      In case the user chooses the 'Function', a pop-up with a 2D graph will appear. Here the user can model his own function 
      of choice, that will be used as a bidding strategy for player 1.",
      
      h4("Increment steps for valuations and bids:"),
      "At the beginning of every auction the app will generate the valuation and bid data for players 1 and 2. Both of 
      these take the form of vectors, and are created in the same way. Starting from the lower bound of the valuation range and going 
      to the upper one, we will generate a vector using a step value. The step function is equivalent to: 
      step = (upper(valuation) - lower(valuation)) / #steps. At each position in the vector, the value is computed as: 
      vector[i] = vector[i - 1] + step, with i in {2,...,n}. The value at the first position in the vector is always the lower bound of 
      the the valuation range of the respective player. When computing the ex-interim utility of player 1, we will uniformly draw the 
      valuations of player 2 from his own valuation range (given by the user), and vice-versa.",
      
      h4("Auction Iterations:"),
      "We define an auction setting as a row in the values matrix of player 1 or player 2. The values matrix of player 1 and 2 are generated 
      independently as evenly spaced points of the valuations and bids vectors (see 'Increment steps for valuations and bids'). For every setting we 
      execute #iterations auctions. After the auctions have been executed, we average the obtained utilities, and save the value as 
      the ex-interim utility for that specific setting.",
      easyClose = T
    ))
  })
  
  observeEvent(input$LLGAbout, {
    showModal(modalDialog(
      title = "About this page",
      
      h4("Prior distribution:"), 
      "Even though in some of the analyzed auction types the joint prior probability distribution takes is uniform, 
      further theoretical insights might be discovered when simulating auctions with different types. 
      Therefore, this user-input feature enlarges said option through the selection of one of the following 
      prior distributions: Normal, Log-normal, or Triangle.",
      
      h4("Payment rule:"), 
      "This setting offers a selection of different types of payment rules: Proxy Rule (Nearest-Zero Rule),Nearest-VCG Rule, and Nearest-Bid Rule. 
      In case the local players win the auction, these rules will be used to distribute the winnings to them, as per their theoretical framework.",
      
      h4("Gamma (γ):"),
      "This input represents the strength of correlation between the local players. In our current set up, we only visualsie the 'Bernoulli weights
      model', and, thus, the parameter actually represents the probability that both local bidders have exactly the same value",
      
      h4("Increment steps for valuations and bids:"),
      "At the beginning of every auction the app will generate the valuation and bid data for the local player 1, 2 and the global player. All 3 of 
      these take the form of vectors, and are created in the same way. Starting from the lower bound of the valuation range and going 
      to the upper one, we will generate a vector using a step value. In this setting we let the valuation range of the local players be
      [0, 1] and of the global player [0, 2]. The step function is equivalent to: step = (upper(valuation) - lower(valuation)) / #steps. 
      At each position in the vector, the value is computed as: vector[i] = vector[i - 1] + step, with i in {2,...,n}. The value at the first 
      position in the vector is always the lower bound of the the valuation range of the respective player. 
      When computing the ex-interim utility of local player 1, we will compute the valuations of local player 2 as per the formula: 
      v = w*s + (1-w)*s, and vice versa. When computing the ex-interim utilities of the local players, the valuations for the global player
      will be drawn uniformly from his valuation range interval. When computing the ex-interim utility of the global player, the valuations for the 
      local players will be computed with the formula above, taking into account the necessary steps. You can find more details in the paper: 
      https://link.springer.com/article/10.1007%2Fs00182-019-00691-3",
      
      h4("Auction Iterations:"),
      "We define an auction setting as a row in the values matrix of local player 1, 2 or global player. The values matrix of the players are generated 
      independently as evenly spaced points of the valuations and bids vectors (see 'Increment steps for valuations and bids'). For every setting we 
      execute #iterations auctions. After the auctions have been executed, we average the obtained utilities, and save the value as 
      the ex-interim utility for that specific setting.",
      
      easyClose = T
    ))
  })
  
  observeEvent(input$DoubleAbout, {
    showModal(modalDialog(
      title = "About this page",
      
      h4("Prior distribution:"), 
      "Even though in some of the analyzed auction types the joint prior probability distribution takes is uniform, 
      further theoretical insights might be discovered when simulating auctions with different types. 
      Therefore, this user-input feature enlarges said option through the selection of one of the following 
      prior distributions: Normal, Log-normal, or Triangle.",
      
      h4("K (Influence of buyer's and seller's bids):"), 
      "This attribute takes the form of a slider which affects the overall trading price, more specifically, how it is 
      influenced by the buyers’ bids and sellers’ bids",
      
      h4("Increment steps for valuations and bids:"),
      "At the beginning of every auction the app will generate the valuation and bid data for the buyer and seller. Both of 
      these take the form of vectors, and are created in the same way. Starting from the lower bound of the valuation range and going 
      to the upper one, we will generate a vector using a step value. In this setting we let the valuation range of the buyer and seller be
      [0, 1]. The step function is equivalent to: step = (upper(valuation) - lower(valuation)) / #steps. 
      At each position in the vector, the value is computed as: vector[i] = vector[i - 1] + step, with i in {2,...,n}. The value at the first 
      position in the vector is always the lower bound of the the valuation range of the respective player. 
      When computing the ex-interim utility landscape of the buyer, we will draw the valuations of the seller from a uniform distribution, 
      and vice versa. The bidding strategy for the opposing player will always be the respective equilibrium. As before, when plotting the utility 
      of the buyer, the seller will play his equilibrium strategy, and the other way around, when inversing the roles.",
      
      h4("Auction Iterations:"),
      "We define an auction setting as a row in the values matrix of the buyer and seller. The values matrix of the players are generated 
      independently as evenly spaced points of the valuations and bids vectors (see 'Increment steps for valuations and bids'). For every setting we 
      execute #iterations auctions. After the auctions have been executed, we average the obtained utilities, and save the value as 
      the ex-interim utility for that specific setting.",
      
      easyClose = T
    ))
  })
  
  
  ## Auction simulations-------------------------------------------------------
  # Symmetric auction
  symmetric_auction_data <- eventReactive(input$submitButtonSymmetric, {
    # Verifies if the number of auction iterations is greater than 50
    validate({
      if(input$symmetricIterations < 50 || input$symmetricIterations %% 1 != 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the number of auction iterations is a whole number larger than 50."
        ))
      }
      if(input$symmetricIterations > maxAuctionIterations){
        showModal(modalDialog(
          title = "Error",
          paste0("Please make sure that the number of auction iterations doesn't exceed the limit of ", maxAuctionIterations)
        ))
      }
      need(input$symmetricIterations >= 50 && input$symmetricIterations <= maxAuctionIterations && input$symmetricIterations %% 1 == 0, "")
    })
    
    player_prediction <- FALSE
    if(input$strategyType == "Function"){
      player_prediction <- TRUE
    }
    
    # Initialize player 1 data
    player_valuations <- assign("player_valuations", seq(0, 1, by = input$step), envir = .GlobalEnv)
    player_bids <- assign("player_bids", seq(0, 1, by = input$step), envir = .GlobalEnv)
    
    # Initialize bidding profile of opposing players
    if(input$strategyType == "Equilibrium"){
      p_other_player_beta <<- bid_fpsb_bne
    } else if(input$player1_strategy == "Truthful"){
      p_other_player_beta <<- bid_truthfully
    } else {
      p_other_player_beta <<- p_model()
    }
    
    # Form evenly spaced data grid from valuations and bids vectors of player 1
    data_grid <- expand.grid(player_valuations, player_bids)
    names(data_grid) <- c("valuation", "bid")
    
    player_ex_interim <- assign("player_ex_interim",
                                matrix(nrow = length(player_valuations) * length(player_bids), ncol = 1), 
                                envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    player_ex_interim <- apply(data_grid, 1, execute_matrix_auction, input$players, input$symmetricIterations, isPrediction = player_prediction,
                               other_player_beta = p_other_player_beta, prior = input$symmetricPriors)
    
    data_grid$utility <- player_ex_interim
    griddf <- create_visualisation_grid(data_grid)
    
    return(list("griddf" = griddf))
  })
  # Asymmetric auction
  asymmetric_auction_data <- eventReactive(input$submitButtonAsymmetric, {
    # Verifies if the valuation range of player 1 is greater than that of player 2
    validate({
      p1_size <- input$player1_range[2] - input$player1_range[1]
      p2_size <- input$player2_range[2] - input$player2_range[1]
      if(p1_size < p2_size){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the valuation range of player 1 is larger than that of player 2."
        ))
      }
      if(p1_size == 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the length of the valuation range of player 1 is larger than 0."
        ))
      }
      if(p2_size == 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the length of the valuation range of player 2 is larger than 0."
        ))
      }
      need(p1_size >= p2_size && p1_size != 0 && p2_size != 0, "")
    })
    
    # Verifies if the user selects "Multiple Equilibrium 1" as a bidding strategy
    # Should output an error if the does, as the equilibrium is computationally complex to evaluate
    validate({
      if(input$player1_strategy == "Multiple Equilibrium 1" || input$player2_strategy == "Multiple Equilibrium 1"){
        showModal(modalDialog(
          title = "Error",
          "Currently the bidding strategy 'Multiple Equilibrium 1' isn't available at the moment. We are sorry for the inconvenience."
        ))
      }
      need(input$player1_strategy != "Multiple Equilibrium 1" && input$player2_strategy != "Multiple Equilibrium 1", "")
    })
    
    # Verifies if the number of auction iterations is greater than 50
    validate({
      if(input$asymmetricIterations < 50 || input$asymmetricIterations %% 1 != 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the number of auction iterations is a whole number larger than 50."
        ))
      }
      if(input$asymmetricIterations > maxAuctionIterations){
        showModal(modalDialog(
          title = "Error",
          paste0("Please make sure that the number of auction iterations doesn't exceed the limit of ", maxAuctionIterations)
        ))
      }
      need(input$asymmetricIterations >= 50 && input$asymmetricIterations <= maxAuctionIterations  && input$asymmetricIterations %% 1 == 0, "")
    })
    
    u_lo <<- c(input$player1_range[1], input$player2_range[1])
    u_hi <<- c(input$player1_range[2], input$player2_range[2])
    
    player1_prediction <- FALSE
    if(input$player1_strategy == "Function"){
      player1_prediction <- TRUE
    }
    
    # Initialize player 1 data
    player1_valuations <- assign("player1_valuations", seq(u_lo[1], u_hi[1], by = (u_hi[1] - u_lo[1])/input$steps), envir = .GlobalEnv)
    player1_bids <- player1_valuations
    
    # Initialize bidding profile of opposing player (player 2)
    if(input$player1_strategy == "Equilibrium"){
      p1_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral
    } else if(input$player1_strategy == "Truthful"){
      p1_other_player_beta <<- bid_truthfully
    } else if(input$player1_strategy == "Function"){
      p1_other_player_beta <<- p1_model()
    } else if(input$player1_strategy == "Multiple Equilibrium 1"){
      p1_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_1
    } else if(input$player1_strategy == "Multiple Equilibrium 2"){
      p1_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_2
    } else if(input$player1_strategy == "Multiple Equilibrium 3"){
      p1_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_3
    }
    
    # Form evenly spaced data grid from valuations and bids vectors of player 1
    p1_data_grid <- expand.grid(player1_valuations, player1_bids)
    names(p1_data_grid) <- c("valuation", "bid")
    
    
    player1_ex_interim <- assign("player1_ex_interim",
                                 matrix(nrow = length(player1_valuations) * length(player1_bids), ncol = 1), 
                                 envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # player1_turn == 1 -> We plot the ex-interim utility of player 1
    player1_ex_interim <- apply(p1_data_grid, 1, execute_matrix_auction, 2, input$asymmetricIterations, player1_turn = 1, 
                                isPrediction = player1_prediction, u_lo, u_hi, other_player_beta = p1_other_player_beta, 
                                prior = input$asymmetricPriors)
    
    p1_data_grid$utility <- player1_ex_interim
    p1_griddf <- create_visualisation_grid(p1_data_grid)
    
    player2_prediction <- FALSE
    if(input$player2_strategy == "Function"){
      player2_prediction <- TRUE
    }
    
    # Initialize player 2 data
    player2_valuations <- assign("player2_valuations", seq(u_lo[2], u_hi[2], by = (u_hi[2] - u_lo[2])/input$steps), envir = .GlobalEnv)
    player2_bids <- player2_valuations
    
    # Initialize bidding profile of opposing player (player 1)
    if(input$player2_strategy == "Equilibrium"){
      p2_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral
    } else if(input$player2_strategy == "Truthful"){
      p2_other_player_beta <<- bid_truthfully
    } else if(input$player2_strategy == "Function"){
      p2_other_player_beta <<- p2_model()
    } else if(input$player2_strategy == "Multiple Equilibrium 1"){
      p2_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_1
    } else if(input$player2_strategy == "Multiple Equilibrium 2"){
      p2_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_2
    } else if(input$player2_strategy == "Multiple Equilibrium 3"){
      p2_other_player_beta <<- optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_3
    }
    
    # Form evenly spaced data grid from valuations and bids vectors of player 2
    p2_data_grid <- expand.grid(player2_valuations, player2_bids)
    names(p2_data_grid) <- c("valuation", "bid")
    
    player2_ex_interim <- assign("player2_ex_interim",
                                 matrix(nrow = length(player2_valuations) * length(player2_bids), ncol = 1), 
                                 envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # player1_turn == 0 -> We plot the ex-interim utility of player 2
    player2_ex_interim <- apply(p2_data_grid, 1, execute_matrix_auction, 2, input$asymmetricIterations, player1_turn = 0, 
                                isPrediction = player2_prediction, u_lo, u_hi, other_player_beta = p2_other_player_beta, 
                                prior = input$asymmetricPriors)
    
    p2_data_grid$utility <- player2_ex_interim
    p2_griddf <- create_visualisation_grid(p2_data_grid)
    
    return(list("p1_griddf" = p1_griddf, 
                "p2_griddf" = p2_griddf))
  })
  # LLG auction
  LLG_auction_data <- eventReactive(input$submitButtonLLG, {
    
    # Verifies if the number of auction iterations is greater than 50
    validate({
      if(input$LLGIterations < 50 || input$LLGIterations %% 1 != 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the number of auction iterations is a whole number larger than 50."
        ))
      }
      if(input$LLGIterations > maxAuctionIterations){
        showModal(modalDialog(
          title = "Error",
          paste0("Please make sure that the number of auction iterations doesn't exceed the limit of ", maxAuctionIterations)
        ))
      }
      need(input$LLGIterations >= 50 && input$LLGIterations <= maxAuctionIterations && input$LLGIterations %% 1 == 0, "")
    })
    
    # Initialize bidding strategy and payment rule
    if(input$PaymentRule == "Proxy Rule (Nearest-Zero Rule)"){
      if(input$Gamma != 1){
        auctionBeta <<- proxy_rule_beta
      } else {
        auctionBeta <<- perfect_proxy_rule_beta
      }
      auctionPaymentRule <<- proxy_rule
    } else if(input$PaymentRule == "Nearest-VCG Rule"){
      if(input$Gamma != 1){
        auctionBeta <<- nearest_vcg_rule_beta
      } else {
        auctionBeta <<- perfect_nearest_vcg_rule_beta
      }
      auctionPaymentRule <<- nearest_vcg_rule
    } else if(input$PaymentRule == "Nearest-Bid Rule"){
      if(input$Gamma != 1){
        auctionBeta <<- nearest_bid_rule_beta
      } else {
        auctionBeta <<- perfect_nearest_bid_rule_beta
      }
      auctionPaymentRule <<- nearest_bid_rule
    } 
    
    # Initialize local player 1 data
    local1_valuations <- assign("local1_valuations", seq(0, 1, by = 1 / input$LLGSteps), envir = .GlobalEnv)
    local1_bids <- local1_valuations
    
    # Form evenly spaced data grid from valuations and bids vectors of local player 1
    l1_data_grid <- expand.grid(local1_valuations, local1_bids)
    names(l1_data_grid) <- c("valuation", "bid")
    
    local1_ex_interim <- assign("local1_ex_interim",
                                matrix(nrow = length(local1_valuations) * length(local1_bids), ncol = 1), 
                                envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # player_number == 1 && global_turn == 0 -> We plot the ex-interim utility of local player 1
    local1_ex_interim <- apply(l1_data_grid, 1, execute_LLG_matrix_auction, input$LLGIterations, player_number = 1, global_turn = 0, 
                               beta = auctionBeta, payment_rule = auctionPaymentRule, input$LLGPriors)
    
    l1_data_grid$utility <- local1_ex_interim
    l1_griddf <- create_visualisation_grid(l1_data_grid)
    
    # Initialize local player 2 data
    local2_valuations <- assign("local2_valuations", seq(0, 1, by = 1 / input$LLGSteps), envir = .GlobalEnv)
    local2_bids <- local2_valuations
    
    # Form evenly spaced data grid from valuations and bids vectors of local player 2
    l2_data_grid <- expand.grid(local2_valuations, local2_bids)
    names(l2_data_grid) <- c("valuation", "bid")
    
    local2_ex_interim <- assign("local2_ex_interim",
                                matrix(nrow = length(local2_valuations) * length(local2_bids), ncol = 1), 
                                envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # player_number == 2 && global_turn == 0 -> We plot the ex-interim utility of local player 2
    local2_ex_interim <- apply(l2_data_grid, 1, execute_LLG_matrix_auction, input$LLGIterations, player_number = 2, global_turn = 0, 
                               beta = auctionBeta, payment_rule = auctionPaymentRule, input$LLGPriors)
    
    l2_data_grid$utility <- local2_ex_interim
    l2_griddf <- create_visualisation_grid(l2_data_grid)
    
    # Initialize global player data
    global_valuations <- assign("global_valuations", seq(0, 2, by = 2 / input$LLGSteps), envir = .GlobalEnv)
    global_bids <- global_valuations
    
    # Form evenly spaced data grid from valuations and bids vectors of global player
    global_data_grid <- expand.grid(global_valuations, global_bids)
    names(global_data_grid) <- c("valuation", "bid")
    
    global_ex_interim <- assign("global_ex_interim",
                                matrix(nrow = length(global_valuations) * length(global_bids), ncol = 1), 
                                envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # player_number == 3 && global_turn == 1 -> We plot the ex-interim utility of global player
    global_ex_interim <- apply(global_data_grid, 1, execute_LLG_matrix_auction, input$LLGIterations, player_number = 3, global_turn = 1, 
                               beta = auctionBeta, payment_rule = auctionPaymentRule, input$LLGPriors)
    
    global_data_grid$utility <- global_ex_interim
    global_griddf <- create_visualisation_grid(global_data_grid)
    
    return(list("l1_griddf" = l1_griddf,
                "l2_griddf" = l2_griddf,
                "global_griddf" = global_griddf))
  })
  # k-Double auction
  double_auction_data <- eventReactive(input$submitButtonDouble, {
    
    # Verifies if the number of auction iterations is greater than 50
    validate({
      if(input$DoubleIterations < 50 || input$DoubleIterations %% 1 != 0){
        showModal(modalDialog(
          title = "Error",
          "Please make sure that the number of auction iterations is a whole number larger than 50."
        ))
      }
      if(input$DoubleIterations > maxAuctionIterations){
        showModal(modalDialog(
          title = "Error",
          paste0("Please make sure that the number of auction iterations doesn't exceed the limit of ", maxAuctionIterations)
        ))
      }
      need(input$DoubleIterations >= 50 && input$DoubleIterations <= maxAuctionIterations && input$DoubleIterations %% 1 == 0, "")
    })
    
    # Initialize buyer player data
    buyer_valuations <- assign("buyer_valuations", seq(0, 1, by = 1 / input$DoubleSteps), envir = .GlobalEnv)
    buyer_bids <- buyer_valuations
    
    # Form evenly spaced data grid from valuations and bids vectors of buyer player
    buyer_data_grid <- expand.grid(buyer_valuations, buyer_bids)
    names(buyer_data_grid) <- c("valuation", "bid")
    
    buyer_ex_interim <- assign("buyer_ex_interim",
                               matrix(nrow = length(buyer_valuations) * length(buyer_bids), ncol = 1), 
                               envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # buyer_turn == 1 -> We plot the ex-interim utility of buyer player
    buyer_ex_interim <- apply(buyer_data_grid, 1, execute_double_matrix_auction, input$DoubleIterations, buyer_turn = 1, input$K, 
                              prior = input$DoublePriors)
    
    buyer_data_grid$utility <- buyer_ex_interim
    buyer_griddf <- create_visualisation_grid(buyer_data_grid)
    
    # Initialize seller player data
    seller_valuations <- assign("seller_valuations", seq(0, 1, by = 1 / input$DoubleSteps), envir = .GlobalEnv)
    seller_bids <- seller_valuations
    
    # Form evenly spaced data grid from valuations and bids vectors of seller player
    seller_data_grid <- expand.grid(seller_valuations, seller_bids)
    names(seller_data_grid) <- c("valuation", "bid")
    
    seller_ex_interim <- assign("seller_ex_interim",
                                matrix(nrow = length(seller_valuations) * length(seller_bids), ncol = 1), 
                                envir = .GlobalEnv)
    
    # Compute resulting ex-interim utility for every row of the evenly spaced data grid
    # buyer_turn == 0 -> We plot the ex-interim utility of seller player
    seller_ex_interim <- apply(seller_data_grid, 1, execute_double_matrix_auction, input$DoubleIterations, buyer_turn = 0, input$K, 
                               prior = input$DoublePriors)
    
    seller_data_grid$utility <- seller_ex_interim
    seller_griddf <- create_visualisation_grid(seller_data_grid)
    
    return(list("buyer_griddf" = buyer_griddf,
                "seller_griddf" = seller_griddf))
  })
  
  
  ## Auction logic of simulation-------------------------------------------------------
  # Symmetric and asymmetric logic
  execute_matrix_auction <- function(player_data, n_players, iterations, player1_turn, isPrediction, other_player_beta, prior, ...){
    
    if(input$auction_tabs == "Symmetric auction"){
      # Assign auction type - Symmetric auction
      if(input$symmetricAuctionType == "First-priced sealed bid auction"){
        auction = fpsb_auction
      } else {
        auction = spsb_auction
      }
      beta = other_player_beta
      
    } else {
      # Assign auction type - Asymmetric auction
      if(input$asymmetricAuctionType == "First-priced sealed bid auction"){
        auction = fpsb_auction
      } else {
        auction = spsb_auction
      }
      beta = other_player_beta
    }
    
    if(input$auction_tabs == "Symmetric auction"){
      # Assign the values to the global variables
      if(prior == "Uniform"){
        valuation_matrix <- matrix(draw_uniform_valuations(n_players, iterations), iterations, n_players)
      } else if(prior == "Log-normal"){
        valuation_matrix <- matrix(draw_log_normal_valuations(n_players, iterations), iterations, n_players)
      } else {
        valuation_matrix <- matrix(draw_triangle_valuations(n_players, iterations), iterations, n_players)
      }
      # Set valuations for player 1 equal to valuation from player_data
      valuation_matrix[, 1] <- rep(player_data[1], iterations)
      
      # Check if the user input graph function feature is selected as a bid strategy
      if(!isPrediction){
        bids_matrix <- beta(valuation_matrix, n_players)
      } else {
        bids_matrix <- matrix(0, nrow = nrow(valuation_matrix), ncol = ncol(valuation_matrix))
        # For every column in the bid matrix predict a bid vector
        for(i in 1:ncol(valuation_matrix)){
          bid_prediction_vector <- predict(p_model(), valuation_matrix[, i])
          bids_matrix[, i] <- bid_prediction_vector
          bids_matrix[, i] <- pmax(bids_matrix[, i], 0)
        }
      }
      
    } else {
      if(player1_turn == 1){
        # Randomly draw valuations for player 1
        if(prior == "Uniform"){
          valuation_matrix <- matrix(draw_uniform_valuations(n_players = 2, iterations, lo = u_lo[1], hi = u_hi[1]), iterations, 2)
        } else if(prior == "Log-normal"){
          valuation_matrix <- matrix(draw_log_normal_valuations(n_players = 2, iterations, lo = u_lo[1], hi = u_hi[1]), iterations, 2)
        } else {
          valuation_matrix <- matrix(draw_triangle_valuations(n_players = 2, iterations, lo = u_lo[1], hi = u_hi[1]), iterations, 2)
        }
      } else {
        # Randomly draw valuations for player 2
        if(prior == "Uniform"){
          valuation_matrix <- matrix(draw_uniform_valuations(n_players = 2, iterations, lo = u_lo[2], hi = u_hi[2]), iterations, 2)
        } else if(prior == "Log-normal"){
          valuation_matrix <- matrix(draw_log_normal_valuations(n_players = 2, iterations, lo = u_lo[2], hi = u_hi[2]), iterations, 2)
        } else {
          valuation_matrix <- matrix(draw_triangle_valuations(n_players = 2, iterations, lo = u_lo[2], hi = u_hi[2]), iterations, 2)
        }
      }
      # Set valuations for player 1 equal to valuation from player_data
      valuation_matrix[, 1] <- rep(player_data[1], iterations)
      
      # Check if the user input graph function feature is selected as a bid strategy
      if(!isPrediction){
        # player_position = 1 - player1_turn, as we are interested in the weaker/ stronger player
        bids_matrix <- beta(valuation_matrix, player_position = (1 - player1_turn), u_lo, u_hi)
      } else {
        bids_matrix <- matrix(0, nrow = nrow(valuation_matrix), ncol = ncol(valuation_matrix))
        # For every column in the bid matrix predict a bid vector
        if(player1_turn == 1){
          for(i in 1:ncol(valuation_matrix)){
            bid_prediction_vector <- predict(p1_model(), valuation_matrix[, i])
            bids_matrix[, i] <- bid_prediction_vector
            bids_matrix[, i] <- pmax(bids_matrix[, i], 0)
          }
        } else {
          for(i in 1:ncol(valuation_matrix)){
            bid_prediction_vector <- predict(p2_model(), valuation_matrix[, i])
            bids_matrix[, i] <- bid_prediction_vector
            bids_matrix[, i] <- pmax(bids_matrix[, i], 0)
          }
        }
      }
    }
    
    # Set bids for player 1 equal to bid from player_data, regardless of the turns
    bids_matrix[, 1] <- rep(player_data[2], iterations)
    
    utility_matrix <- auction(valuation_matrix, bids_matrix)
    # Save utilities from the auction iterations in the global variable
    return(mean(utility_matrix[, 1]))
  }
  
  # LLG logic
  execute_LLG_matrix_auction <- function(player_data, iterations, player_number, global_turn, beta, payment_rule, prior){
    
    n_players <- 3
    # Randomly draw the valuations for the players
    if(prior == "Uniform"){
      valuation_matrix <- matrix(draw_uniform_valuations(n_players, iterations, lo = 0, hi = 2), iterations, n_players)
    } else if(prior == "Log-normal"){
      valuation_matrix <- matrix(draw_log_normal_valuations(n_players, iterations, lo = 0, hi = 2), iterations, n_players)
    } else {
      valuation_matrix <- matrix(draw_triangle_valuations(n_players, iterations, lo = 0, hi = 2), iterations, n_players)
    }
    
    playerV <- player_data[1]
    # Set valuations for local/ global player(s) equal to valuation from player_data
    valuation_matrix[, player_number] <- rep(playerV, iterations)
    # Generate the valuation for the other local player for every row of the valuation matrix through the binomial distribution
    if(global_turn == 0){
      for(i in 1:nrow(valuation_matrix)){
        # v = w * s + (1 - w) * z
        s <- playerV
        z <- runif(1)
        w <- rbinom(n = 1, size = 1, prob = input$Gamma)
        v <- w * s + (1 - w) * z
        
        valuation_matrix[i, 3 - player_number] <- v
      }
    # Generate the valuation for both local players for every row of the valuation matrix
    } else {
      for(i in 1:nrow(valuation_matrix)){
        # v = w * s + (1 - w) * z
        s <- runif(1)
        z1 <- runif(1)
        z2 <- runif(1)
        w <- rbinom(n = 1, size = 1, prob = input$Gamma)
        
        v1 <- w * s + (1 - w) * z1
        v2 <- w * s + (1 - w) * z2
        
        valuation_matrix[i, 1] <- v1
        valuation_matrix[i, 2] <- v2
      }
    }
    
    playerB <- player_data[2]
    # Create an empty valuation matrix for the bids of the participating players
    bids_matrix <- matrix(NA, iterations, n_players)
    # Set bids for local/ global player(s) equal to bid from player_data
    bids_matrix[, player_number] <- rep(playerB, iterations)
    # Generate b for every row of the bids matrix based on the valuations matrix and the current player
    # Initialize the payment function according to the selected payment rule by the user
    if(global_turn == 0){
      bids_matrix[, 3 - player_number] <- sapply(valuation_matrix[, 3 - player_number], beta, gamma = input$Gamma)
    } else {
      bids_matrix[, 1] <- sapply(valuation_matrix[, 1], beta, gamma = input$Gamma)
      bids_matrix[, 2] <- sapply(valuation_matrix[, 2], beta, gamma = input$Gamma)
    }
    # Assign bid of global player as truthful (if it isn't the turn of the global player)
    if(global_turn == 0){
      bids_matrix[, 3] <- valuation_matrix[, 3]
    }
    
    if(global_turn == 0){
      # Establish auctions in which local players won
      wins_vector <- apply(bids_matrix, 1, get_local_winners)
      payment_matrix <- matrix(NA, iterations, n_players - 1)
      utility_vector <- rep(NA, iterations)
      for(i in 1:length(utility_vector)){
        # If locals won the auction, they get the payment, if not they get 0
        b1 <- bids_matrix[i, 1]
        b2 <- bids_matrix[i, 2]
        B <- bids_matrix[i, 3]
        payment_matrix[i, ] <- payment_rule(b1, b2, B) * wins_vector[i]
        # If the player didn't win the auction, his utility is 0. Here we check for that
        if(wins_vector[i] == 0){
          utility_vector[i] <- 0
        } else {
          utility_vector[i] <- (playerV - payment_matrix[i, player_number])
        }
      }
      return(mean(utility_vector))
      
    } else {
      # Establish auctions in which the global player won
      wins_vector <- apply(bids_matrix, 1, get_global_winners)
      payment_vector <- rep(NA, iterations)
      utility_vector <- rep(NA, iterations)
      for(i in 1:length(wins_vector)){
        # If global won the auction, he gets the sum the of the bids of the 2 local players, if not he gets 0
        b1 <- bids_matrix[i, 1]
        b2 <- bids_matrix[i, 2]
        payment_vector[i] <- (b1 + b2) * wins_vector[i]
        # If the player didn't win the auction, his utility is 0. Here we check for that
        if(wins_vector[i] == 0){
          utility_vector[i] <- 0
        } else {
          utility_vector[i] <- playerV - payment_vector[i]
        }
      }
      return(mean(utility_vector))
    }
  }
  
  # k-Double logic
  execute_double_matrix_auction <- function(player_data, iterations, buyer_turn, k, prior){
    
    # Assign the values to the global variables
    if(prior == "Uniform"){
      valuation_matrix <- matrix(draw_uniform_valuations(n_players = 2, iterations), iterations, 2)
    } else if(prior == "Log-normal"){
      valuation_matrix <- matrix(draw_log_normal_valuations(n_players = 2, iterations), iterations, 2)
    } else {
      valuation_matrix <- matrix(draw_triangle_valuations(n_players = 2, iterations), iterations, 2)
    }
    
    # Set valuations for buyer/seller equal to valuation from player_data
    valuation_matrix[, 2 - buyer_turn] <- rep(player_data[1], iterations)
    
    # Initialize bid function
    if(buyer_turn == 1) {
      beta <- double_auction_seller_general_equilibrium 
    } else {
      beta <- double_auction_buyer_general_equilibrium 
    }
    
    # Create bids matrix
    bids_matrix <- beta(valuation_matrix, k)
    bids_matrix[, 2 - buyer_turn] <- rep(player_data[2], iterations)
    
    # Compute resulting trade_prices. If the trade price is 0, then the exchange doesn't take place
    trade_prices <- apply(bids_matrix, 1, compute_general_trade_price, k)
    
    # Compute resulting utilities
    utility_matrix <- matrix(NA, iterations, 2)
    for(i in 1:length(trade_prices)){
      utility_matrix[i, 1] <- get_buyer_utility(valuation_matrix[i, 1], trade_prices[i])
      utility_matrix[i, 2] <- get_seller_utility(valuation_matrix[i, 2], trade_prices[i])
    }
    
    return(mean(utility_matrix[, 2 - buyer_turn]))
  }
  
  ## Graph generations-------------------------------------------------------
  # Symmetric graphs
  output$ggplot <- renderPlot({
    max_util_data <- symmetric_auction_data()$griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = symmetric_auction_data()$griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.01, xmax=valuation + 0.01, ymin=bid - 0.01, ymax=bid + 0.01)) + 
      geom_function(fun = bid_fpsb_bne, 
                    args = list(n_players = input$players), 
                    colour = "blue") + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Player 1 ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotly <- renderPlotly({
    z <- symmetric_auction_data()$griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~symmetric_auction_data()$griddf$valuation[1:40],
            y = ~symmetric_auction_data()$griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Player 1 ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  
  # Asymmetric graphs
  output$ggplot1 <- renderPlot({
    max_util_data <- asymmetric_auction_data()$p1_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    p1_valuation_length <- u_hi[1] - u_lo[1]
    p1_rect_size <- p1_valuation_length / 100
    
    ggplot(data = asymmetric_auction_data()$p1_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - p1_rect_size, xmax=valuation + p1_rect_size, ymin=bid - p1_rect_size, ymax=bid + p1_rect_size)) + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Player 1 ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotly1 <- renderPlotly({
    z <- asymmetric_auction_data()$p1_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~asymmetric_auction_data()$p1_griddf$valuation[1:40],
            y = ~asymmetric_auction_data()$p1_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Player 1 ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  output$ggplot2 <- renderPlot({
    max_util_data <- asymmetric_auction_data()$p2_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    p2_valuation_length <- u_hi[2] - u_lo[2]
    p2_rect_size <- p2_valuation_length / 100
    
    ggplot(data = asymmetric_auction_data()$p2_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - p2_rect_size, xmax=valuation + p2_rect_size, ymin=bid - p2_rect_size, ymax=bid + p2_rect_size)) + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Player 2 ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotly2 <- renderPlotly({
    z <- asymmetric_auction_data()$p2_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~asymmetric_auction_data()$p2_griddf$valuation[1:40],
            y = ~asymmetric_auction_data()$p2_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Player 2 ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  
  # LLG graphs
  output$ggplotL1 <- renderPlot({
    max_util_data <- LLG_auction_data()$l1_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = LLG_auction_data()$l1_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.01, xmax=valuation + 0.01, ymin=bid - 0.01, ymax=bid + 0.01)) +
      scale_fill_viridis_c(option = "A") +
      labs(title = "Local player 1 ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotlyL1 <- renderPlotly({
    z <- LLG_auction_data()$l1_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~LLG_auction_data()$l1_griddf$valuation[1:40],
            y = ~LLG_auction_data()$l1_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Local player 1 ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  output$ggplotL2 <- renderPlot({
    max_util_data <- LLG_auction_data()$l2_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = LLG_auction_data()$l2_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.01, xmax=valuation + 0.01, ymin=bid - 0.01, ymax=bid + 0.01)) + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Local player 2 ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotlyL2 <- renderPlotly({
    z <- LLG_auction_data()$l2_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~LLG_auction_data()$l2_griddf$valuation[1:40],
            y = ~LLG_auction_data()$l2_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Local player 2 ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  output$ggplotG <- renderPlot({
    max_util_data <- LLG_auction_data()$global_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = LLG_auction_data()$global_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.02, xmax=valuation + 0.02, ymin=bid - 0.02, ymax=bid + 0.02)) + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Global player ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotlyG <- renderPlotly({
    z <- LLG_auction_data()$global_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~LLG_auction_data()$global_griddf$valuation[1:40],
            y = ~LLG_auction_data()$global_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Global player ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  
  # K-double graphs
  output$ggplotBuyer <- renderPlot({
    max_util_data <- double_auction_data()$buyer_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = double_auction_data()$buyer_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.01, xmax=valuation + 0.01, ymin=bid - 0.01, ymax=bid + 0.01)) + 
      geom_function(fun = double_auction_buyer_general_equilibrium, 
                    args = list(k = input$K), 
                    colour = "blue") + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Buyer ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotlyBuyer <- renderPlotly({
    z <- double_auction_data()$buyer_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~double_auction_data()$buyer_griddf$valuation[1:40],
            y = ~double_auction_data()$buyer_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Buyer ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  output$ggplotSeller <- renderPlot({
    max_util_data <- double_auction_data()$seller_griddf %>%
      group_by(valuation) %>%
      arrange(desc(utility)) %>%
      slice(1)
    
    ggplot(data = double_auction_data()$seller_griddf,
           aes(x = valuation,
               y = bid,
               z = utility)) + 
      geom_tile(aes(fill = utility)) + 
      geom_rect(data = max_util_data, size = 1, fill = NA, colour = "red", 
                aes(xmin=valuation - 0.01, xmax=valuation + 0.01, ymin=bid - 0.01, ymax=bid + 0.01)) + 
      geom_function(fun = double_auction_seller_general_equilibrium, 
                    args = list(k = input$K), 
                    colour = "blue") + 
      scale_fill_viridis_c(option = "A") +
      labs(title = "Seller ex-interim utility (2D)") +
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 96)
  output$plotlySeller <- renderPlotly({
    z <- double_auction_data()$seller_griddf$utility
    dim(z) = c(40, 40)
    z <- t(z)
    
    plot_ly(type = "surface",
            showscale = FALSE,
            colors = viridis_pal(option = "A")(10),
            x = ~double_auction_data()$seller_griddf$valuation[1:40],
            y = ~double_auction_data()$seller_griddf$bid[seq(0, 1600, by = 40)],
            z = ~z) %>%
      layout(
        title = "Seller ex-interim utility (3D)",
        scene = list(
          xaxis = list(title = "valuation"),
          yaxis = list(title = "bid"),
          zaxis = list(title = "utility"),
          camera=list(
            eye = list(x=-1.6, y=1.6, z=0.4)
          )
        )
      )
  })
  
  
  ## Auxiliary functions-------------------------------------------------------
  # UI asymmetric valuation ranges update button for v1 in [0, 5] and v2 in [6, 7]
  observeEvent(input$multipleEquilibriaDefault, {
    updateSliderInput(session, "player1_range", value = c(0, 5))
    updateSliderInput(session, "player2_range", value = c(6, 7))
  })
  
  # Asymmetric bidding strategies update for v1 in [0, 5] and v2 in [6, 7]
  observe({
    p1_lo <- input$player1_range[1]
    p1_hi <- input$player1_range[2]
    p2_lo <- input$player2_range[1]
    p2_hi <- input$player2_range[2]
    
    if(p1_lo == 0 && p1_hi == 5 && p2_lo == 6 && p2_hi == 7){
      strategyType <- prepend(strategyType, c("Multiple Equilibrium 1", "Multiple Equilibrium 2", "Multiple Equilibrium 3"))
      strategyType <- strategyType[!strategyType %in% c("Equilibrium")]
      updateSelectInput(session, "player1_strategy",
                        choices = strategyType)
      updateSelectInput(session, "player2_strategy",
                        choices = strategyType)
    } else {
      strategyType <- strategyType[!strategyType %in% c("Multiple Equilibrium 1", 
                                                         "Multiple Equilibrium 2", 
                                                         "Multiple Equilibrium 3")]
      strategyType <- prepend(strategyType, c("Equilibrium"))
      updateSelectInput(session, "player1_strategy",
                        choices = strategyType)
      updateSelectInput(session, "player2_strategy",
                        choices = strategyType)
    }
  })
  
  # Computes the utility of the buyer for the k-double auction
  get_buyer_utility <- function(valuation, trade_price){
    if(trade_price != 0){
      return(valuation - trade_price)
    }
    return(0)
  }
  
  # Computes the utility of the seller for the k-double auction
  get_seller_utility <- function(valuation, trade_price){
    if(trade_price != 0){
      return(trade_price - valuation)
    }
    return(0)
  }
  
  # Computes the trade price for the k-double auction
  compute_general_trade_price <- function(bids_row, k){
    if(bids_row[1] >= bids_row[2]){
      return(k * bids_row[1] + (1 - k) * bids_row[2])
    } else return(0)
  }
  
  # Establishes if the local players won for the LLG auction
  get_local_winners <- function(bid_vector){
    if(bid_vector[1] + bid_vector[2] > bid_vector[3]){
      return(1)
    } else return(0)
  }
  
  # Establishes if the global player won for the LLG auction
  get_global_winners <- function(bid_vector){
    if(bid_vector[1] + bid_vector[2] > bid_vector[3]){
      return(0)
    } else return(1)
  }
  
  # Interpolation function for auction data
  create_visualisation_grid <- function(data_grid){
    interp_grid <- akima::interp(data_grid$valuation, data_grid$bid, data_grid$utility)
    data.frame(valuation = rep(interp_grid$x, ncol(interp_grid$z)), 
               bid = rep(interp_grid$y, each = nrow(interp_grid$z)), 
               utility = as.numeric(interp_grid$z))
  }
  
  
  ## UI and logic for user-defined bidding function-------------------------------------------------------
  # User-defined bidding function for symmetric auction
  
  # Idea and logic inspired from: https://stackoverflow.com/questions/47280032/draggable-line-chart-in-r-shiny/54777145#54777145
  player_valuation_graph <- observeEvent(input$players, {
    
    rv_x <- c(seq(0, 1, by = 1/10))
    rv_y <- c(seq(0, 1, by = 1/10))
    
    # Reactive values
    p_graph_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p_truthful_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p_eq_rv <- reactiveValues(
      x = rv_x,
      y = bid_fpsb_bne(rv_x, n_players = input$players)
    )
    
    # Reactive grids
    p_graph_grid <- reactive({
      data.frame(x = seq(min(p_graph_rv$x), max(p_graph_rv$x), length = 100))
    })
    
    p_truthful_grid <- reactive({
      data.frame(x = seq(min(p_truthful_rv$x), max(p_truthful_rv$x), length = 100))
    })
    
    p_eq_grid <- reactive({
      data.frame(x = seq(min(p_eq_rv$x), max(p_eq_rv$x), length = 100))
    })
    
    # Models
    p_model <<- reactive({
      d <- data.frame(x = p_graph_rv$x, y = p_graph_rv$y)
      loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
    
    p_truthful_model <<- reactive({
      d <- data.frame(x = p_truthful_rv$x, y = p_truthful_rv$y)
      lm(y ~ x, d)
    })
    
    p_eq_model <<- reactive({
      d <- data.frame(x = p_eq_rv$x, y = p_eq_rv$y)
      lm(y ~ x, d)
    })
    
    output$player_plot_strategy <- renderPlotly({
      # creates a list of circle shapes from x/y data
      p_circles <- map2(p_graph_rv$x, p_graph_rv$y, 
                         ~list(
                           type = "circle",
                           # anchor circles at (mpg, wt)
                           xanchor = .x,
                           yanchor = .y,
                           # give each circle a 2 pixel diameter
                           x0 = -4, x1 = 4,
                           y0 = -4, y1 = 4,
                           xsizemode = "pixel", 
                           ysizemode = "pixel",
                           # other visual properties
                           fillcolor = "black",
                           line = list(color = "transparent")
                         )
      )
      
      # plot the shapes and fitted line
      plot_ly(source = "p_graph") %>%
        add_lines(x = p_truthful_grid()$x, y = predict(p_truthful_model(), p_truthful_grid()), color = I("blue"), name = "Truthful") %>%
        add_lines(x = p_eq_grid()$x, y = predict(p_eq_model(), p_eq_grid()), color = I("dark green"), name = "Equilibirium") %>%
        add_lines(x = p_graph_grid()$x, y = predict(p_model(), p_graph_grid()), color = I("red"), name = "Function") %>%
        layout(shapes = p_circles) %>%
        config(edits = list(shapePosition = TRUE))
    })
    
    # Update x/y reactive values in response to changes in shape anchors
    observe({
      p_ed <- event_data("plotly_relayout", source = "p_graph")
      p_shape_anchors <- p_ed[grepl("^shapes.*anchor$", names(p_ed))]
      if (length(p_shape_anchors) != 2) return()
      row_index <- unique(readr::parse_number(names(p_shape_anchors)) + 1)
      p_pts <- as.numeric(p_shape_anchors)
      p_graph_rv$x[row_index] <- p_pts[1]
      p_graph_rv$y[row_index] <- p_pts[2]
      # Update model
      d <- data.frame(x = p_graph_rv$x, y = p_graph_rv$y)
      p_model <- loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
  })
  
  # User-defined bidding function for asymmetric auction
  player1_valuation_graph <- observeEvent(input$player2_range, {
    
    rv_x <- c(seq(input$player2_range[1], input$player2_range[2], by = (input$player2_range[2] - input$player2_range[1]) / 10))
    rv_y <- c(seq(input$player2_range[1], input$player2_range[2], by = (input$player2_range[2] - input$player2_range[1]) / 10))
    
    u_lo <- c(input$player1_range[1], input$player2_range[1])
    u_hi <- c(input$player1_range[2], input$player2_range[2])
    
    # Reactive values
    p1_graph_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p1_truthful_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p1_eq_rv <- reactiveValues(
      x = rv_x,
      y = optimal_bid_2P_asymmetric_uniform_risk_neutral(rv_x, player_position = 0, u_lo, u_hi)
    )
    
    # Reactive grids
    p1_graph_grid <- reactive({
      data.frame(x = seq(min(p1_graph_rv$x), max(p1_graph_rv$x), length = 100))
    })
    
    p1_truthful_grid <- reactive({
      data.frame(x = seq(min(p1_truthful_rv$x), max(p1_truthful_rv$x), length = 100))
    })
    
    p1_eq_grid <- reactive({
      data.frame(x = seq(min(p1_eq_rv$x), max(p1_eq_rv$x), length = 100))
    })
    
    # Models
    p1_model <<- reactive({
      d <- data.frame(x = p1_graph_rv$x, y = p1_graph_rv$y)
      loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
    
    p1_truthful_model <<- reactive({
      d <- data.frame(x = p1_truthful_rv$x, y = p1_truthful_rv$y)
      lm(y ~ x, d)
    })
    
    p1_eq_model <<- reactive({
      d <- data.frame(x = p1_eq_rv$x, y = p1_eq_rv$y)
      lm(y ~ x, d)
    })

    output$player1_plot_strategy <- renderPlotly({
      # creates a list of circle shapes from x/y data
      p1_circles <- map2(p1_graph_rv$x, p1_graph_rv$y, 
                      ~list(
                        type = "circle",
                        # anchor circles at (mpg, wt)
                        xanchor = .x,
                        yanchor = .y,
                        # give each circle a 2 pixel diameter
                        x0 = -4, x1 = 4,
                        y0 = -4, y1 = 4,
                        xsizemode = "pixel", 
                        ysizemode = "pixel",
                        # other visual properties
                        fillcolor = "black",
                        line = list(color = "transparent")
                      )
      )
      
      # plot the shapes and fitted line
      plot_ly(source = "p1_graph") %>%
        add_lines(x = p1_truthful_grid()$x, y = predict(p1_truthful_model(), p1_truthful_grid()), color = I("blue"), name = "Truthful") %>%
        add_lines(x = p1_eq_grid()$x, y = predict(p1_eq_model(), p1_eq_grid()), color = I("dark green"), name = "Equilibirum") %>%
        add_lines(x = p1_graph_grid()$x, y = predict(p1_model(), p1_graph_grid()), color = I("red"), name = "Function") %>%
        layout(shapes = p1_circles) %>%
        config(edits = list(shapePosition = TRUE))
    })
    
    # Update x/y reactive values in response to changes in shape anchors
    observe({
      p1_ed <- event_data("plotly_relayout", source = "p1_graph")
      p1_shape_anchors <- p1_ed[grepl("^shapes.*anchor$", names(p1_ed))]
      if (length(p1_shape_anchors) != 2) return()
      row_index <- unique(readr::parse_number(names(p1_shape_anchors)) + 1)
      p1_pts <- as.numeric(p1_shape_anchors)
      p1_graph_rv$x[row_index] <- p1_pts[1]
      p1_graph_rv$y[row_index] <- p1_pts[2]
      # Update model
      d <- data.frame(x = p1_graph_rv$x, y = p1_graph_rv$y)
      p1_model <- loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
  })
  player2_valuation_graph <- observeEvent(input$player1_range, {
    
    rv_x <- c(seq(input$player1_range[1], input$player1_range[2], by = (input$player1_range[2] - input$player1_range[1]) / 10))
    rv_y <- c(seq(input$player1_range[1], input$player1_range[2], by = (input$player1_range[2] - input$player1_range[1]) / 10))
    
    u_lo <- c(input$player1_range[1], input$player2_range[1])
    u_hi <- c(input$player1_range[2], input$player2_range[2])
    
    # Reactive values
    p2_graph_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p2_truthful_rv <- reactiveValues(
      x = rv_x,
      y = rv_y
    )
    
    p2_eq_rv <- reactiveValues(
      x = rv_x,
      y = optimal_bid_2P_asymmetric_uniform_risk_neutral(rv_x, player_position = 1, u_lo, u_hi)
    )
    
    # Reactive grids
    p2_graph_grid <- reactive({
      data.frame(x = seq(min(p2_graph_rv$x), max(p2_graph_rv$x), length = 100))
    })
    
    p2_truthful_grid <- reactive({
      data.frame(x = seq(min(p2_truthful_rv$x), max(p2_truthful_rv$x), length = 100))
    })
    
    p2_eq_grid <- reactive({
      data.frame(x = seq(min(p2_eq_rv$x), max(p2_eq_rv$x), length = 100))
    })
    
    # Models
    p2_model <<- reactive({
      d <- data.frame(x = p2_graph_rv$x, y = p2_graph_rv$y)
      loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
    
    p2_truthful_model <<- reactive({
      d <- data.frame(x = p2_truthful_rv$x, y = p2_truthful_rv$y)
      lm(y ~ x, d)
    })
    
    p2_eq_model <<- reactive({
      d <- data.frame(x = p2_eq_rv$x, y = p2_eq_rv$y)
      lm(y ~ x, d)
    })
    
    output$player2_plot_strategy <- renderPlotly({
      # creates a list of circle shapes from x/y data
      p2_circles <- map2(p2_graph_rv$x, p2_graph_rv$y, 
                      ~list(
                        type = "circle",
                        # anchor circles at (mpg, wt)
                        xanchor = .x,
                        yanchor = .y,
                        # give each circle a 2 pixel diameter
                        x0 = -4, x1 = 4,
                        y0 = -4, y1 = 4,
                        xsizemode = "pixel", 
                        ysizemode = "pixel",
                        # other visual properties
                        fillcolor = "black",
                        line = list(color = "transparent")
                      )
      )
      
      # plot the shapes and fitted line
      plot_ly(source = "p2_graph") %>%
        add_lines(x = p2_truthful_grid()$x, y = predict(p2_truthful_model(), p2_truthful_grid()), color = I("blue"), name = "Truthful") %>%
        add_lines(x = p2_eq_grid()$x, y = predict(p2_eq_model(), p2_eq_grid()), color = I("dark green"), name = "Equilibrium") %>%
        add_lines(x = p2_graph_grid()$x, y = predict(p2_model(), p2_graph_grid()), color = I("red"), name = "Function") %>%
        layout(shapes = p2_circles) %>%
        config(edits = list(shapePosition = TRUE))
    })
    
    # Update x/y reactive values in response to changes in shape anchors
    observe({
      p2_ed <- event_data("plotly_relayout", source = "p2_graph")
      p2_shape_anchors <- p2_ed[grepl("^shapes.*anchor$", names(p2_ed))]
      if (length(p2_shape_anchors) != 2) return()
      row_index <- unique(readr::parse_number(names(p2_shape_anchors)) + 1)
      p2_pts <- as.numeric(p2_shape_anchors)
      p2_graph_rv$x[row_index] <- p2_pts[1]
      p2_graph_rv$y[row_index] <- p2_pts[2]
      d <- data.frame(x = p2_graph_rv$x, y = p2_graph_rv$y)
      p2_model <- loess(y ~ x, d, span = 0.75, control=loess.control(surface="direct"))
    })
  })
  
  ## Draw Valuations (priors for v)--------------------------------------------
  # Functions that draw the different valuation profiles
  draw_uniform_valuations <- function(n_players, iterations, lo=0, hi=1){
    runif(n_players*iterations, lo, hi)
  }
  
  draw_log_normal_valuations <- function(n_players, iterations, lo=0, hi=1){
    valuations = rlnorm(1.8*n_players*iterations, meanlog = 1, sdlog = 0.5)/10 * (hi - lo) + lo
    valuations = valuations[valuations <= hi]
    valuations = valuations[1:n_players*iterations]
  }
  
  draw_triangle_valuations <- function(n_players, iterations, lo=0, hi=1){
    rtriangle(n = n_players*iterations, a = lo, b = hi)
  }
  
  
  ## Bidding Strategies (\beta)------------------------------------------------
  ## input: valuation profile v
  ## output: bid profile b
  # truthful bidding: b = v
  bid_truthfully <- function(v, ...){
    v
  }
  
  # equilibrium in symmetric first price auction
  bid_fpsb_bne <- function(v, n_players){
    (n_players - 1) / n_players * v
  }
  
  # equilbirium in asymmetric sealed bid auction where players have the same lower bound
  optimal_bid_2P_asymmetric_uniform_risk_neutral <- function(valuation, player_position, u_lo, u_hi){
    c <- 1 / (u_hi[1] - u_lo[1]) ^ 2 - 1 / (u_hi[2] - u_lo[1]) ^ 2
    factor <- 2 * player_position - 1  # -1 for 0 (weak player), +1 for 1 (strong player)
    denominator <- 1.0 + sqrt(1 + factor * c * (valuation - u_lo[1]) ^ 2)
    bid <- u_lo[1] + (valuation - u_lo[1]) / denominator
  }
  
  # inverse equilibrium 1 in asymmetric asymmetric sealed bid auction where players have specific valuation bounds (i.e. [0, 5], [6, 7])
  inverse_bid_player_1 <- function(bid){
    return (36 / ((2 * bid - 6) * (1 / 5) * exp(9 / 4 + 6 / (6 - 2 * bid)) + 24 - 4 * bid))
  }
  
  # inverse equilibrium 2 in asymmetric asymmetric sealed bid auction where players have specific valuation bounds (i.e. [0, 5], [6, 7])
  inverse_bid_player_2 <- function(bid){
    return (6 + 36 / ((2 * bid - 6) * 20 * exp(-9 / 4 - 6 / (6 - 2 * bid)) - 4 * bid))
  }
  
  # equilibrium 2 in asymmetric asymmetric sealed bid auction where players have specific valuation bounds (i.e. [0, 5], [6, 7])
  optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_2 <- function(valuation, player_position, ...){
    bid <- matrix(0, nrow = nrow(valuation), ncol = ncol(valuation))
    if (player_position == 0){
      for(i in 1: nrow(valuation)){
        if(valuation[i, 2] > 4){
          bid[i, 2] <- valuation[i, 2] / 2 + 2
        } else {
          bid[i, 2] <- valuation[i, 2] / 4 + 3
        }
      }
    }
    else{
      bid[, 2] <- valuation[, 2] / 2 + 1
    }
    return(bid)
  }
  
  # equilibrium 3 in asymmetric asymmetric sealed bid auction where players have specific valuation bounds (i.e. [0, 5], [6, 7])
  optimal_bid_2P_asymmetric_uniform_risk_neutral_multi_lower_3 <- function(valuation, player_position, ...){
    bid <- matrix(0, nrow = nrow(valuation), ncol = ncol(valuation))
    if (player_position == 0)
      bid[, 2] <- valuation[, 2] / 5 + 4
    else
      bid[, 2] <- rep(5, nrow(valuation))
    return(bid)
  }
  
  # proxy rule equilibrium when gamma != 1
  proxy_rule_beta <- function(v, gamma){
    bid <- 1 + (log(gamma + (1 - gamma) * v)) / (1 - gamma)
    max(0, bid)
  }
  
  # proxy rule equilibrium when gamma == 1
  perfect_proxy_rule_beta <- function(v, ...){
    v
  }
  
  # nearest-vcg rule equilibrium when gamma != 1
  nearest_vcg_rule_beta <- function(v, gamma){
    v_hat <- (3 - sqrt(9 - (1 - gamma) ^ 2)) / (1 - gamma)
    bid <- (2 * (v - v_hat)) / (2 + gamma) 
    max(0, bid)
  }
  
  # nearest-vcg rule equilibrium when gamma == 1
  perfect_nearest_vcg_rule_beta <- function(v, sigma = 1, ...){
    v * sigma / (1 + sigma - 2^(-sigma))
  }
  
  # nearest-bid rule equilibrium when gamma != 1
  nearest_bid_rule_beta <- function(v, gamma){
    (log(2) - log(2 - (1 - gamma) * v)) / (1 - gamma)
  }
  
  # nearest-bid rule equilibrium when gamma == 1
  perfect_nearest_bid_rule_beta <- function(v, sigma = 1, ...){
    v * sigma / (1 + sigma)
  }
  
  # buyer equilibrium
  double_auction_buyer_general_equilibrium <- function(valuation, k){
    return((valuation / (1 + k)) + ((k * (1 - k)) / (2 * (1 + k))))
  }
  
  # seller equilibrium
  double_auction_seller_general_equilibrium <- function(valuation, k){
    return((valuation / (2 - k)) + ((1 - k) / 2))
  }
  
  ## Payment allocation mechansim-------------------------------------------------------
  
  # The function assigns the winner of any given auction.
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
  
  # Payment allocation for roxy rule
  proxy_rule <- function(b1, b2, B){
    if(B >= 0 && B <= 2*b2){
      c(1/2 * B, 1/2 * B)
    } else {
      c(B - b2, b2)
    }
  }
  
  # Payment allocation for nearest VCG rule
  nearest_vcg_rule <- function(b1, b2, B){
    p1V <- max(0, B - b2)
    p2V <- max(0, B - b1)
    delta <- 1/2 * (B - p1V - p2V)
    
    c(p1V + delta, p2V + delta)
  }
  
  # Payment allocation for nearest-bid rule
  nearest_bid_rule <- function(b1, b2, B){
    delta <- 1/2 * (b1 + b2 - B)
    
    if(B >= 0 && B <= b1 - b2){
      c(B, 0)
    } else {
      c(b1 - delta, b2 - delta)
    }
  }
  
  
  ## Auction Mechanism (u)-------------------------------------------------------
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
    secondB <- Rfast::rownth(b, rep(2, nrow(b)), descending = T)
    secondB <- t(secondB)
    
    u <- winners * sweep(v, 1, secondB, '-')
  }
}

shinyApp(ui = ui, server = server)
