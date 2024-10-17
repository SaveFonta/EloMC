\name{EloMC}
\title{EloMC: Tournament Analysis Using Elo Ratings}
\description{
  The **EloMC** package is designed for analyzing and simulating tournament results using the Elo rating system. It provides functions to assess betting strategies and analyze performance in various tournaments.
}
\details{
  This package depends on the following packages:
  
  - **welo**: Needed for various Elo-related calculations and functions.
  - **Rlab**: Provides the `rbern` function for generating random Bernoulli variables.
  
  Please ensure you have these packages installed before using **EloMC**. You can install them from CRAN with the following commands:
  
  \code{install.packages("welo")}
  
  \code{install.packages("Rlab")}
  
  You can install **EloMC** directly from GitHub using the `remotes` package:
  
  \code{install.packages("remotes")}
  
  \code{remotes::install_github("SaveFonta/EloMC")}
}
\author{Your Name Here}
\license{MIT}
\section{Functions}{
  \itemize{
    \item \code{merged_tennis_data(data1, data2)}: 
      Merges two datasets containing tennis match data into a single cohesive dataset.
      \itemize{
        \item \strong{Parameters}:
        \item `data1`: The first dataset to merge.
        \item `data2`: The second dataset to merge.
        \item \strong{Returns}: A merged dataset containing all relevant tennis match data.
      }
      
    \item \code{clean_data(dataset)}: 
      Cleans the given dataset by removing any unnecessary or problematic entries.
      \itemize{
        \item \strong{Parameters}:
        \item `dataset`: The dataset to be cleaned.
        \item \strong{Returns}: A cleaned dataset, free of NA values and outliers.
      }
      
    \item \code{compute_elo(player_ratings, match_results)}: 
      Computes the updated Elo ratings for players based on their match results.
      \itemize{
        \item \strong{Parameters}:
        \item `player_ratings`: A data frame containing the current Elo ratings of players.
        \item `match_results`: A data frame containing the results of the matches.
        \item \strong{Returns}: A data frame with updated Elo ratings for each player.
      }
      
    \item \code{define_tournament(X, start_date, Serie)}: 
      Defines a tournament structure based on the input data and specified start date.
      \itemize{
        \item \strong{Parameters}:
        \item `X`: The input dataset for defining the tournament.
        \item `start_date`: The starting date of the tournament.
        \item `Serie`: The series/type of the tournament (e.g., "Grand Slam").
        \item \strong{Returns}: A structured tournament object containing matchups and player information.
      }
      
    \item \code{simulate_tournament(tournament_data, n)}: 
      Simulates a tournament based on the defined tournament data and number of simulations.
      \itemize{
        \item \strong{Parameters}:
        \item `tournament_data`: The structured tournament object.
        \item `n`: The number of simulations to run.
        \item \strong{Returns}: A list of results from the simulated tournament.
      }
      
    \item \code{Betting_function(probab, Bookmakers, q_values, r_values, q_max, r_max)}: 
      Evaluates betting strategies based on calculated probabilities and bookmaker odds.
      \itemize{
        \item \strong{Parameters}:
        \item `probab`: The probabilities of outcomes from the simulations.
        \item `Bookmakers`: A dataset of bookmaker odds.
        \item `q_values`: A vector of threshold values for betting.
        \item `r_values`: A vector of odds from bookmakers.
        \item `q_max`: Maximum threshold value for betting.
        \item `r_max`: Maximum odds value.
        \item \strong{Returns}: A list of betting outcomes based on the evaluated strategies.
      }
      
    \item \code{Results_betting(X, n = 1000, tournament = NA, Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3), R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3), Q_max = 1000, R_max = 1000)}: 
      Simulates betting outcomes for various tournaments and returns the results.
      \itemize{
        \item \strong{Parameters}:
        \item `X`: Input data for tournament simulations.
        \item `n`: Number of simulations to run (default is 1000).
        \item `tournament`: Name of the tournament (e.g., "Australian Open").
        \item `Q`: A vector of betting thresholds.
        \item `R`: A vector of odds.
        \item `Q_max`: Maximum threshold value for betting (default is 1000).
        \item `R_max`: Maximum odds value (default is 1000).
        \item \strong{Returns}: A list of betting results for each tournament.
      }
      
    \item \code{Results_balance(x)}: 
      Computes the balance, total number of bets, number of won bets, and ROI from the results provided.
      \itemize{
        \item \strong{Parameters}:
        \item `x`: A list of results containing balance, number of bets, and number of won bets.
        \item \strong{Returns}: A data frame with calculated statistics.
      }
  }
}


