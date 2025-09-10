\name{EloMC-package}
\alias{EloMC-package}
\docType{package}
\title{Elo Rating System and Monte Carlo Simulations for Tennis Tournaments}
\description{
The \pkg{EloMC} package provides a comprehensive framework for analyzing professional tennis tournaments using Elo ratings and Monte Carlo simulations.
It allows users to compute different Elo-based measures of player strength, simulate tournament outcomes, and evaluate betting strategies by comparing Elo-derived probabilities with bookmaker odds.
}
\details{
The package is designed for tennis analytics enthusiasts, statisticians, and sports economists interested in performance modeling and predictive accuracy.

Key functionalities include:
  \itemize{
    \item \strong{Data handling:} Downloading, cleaning, and merging tennis match data into consistent data frames.
    \item \strong{Rating computation:} Calculating player-specific Elo ratings with different specifications.
    \item \strong{Tournament modeling:} Defining and simulating the structure of ATP/WTA Grand Slam tournaments.
    \item \strong{Betting evaluation:} Comparing Elo-based win probabilities to bookmaker odds, and assessing performance of threshold-based betting strategies.
  }

By combining Monte Carlo simulations with Elo ratings, \pkg{EloMC} enables rigorous comparisons between statistical expectations and market-implied probabilities, with applications in forecasting and sports betting research.
}
\section{Main Functions}{
  \code{\link{merged_tennis_data}}: Downloads and merges raw tennis match data. \cr
  \code{\link{clean_data}}: Cleans and prepares data for Elo modeling. \cr
  \code{\link{compute_elo}}: Computes player Elo ratings across matches. \cr
  \code{\link{define_tournament}}: Defines tournament draws and participants. \cr
  \code{\link{simulate_tournament}}: Runs Monte Carlo simulations of tournaments. \cr
  \code{\link{GS_sim}}: Simulates all Grand Slam tournaments from 2019 onwards. \cr
  \code{\link{Estract_book}}: Extracts bookmaker odds from Excel files. \cr
  \code{\link{Place_bet}}: Places simulated bets based on Elo and odds. \cr
  \code{\link{Betting_function}}: Core engine for evaluating betting strategies. \cr
  \code{\link{Results_betting}}: Aggregates results of simulated betting. \cr
  \code{\link{Results_balance}}: Summarizes balances, ROI, and betting statistics. \cr
  \code{\link{plot_player_elo}}: Visualizes the evolution of a player’s Elo rating over time.
}
\examples{
library(EloMC)

# Step 1: Download and prepare ATP/WTA match data
X <- merged_tennis_data()
X.clean <- clean_data(X)

# Step 2: Compute Elo ratings
X.fit <- compute_elo(X.clean)
Table <- X.fit$results

# Step 3: Define the Australian Open 2024
AO24 <- define_tournament(Table, start_date = "2024-01-14")

# Step 4: Simulate the tournament 100,000 times
round_of_128 <- AO24$sessantaquattresimi_ord
sim_results <- simulate_tournament(round_of_128, sim = 100000)

# Step 5: Simulate all Grand Slams 2019–2025 and perform betting analysis
bet_list <- Results_betting(Table, Excel_name = "Database Tennis.xlsx", n = 100000)

# Step 6: Summarize betting performance
balance <- Results_balance(bet_list)
print(balance)
}
\references{
De Angelis, Luca \& Fontana, Saverio (2024).
\emph{Monte Carlo meets Wimbledon: Elo-based simulations for predicting tennis tournament winners}.
SSRN.
\Sexpr[results=rd]{tools:::Rd_expr_doi("10.2139/ssrn.5027380")}
}
\author{
Saverio Fontana
}
\keyword{package}
