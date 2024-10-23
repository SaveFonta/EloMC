\name{EloMC-package}
\alias{EloMC-package}
\docType{package}
\title{Elo Rating System for Monte Carlo simulation of Tennis Tournament}
\description{
The EloMC package provides tools for calculating different Elo ratings for tennis matches, simulating tournament outcomes, and evaluating betting strategies based on Elo probabilities and bookmaker odds.
}
\details{
This library includes functions for:
  \itemize{
    \item Downloading tennis match data from \url{http://www.tennis-data.co.uk}.
    \item Fitting Elo ratings for each player.
    \item Simulating tournament outcomes.
    \item Evaluating a threshold-base betting strategy
  }
  With EloMC, users can evaluate the relationship between Elo-simulated probabilities and actual bookmaker odds, allowing for the identification of potentially profitable betting opportunities in major Grand Slam tournaments.

To obtain betting odds, visit \url{https://github.com/SaveFonta/Tennis_odds_scraping}
}
\examples{
library(EloMC)
#Download tennis data for ATP players 
X <- merged_tennis_data()


#Clean the df
X.clean<- clean_data(X)


#Fit the Standard Elo
X.clean.fit <- compute_elo(X.clean)
Table<- X.clean.fit$results


#set.seed()

#define the ordered tables of the Australian Open 2024
AO24 <- define_tournament(Table, start_date = "2024-01-14")

#simulate the AO2024 starting from the round of 128 100000 times
round_of_128 <- AO24$sessantaquattresimi_ord
sim_results <- simulate_tournament(round_of_128, sim = 100000)


#BETTING:
#Obtain betting results and simulate Grand Slams from 2019 to 2024, simulating 100000 times each tournament.
#Ensure the right Excel name and path is specified
bet_list <- Results_betting(Table, Excel_name = "Database Tennis.xlsx", n =100000)

#Visualize the total betting results 
balance <- Results_balance(bet_list)
}
\author{
Saverio Fontana
}

\keyword{package}
