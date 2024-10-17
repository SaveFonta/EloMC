\name{Results_betting}
\alias{Results_betting}
\title{Simulate Betting Results for Grand Slam Tournaments}
\description{
  The \code{Results_betting} function simulates betting results for specified Grand Slam tournaments based on player probabilities and bookmaker odds. The function iterates through a series of tournaments, extracting relevant data and computing potential betting outcomes.
}
\usage{
Results_betting(X, n = 1000, tournament = NA, 
                Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3), 
                Q_max = 1000, R_max = 1000)
}
\arguments{
  \item{X}{
    A data frame or object containing player data and match results necessary for tournament simulations. This may include player ratings, match results, and other relevant statistics.
  }
  \item{n}{
    An integer specifying the number of tournament simulations to run. Default is set to 1000.
  }
  \item{tournament}{
    A character string indicating which Grand Slam tournament to simulate. Options include \code{"Australian Open"}, \code{"Roland Garros"}, \code{"US Open"}, and \code{"Wimbledon"}. If set to \code{NA}, simulations will be performed for all tournaments.
  }
  \item{Q}{
    A numeric vector of thresholds for the probability of winning, defining the betting criteria. Default values are set to \code{c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3)}.
  }
  \item{R}{
    A numeric vector of odds ratios for betting decisions, which determine the acceptable odds for placing bets. Default values are set to \code{c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3)}.
  }
  \item{Q_max}{
    A numeric value representing the maximum allowable probability threshold for betting. Default is set to 1000.
  }
  \item{R_max}{
    A numeric value representing the maximum allowable odds ratio for betting. Default is set to 1000.
  }
}
\details{
  The function initializes a data frame containing the start dates and names of the Grand Slam tournaments. It supports the extraction and simulation of betting results for the Australian Open, Roland Garros, US Open, and Wimbledon.

  For each specified tournament, the function defines the tournament structure, simulates match results, and retrieves the corresponding bookmaker odds from an Excel file. The betting results are then calculated based on the provided probability and odds criteria.
}
\value{
  A list where each element corresponds to a Grand Slam tournament and contains the results of the betting simulations, including potential profits and other relevant betting metrics.
}
\examples{
# Example usage of the Results_betting function
# Define player data and tournament simulations
betting_results <- Results_betting(X, n = 1000, tournament = "Australian Open")

# View the simulated betting results for the Australian Open 2024
print(betting_results[["Australian Open 2024"]])
}
\seealso{
  \itemize{
    \item \code{\link{Betting_function}}: Compute betting outcomes based on player probabilities and bookmaker odds.
    \item \code{\link{define_tournament}}: Define tournament matchups based on player ratings.
  }
}
\author{
Saverio Fontana
}
