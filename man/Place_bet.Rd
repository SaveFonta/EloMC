\name{Place_bet}
\alias{Place_bet}
\title{
Compute Betting Outcomes Based on Probabilities and Odds
}
\description{
Places bets on tennis tournaments based on simulated probabilities and betting odds.
Supports multiple tournaments and applies configurable betting strategies.
}
\usage{
Place_bet(probab, book, Q_values = c(0.02, 0.05, 0.10, 0.15, 0.2),
          R_values = c(1.05, 1.1, 1.20, 1.4, 1.6, 1.8),
          Q_max = 1000, R_max = 1000)
}
\arguments{
  \item{probab}{
    A list of simulated probabilities for each tournament.
    Each element corresponds to a tournament and contains a numeric vector of win probabilities for each player.
  }
  \item{book}{
    A list of betting odds data frames for each tournament, as returned by \code{\link{Estract_book}}.
  }
  \item{Q_values}{
    Numeric vector of fraction thresholds for the Kelly-based staking strategy. Default is \code{c(0.02, 0.05, 0.10, 0.15, 0.2)}.
  }
  \item{R_values}{
    Numeric vector of payout multipliers associated with \code{Q_values}. Default is \code{c(1.05, 1.1, 1.20, 1.4, 1.6, 1.8)}.
  }
  \item{Q_max}{
    Maximum allowed stake per bet. Default is 1000.
  }
  \item{R_max}{
    Maximum allowed potential return per bet. Default is 1000.
  }
}
\details{
This function iterates over each tournament in the \code{probab} and \code{book} lists,
computes optimal bet sizes using a configurable Kelly-like strategy (via \code{Betting_function}),
and returns the results in a list structure.

\code{Q_values} and \code{R_values} define the tiered betting strategy:
for each probability threshold in \code{Q_values}, the corresponding payout multiplier in \code{R_values} is applied.
\code{Q_max} and \code{R_max} impose hard limits to avoid excessive stakes or returns.

The function prints a message for each tournament after placing the bets.
}
\value{
A list where each element corresponds to a tournament.
Each element contains the results of \code{\link{Betting_function}} for that tournament, including recommended stakes and expected returns.
}
\references{
De Angelis, Luca & Fontana, Saverio (2024). Monte Carlo meets Wimbledon: Elo-based simulations for predicting tennis tournament winners. SSRN. DOI: 10.2139/ssrn.5027380.
}
\author{
Saverio Fontana
}
\note{
Ensure that the lengths of \code{probab} and \code{book} match, i.e., each tournament in \code{probab} has a corresponding betting odds data frame in \code{book}.
If lengths do not match, the function returns an error message.
}
\seealso{
\code{\link{Estract_book}}, \code{\link{GS_sim}}, \code{\link{Betting_function}}
}
\examples{
## Example workflow:

## 1. Extract betting odds
book_all <- Estract_book("tennis_odds.xlsx")

## 2. Simulate tournament probabilities
probabilities <- GS_sim(X = my_data, tournament = NA, n = 1000)

## 3. Place bets according to probabilities and odds
bet_results <- Place_bet(probab = probabilities, book = book_all)

## Access betting results for Wimbledon 2024
wimbledon_bets <- bet_results[["Wimbledon 2024"]]
}
\keyword{betting}
\keyword{tennis}
\keyword{
