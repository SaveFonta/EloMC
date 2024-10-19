\name{Results_betting}
\alias{Results_betting}
\title{Simulate Betting Results for Tennis Tournaments}
\description{
  This function simulates betting results for Grand Slam tournaments from 2019 to 2024 (with the last update for the "US Open 2024") based on the relationship between Elo-simulated winning probabilities and bookmaker odds. It retrieves tournament data, set up the ordered tables with \code{\link{define_tournament}} and after simulating the possible outcomes using \code{\link{simulate_tournament}}, calculates betting outcomes using the \code{\link{Betting_function}}. It then returns a summary of the results for each tournament. The betting strategy is better presented in the \code{\link{Betting_function}} documentation. \cr
  The results can be aggregated using the \code{\link{Results_balance}} function for a comprehensive overview of betting performance.
}
\usage{
Results_betting(X, Excel_name, n = 1000, tournament = NA,
                Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3),
                Q_max = 1000, R_max = 1000)
}
\arguments{
  \item{X}{A data frame containing Elo-based probability estimates, typically obtained using the \code{\link{compute_elo}} function.}
  \item{Excel_name}{A string specifying the path to the Excel file containing bookmaker odds and results for the tournaments. To obtain this data, see: \url{https://github.com/SaveFonta/Tennis_odds_scraping}.}
  \item{n}{An integer specifying the number of tournament simulations to perform. Defaults to 1000.}
  \item{tournament}{A character string specifying which tournament to analyze. It can be "Australian Open", "Roland Garros", "US Open", or "Wimbledon". If set to \code{NA}, results for all tournaments will be calculated.}
  \item{Q}{A numeric vector representing thresholds \(q\) for bookmaker probabilities. A bet is placed if the bookmaker probability is greater than \(q\).}
  \item{R}{A numeric vector representing thresholds \(r\) for the Elo-to-bookmaker probability ratio. A bet is placed if this ratio exceeds \(r\), targeting favorable bets.}
  \item{Q_max}{An upper limit for bookmaker probabilities, default is 1000. Only bets with bookmaker probabilities less than this are considered.}
  \item{R_max}{An upper limit for the betting ratio, default is 1000. Only bets with an Elo-to-bookmaker ratio below this are considered.}
}
\details{
  The function retrieves tournament data for major Grand Slam events and runs simulations for each tournament specified. It uses the \code{\link{define_tournament}} function to set up the tournament structure and the \code{\link{simulate_tournament}} function to simulate match outcomes.

  For each tournament, it reads the bookmaker odds from the specified Excel file and applies the \code{\link{Betting_function}} to evaluate betting opportunities based on the Elo-based probabilities and bookmaker odds.

  The tournaments used for evaluating the betting strategy include all Grand Slams from 2019 to 2024, excluding Wimbledon 2020 due to the COVID-19 pandemic.

  The results from all tournaments are compiled into a list, allowing users to easily access the betting outcomes for each tournament. The output of this function can be summarized using the \code{\link{Results_balance}} function to evaluate the overall betting strategy.
}
\value{
  A list of data frames, each containing the betting results for a specific tournament. Each data frame includes:
  \itemize{
    \item The combination of q and r values used for betting.
    \item The net balance (profit or loss) for each combination.
    \item A list of players that were bet on.
    \item The number of unique bets placed.
    \item The number of winning bets.
  }
}
\seealso{
  \code{\link{define_tournament}}, \cr
  \code{\link{simulate_tournament}}, \cr
  \code{\link{Betting_function}}, \cr
  \code{\link{compute_elo}}
}
\author{
 Saverio Fontana
}
\examples{
# Assuming `estimation` is a data frame obtained using the function compute_elo:
fit <- estimation$results

# Obtain betting results for all the tournaments, assuming the Excel file is calles "Database Tennis.xlsx"
Results_betting <- (fit, "Database Tennis.xlsx", n = 10000, tournament = NA,
                Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3),
                Q_max = 1000, R_max = 1000)
}

\keyword{Elo}
\keyword{tennis}
\keyword{ratings}
