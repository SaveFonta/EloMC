\name{simulate_tournament}
\alias{simulate_tournament}
\title{Simulate Tennis Tournament Outcomes Based on ELO or WELO Ratings}
\description{
The \code{simulate_tournament} function simulates a single-elimination tennis tournament using either ELO or WELO ratings for the players. The simulation runs for a specified number of rounds and is repeated across multiple iterations to estimate each player’s probability of reaching various tournament stages, including the round of 64, round of 32, round of 16, quarterfinals, semifinals, and finals.
}
\usage{
simulate_tournament(X, sim = 10000, WELO = FALSE)
}
\arguments{
  \item{X}{
    A data frame or matrix representing the tournament matchups. It should contain at least six columns:
    \enumerate{
      \item Names of Player 1 for each matchup.
      \item Names of Player 2 for each matchup.
      \item ELO or WELO rating of Player 1.
      \item ELO or WELO rating of Player 2.
      \item Number of previous wins for Player 1.
      \item Number of previous wins for Player 2.
    }
    The number of rows in \code{X} should correspond to the number of initial matchups in the tournament (typically 64 or 32).
  }

  \item{sim}{
    A numeric value specifying the number of tournament simulations to perform. The default is \code{10000}.
  }

  \item{WELO}{
    A logical value indicating whether to use the WELO rating system. If \code{TRUE}, WELO ratings are used for match simulations; if \code{FALSE}, standard ELO ratings are employed. The default is \code{FALSE}.
  }
}
\details{
The \code{simulate_tournament} function performs a full tournament simulation in a single-elimination format, using either ELO or WELO ratings to determine match outcomes. For each match, the winner is determined using a Bernoulli distribution, where the probability of Player 1 winning is calculated based on their ELO or WELO rating relative to their opponent's.

After each match, winners advance to the next round, and player ratings are updated based on performance, influencing their chances in subsequent rounds. The function executes \code{sim} simulations and aggregates outcomes to estimate the probability of each player advancing through the tournament stages: round of 64, round of 32, round of 16, quarterfinals, semifinals, finals, and winning the tournament. These probabilities provide insights into player performance across numerous tournament scenarios.
}

\value{
A list containing the estimated probabilities for each player reaching various stages of the tournament across all simulations:
  \item{prob.64}{Probabilities of each player advancing past the round of 64.}
  \item{prob.32}{Probabilities of each player advancing past the round of 32.}
  \item{prob.16}{Probabilities of each player advancing past the round of 16.}
  \item{prob.8}{Probabilities of each player advancing to the quarterfinals.}
  \item{prob.4}{Probabilities of each player advancing to the semifinals.}
  \item{prob.2}{Probabilities of each player reaching the finals.}
  \item{prob.1}{Probabilities of each player winning the tournament.}
}
\examples{
# Example: Simulating a 64-player tournament using the output from define_tournament
# Assume cleaned_data is the result of clean_data() function

start_date <- "2023-01-16"
Serie <- "Grand Slam"
result <- define_tournament(cleaned_data, start_date, Serie, WELO = TRUE)

round_of_128 <- result$sessantaquattresimi_ord

# Simulating tournament outcomes with ELO ratings
simulate_tournament(round_of_128, sim = 10000, WELO = FALSE)

# Example using the WELO rating system
tournament_results_welo <- simulate_tournament(round_of_128, sim = 10000, WELO = TRUE)

}
\seealso{
  \itemize{
    \item \code{\link{define_tournament}}: Create tournament matchups based on player ratings.
    \item \code{\link{clean_data}}: Clean and preprocess tennis match data for analysis.
    \item \code{\link{compute_elo}}: Compute Elo ratings for players based on match results.
    \item \code{\link{tennis_prob}}: Calculate the probability of winning for given player ratings.
  }
}

\author{
Saverio Fontana
}
\keyword{simulation}
\keyword{ELO}
\keyword{WELO}
\keyword{tennis}
\keyword{tournament}
