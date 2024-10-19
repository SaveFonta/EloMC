\name{simulate_tournament}
\alias{simulate_tournament}
\title{Simulate Single-Elimination Tennis Tournament Outcomes Using ELO or WELO Ratings}
\description{
  The \code{simulate_tournament} function simulates outcomes for a single-elimination tennis tournament based on players' ELO or WELO ratings. The simulations are repeated \code{sim} times, to estimate each player's probabilities of reaching different stages, such as the round of 64, round of 32, quarterfinals, semifinals, and finals.
}
\usage{
simulate_tournament(X, sim = 10000, WELO = FALSE)
}
\arguments{
  \item{X}{
    A data frame with the ordered table (ensuring the correct matchups) to start the simulation process. NOTE this is obtained using the function \code{\link{define_tournament}}.}

  \item{sim}{
    A numeric value specifying the number of simulations to run. The default is \code{10000}.
  }

  \item{WELO}{
    A logical value indicating whether to use the WELO rating system. If \code{TRUE}, WELO ratings are used; if \code{FALSE}, standard ELO ratings are used. The default is \code{FALSE}.
  }
}
\details{
  The \code{simulate_tournament} function simulates a tennis tournament where match outcomes are determined based on ELO or WELO ratings. Each match's result is drawn from a Bernoulli(p) distribution, where p is the probability that player i will win against player j in a match at time t and is computed as follows:

\deqn{\hat{p}_{i,j}(t) = \frac{1}{1 + 10^{(E_j(t) - E_i(t))/400}}}

The Elo rating for player i is then updated based on match results:

\deqn{E_i(t + 1) = E_i(t) + K_i(t) \left [W_i(t) - \hat{p}_{i,j}(t)\right]}

Where:
\itemize{
  \item W_i(t): Binary indicator (1 if player i wins, 0 otherwise).
  \item K_i(t): Constant factor determining the magnitude of rating adjustments, which can vary based on match significance or surface type.
}



The function performs \code{sim} simulations, updating player ratings after each match. After all simulations are complete, the probabilities for each player advancing to various stages (round of 64, 32, quarterfinals, semifinals, finals) and winning the tournament are estimated.

  This approach helps estimate how likely each player is to advance through the tournament and provides insights into player performance under various scenarios.
}
\value{
  A list containing estimated probabilities for each player advancing through different tournament stages:
  \itemize{
    \item{\code{prob.64}:}{Probability of advancing past the round of 64.}
    \item{\code{prob.32}:}{Probability of advancing past the round of 32.}
    \item{\code{prob.16}:}{Probability of advancing past the round of 16.}
    \item{\code{prob.8}:}{Probability of advancing to the quarterfinals.}
    \item{\code{prob.4}:}{Probability of advancing to the semifinals.}
    \item{\code{prob.2}:}{Probability of reaching the finals.}
    \item{\code{prob.1}:}{Probability of winning the tournament.}
  }
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. \cr
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127-138. \cr
Angelini, G., Candila, V., & De Angelis, L. (2022). Weighted Elo rating for tennis match predictions. \emph{European Journal of Operational Research}, 297(1), 120-132.s \cr
\url{https://github.com/SaveFonta}
}

\seealso{
  \code{\link{define_tournament}}\cr
  \code{\link{clean_data}}\cr
  \code{\link{compute_elo}} \cr
  \code{\link{tennis_prob}}
}
\author{
Saverio Fontana
}

\examples{
# Example: Simulating a 64-player tournament using the output from define_tournament

start_date <- "2023-01-16"
Serie <- "Grand Slam"
result <- define_tournament(fit, start_date, Serie, WELO = TRUE)

# Extract the ordered table for the Round of 128
round_of_128 <- result$sessantaquattresimi_ord

# Simulate tournament outcomes using ELO ratings
simulate_tournament(round_of_128, sim = 10000, WELO = FALSE)

# Simulate tournament outcomes using WELO ratings
tournament_results_welo <- simulate_tournament(round_of_128, sim = 10000, WELO = TRUE)
}
\keyword{simulation}
\keyword{ELO}
\keyword{WELO}
\keyword{tennis}
\keyword{tournament}

