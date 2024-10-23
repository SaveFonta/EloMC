\name{Betting_function}
\alias{Betting_function}
\title{Simulate Betting Strategy Based on Elo Probabilities and Bookmaker Odds (Internal)}
\description{
  This internal function evaluates a betting strategy based on Elo-simulated probabilities and bookmaker ante-post odds. It places bets when the ratio of Elo probability to bookmaker probability exceeds a specified threshold r and the bookmaker probability is above a certain value q. This strategy is used within the function \code{\link{Results_betting}} to provide betting results for Grand Slams from 2019 to 2024.
}
\usage{
Betting_function(X, Book, q_values, r_values, q_max = 1000, r_max = 1000)
}
\arguments{
  \item{X}{A data frame containing Elo-based probability estimates for tournaments outcome, typically obtained using the \code{\link{simulate_tournament}} function.}
  \item{Book}{A data frame containing bookmaker data with columns: \code{Player}, \code{Prob_bookmaker} (bookmaker probabilities), \code{Quota} (odds), and \code{Result} (either "WINNER" or \code{NA}). To obtain this data, see: \url{https://github.com/SaveFonta/Tennis_odds_scraping}.}
  \item{q_values}{A numeric vector representing thresholds q for bookmaker probabilities. A bet is placed if the bookmaker probability is greater than \(q\).}
  \item{r_values}{A numeric vector representing thresholds r for the Elo-to-bookmaker probability ratio. A bet is placed if this ratio exceeds r, targeting favorable bets.}
  \item{q_max}{An upper limit for bookmaker probabilities, default is 1000. Only bets with bookmaker probabilities less than this are considered.}
  \item{r_max}{An upper limit for the betting ratio, default is 1000. Only bets with an Elo-to-bookmaker ratio below this are considered.}
}
\details{
Let \eqn{o_i^{(h)} (t+C_h | t)} denote the (outright) odd provided by the bookmaker at time \eqn{t} for player \eqn{i} to win tournament \eqn{h}, which starts at time \eqn{t+1} and consists of \eqn{C_h} rounds.

We define the implied winning probability for player \eqn{i} as the reciprocal of the outright odd provided by the bookmaker, i.e.,

\deqn{
  q_i^{(h)} (t+C_h | t) = \frac{1}{o_i^{(h)} (t+C_h | t)}
}

Additionally, let \eqn{\hat{p}_i^{(h)}(t + C_h | t)} represent the estimated probability that player \eqn{i} will win the tournament, derived from simulating the tournament using Elo ratings. This simulation estimates each player's chances of advancing through all the rounds of the tournament, ultimately producing \eqn{\hat{p}_i^{(h)}} as the Elo-based winning probability (see   \code{\link{simulate_tournament}}).

This function implements a betting strategy where a bet is placed under the following conditions:

\deqn{
    \frac{\hat{p}_i^{(h)}(t + C_h | t)}{q_i^{(h)}(t + C_h | t)} > r \quad \text{with} \ r > 1
}
and
\deqn{
    q_i^{(h)}(t + C_h | t) > q
}

The condition \eqn{\frac{\hat{p}_i^{(h)}(t + C_h | t)}{q_i^{(h)}(t + C_h | t)} > r} ensures that a bet is placed only when the Elo-based probability is sufficiently higher than the bookmaker's implied probability, which reflects a potential market inefficiency. The higher the threshold \eqn{r}, the fewer the betting opportunities, but potentially the higher the chances of winning.

Furthermore, the condition \eqn{q_i^{(h)}(t + C_h | t) > q} ensures that bets are placed only when the bookmaker's implied probability is above a specified threshold, to account for biases such as the favorite-longshot bias in betting markets.

This function is intended for internal use within functions like \code{\link{Results_betting}}.
}


\value{
  A data frame summarizing the results for each combination of \code{q} and \code{r}, with columns:
  \itemize{
    \item \code{q_Value}: The probability threshold \(q\) used.
    \item \code{r_Value}: The ratio threshold \(r\) used.
    \item \code{Balance}: The net profit or loss for this combination.
    \item \code{Players}: A list of players bet on for this combination.
    \item \code{Num_Bets}: The number of unique bets placed for this combination.
    \item \code{N_Bets_won}: The number of winning bets.
  }
}
\seealso{
  \code{\link{define_tournament}}, \cr
  \code{\link{clean_data}}, \cr
  \code{\link{simulate_tournament}}, \cr
  \code{\link{Results_betting}}
}
\author{
Saverio Fontana
}


