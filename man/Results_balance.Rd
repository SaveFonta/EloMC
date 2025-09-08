\name{Results_balance}
\alias{Results_balance}
\title{
Summarize Betting Results Across Multiple Tournaments
}
\description{
Aggregates betting outcomes from multiple tournaments to compute total balance,
number of bets, number of bets won, and return on investment (ROI).
}
\usage{
Results_balance(x)
}
\arguments{
  \item{x}{
    A list of betting results, where each element is a list returned by \code{\link{Place_bet}} for a tournament.
    Each element must contain \code{Balance}, \code{Num_Bets}, \code{N_Bets_won}, \code{q_Value}, and \code{r_Value}.
  }
}
\details{
This function iterates through each tournament in the input list, sums the total balance,
counts the number of bets placed, counts the number of bets won, and calculates the ROI.

The ROI is calculated as:
\deqn{ROI = (Total\ Balance / Total\ Bets) \times 100\%}

If the number of bets is zero, the ROI is set to 0 to avoid division by zero.
}
\value{
A data frame with the following columns:
\item{q_Value}{The betting fraction value used in the strategy.}
\item{r_Value}{The payout multiplier associated with the betting fraction.}
\item{Balance}{Total balance across all tournaments, rounded to 2 decimal places.}
\item{Num_Bets}{Total number of bets placed across all tournaments.}
\item{N_Bets_won}{Total number of bets won across all tournaments.}
\item{ROI}{Return on investment as a percentage.}
}
\references{
De Angelis, Luca & Fontana, Saverio (2024). Monte Carlo meets Wimbledon: Elo-based simulations for predicting tennis tournament winners. SSRN. DOI: 10.2139/ssrn.5027380.
}
\author{
Saverio Fontana
}
\note{
Ensure that all elements of the input list contain the required components (\code{Balance}, \code{Num_Bets}, \code{N_Bets_won}, \code{q_Value}, and \code{r_Value}).
The function is intended to summarize the output of \code{\link{Place_bet}} for multiple tournaments.
}
\seealso{
\code{\link{Place_bet}}, \code{\link{Estract_book}}, \code{\link{GS_sim}}
}
\examples{
## Example workflow:

## 1. Extract betting odds
book_all <- Estract_book("tennis_odds.xlsx")

## 2. Simulate tournament probabilities
probabilities <- GS_sim(X = my_data, tournament = NA, n = 1000)

## 3. Place bets according to probabilities and odds
bet_results <- Place_bet(probab = probabilities, book = book_all)

## 4. Aggregate betting results
overall_results <- Results_balance(bet_results)

## Display summary
print(overall_results)
}
\keyword{betting}
\keyword{tennis}
\keyword{simulation}
\concept{Grand Slam betting}
