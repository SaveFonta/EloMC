\name{Results_balance}
\alias{Results_balance}
\title{Calculate Overall Betting Balance}
\description{
  This function computes the overall balance, total number of bets, number of winning bets, and the return on investment (ROI) from a list of betting results. It aggregates these statistics from the output of the \code{\link{Results_betting}} function to provide a comprehensive summary for evaluating the betting strategy.
}
\usage{
Results_balance(x)
}
\arguments{
  \item{x}{A list of data frames, where each data frame contains the results of betting for a specific tournament, typically obtained from the \code{\link{Results_betting}} function. Each data frame should include columns for \code{Balance}, \code{Num_Bets}, and \code{N_Bets_won}.}
}
\details{
  The function iterates over each data frame in the list, summing up the total balance, total number of bets placed, and the number of winning bets across all entries. It calculates the overall return on investment (ROI) as a percentage, which is defined as the total balance divided by the total number of bets, multiplied by 100.

  If the ROI calculation results in a NaN (for instance, if no bets were placed), it is replaced with 0 for clarity in the output.

  The resulting data frame contains the following columns:
  \itemize{
    \item \code{q_Value}: The probability threshold q used in the betting strategy.
    \item \code{r_Value}: The ratio threshold r used in the betting strategy.
    \item \code{Balance}: The net balance (profit or loss) from all bets.
    \item \code{Num_Bets}: The total number of bets placed across all tournaments.
    \item \code{N_Bets_won}: The total number of winning bets.
    \item \code{ROI}: The return on investment as a percentage.
  }
}
\value{
  A data frame summarizing the overall betting performance, including the net balance, number of bets, number of winning bets, and ROI, derived from the output of the \code{\link{Results_betting}} function.
}
\seealso{
  \code{\link{define_tournament}}, \cr
  \code{\link{simulate_tournament}}, \cr
  \code{\link{Results_betting}}
}
\author{
Saverio Fontana
}
