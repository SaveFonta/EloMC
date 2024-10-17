\name{Results_balance}
\alias{Results_balance}
\title{Calculate Betting Results Summary}
\description{
  The \code{Results_balance} function computes the overall balance, total number of bets, and number of won bets from a list of betting results. It provides a summary of the betting outcomes including the return on investment (ROI) expressed as a percentage.
}
\usage{
Results_balance(x)
}
\arguments{
  \item{x}{
    A list of data frames, where each data frame contains details of betting outcomes, including balance, number of bets, and number of bets won. Each data frame should have the following columns:
    \itemize{
      \item \code{Balance}: Numeric value representing the net balance after the bets.
      \item \code{Num_Bets}: Integer value indicating the total number of bets placed.
      \item \code{N_Bets_won}: Integer value indicating the number of bets that were won.
      \item \code{q_Value}: Numeric value indicating the probability threshold used for betting.
      \item \code{r_Value}: Numeric value indicating the odds ratio used for betting.
    }
  }
}
\details{
  The function iterates through the list of betting results, summing up the balances, total bets, and won bets. It calculates the return on investment (ROI) as the ratio of the total balance to the total number of bets, multiplied by 100 to express it as a percentage. If the ROI calculation results in a NaN (due to division by zero), it replaces the ROI with zero.
}
\value{
  A data frame summarizing the betting results, containing the following columns:
  \itemize{
    \item \code{q_Value}: The probability threshold used for betting.
    \item \code{r_Value}: The odds ratio used for betting.
    \item \code{Balance}: The total balance after all bets, rounded to two decimal places.
    \item \code{Num_Bets}: The total number of bets placed.
    \item \code{N_Bets_won}: The total number of bets won.
    \item \code{ROI}: The return on investment, expressed as a percentage. If ROI cannot be calculated, it is set to zero.
  }
}
\examples{
# Example usage of the Results_balance function
# Simulated betting results
betting_results <- list(
  data.frame(q_Value = 0.05, r_Value = 1.10, Balance = 100, Num_Bets = 20, N_Bets_won = 12),
  data.frame(q_Value = 0.05, r_Value = 1.10, Balance = 150, Num_Bets = 30, N_Bets_won = 15)
)

# Calculate the summary of betting results
summary_results <- Results_balance(betting_results)

# Display the results
print(summary_results)
}
\author{
Saverio Fontana
}

