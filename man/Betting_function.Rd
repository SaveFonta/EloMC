\name{Bettiamo_seq_df}
\alias{Bettiamo_seq_df}
\title{Sequential Betting Analysis Based on ELO and Bookmaker Probabilities}
\description{
  The \code{Bettiamo_seq_df} function performs a sequential betting analysis using player probabilities derived from ELO ratings and bookmaker odds. The function evaluates different combinations of threshold values for ELO probabilities and bookmaker odds to calculate the balance from betting outcomes.
}
\usage{
Bettiamo_seq_df(X, Book, q_values, r_values, q_max = 1000, r_max = 1000)
}
\arguments{
  \item{X}{
    A data frame containing player names and their respective ELO probabilities.
  }
  \item{Book}{
    A data frame containing bookmaker probabilities and betting odds for the players.
  }
  \item{q_values}{
    A numeric vector of threshold values for ELO probabilities to filter potential bets.
  }
  \item{r_values}{
    A numeric vector of ratios to compare ELO probabilities with bookmaker probabilities.
  }
  \item{q_max}{
    A numeric value indicating the maximum threshold for ELO probabilities. Default is \code{1000}.
  }
  \item{r_max}{
    A numeric value indicating the maximum ratio for ELO and bookmaker probabilities. Default is \code{1000}.
  }
}
\details{
  This function analyzes potential betting opportunities by merging player probability data from ELO ratings with bookmaker odds. It iterates over specified threshold values for ELO probabilities and ratios of ELO to bookmaker odds to filter for potential bets. For each combination of \code{q} and \code{r}, it calculates the balance from bets placed and counts the number of bets won.

  The function identifies players that meet the specified criteria and keeps track of betting results to return a summary data frame.
}
\value{
  A data frame containing the results of the betting analysis:
  \itemize{
    \item \code{q_Value}: The ELO probability threshold used for filtering.
    \item \code{r_Value}: The ratio threshold used for filtering.
    \item \code{Balance}: The overall balance from bets placed for the given thresholds.
    \item \code{Players}: A character string listing the players on whom bets were placed.
    \item \code{Num_Bets}: The number of unique players bet on for the given thresholds.
    \item \code{N_Bets_won}: The number of bets that were successful.
  }
}
\examples{
# Example usage of the Bettiamo_seq_df function
# Assuming `elo_probabilities` contains ELO probabilities and `bookmaker_probs` contains bookmaker odds
# Define your q_values and r_values for analysis
q_values <- seq(0.5, 1, by = 0.1)
r_values <- seq(1, 2, by = 0.5)

# Perform the betting analysis
results <- Bettiamo_seq_df(elo_probabilities, bookmaker_probs, q_values, r_values)

# View the results
print(results)
}
\seealso{
  \itemize{
    \item \code{\link{Bettiamo_seq_df}}: Perform sequential betting analysis.
    \item \code{\link{compute_elo}}: Compute ELO ratings for players based on match results.
    \item \code{\link{clean_data}}: Clean and preprocess tennis match data for analysis.
  }
}
\author{
Saverio Fontana
}
