\name{compute_elo}
\alias{compute_elo}
\title{
Calculate Elo Ratings for Tennis Matches with Surface Adjustments
}
\description{
The \code{compute_elo} function calculates Elo ratings for tennis players based on match outcomes, incorporating adjustments for surface types and tournament significance. The function supports standard Elo ratings and variations such as surface-adjusted Elo and surface-specific Elo ratings. It can also generate confidence intervals and custom weights to refine rating computations.
}
\usage{
compute_elo(x, W = "GAMES", SP = 1500, K = "Kovalchik", s = 0.5,
            CI = FALSE, alpha = 0.05, B = 1000, WELO = FALSE)
}
\arguments{
  \item{x}{
    A data frame containing match results with required columns for winners, losers, and match metadata (e.g., \code{Date}, \code{Series}, \code{Round}, \code{Surface}, \code{Comment}).
  }
  \item{W}{
    A character string specifying the type of weighting to apply. Options include \code{"GAMES"} or \code{"SETS"}. The default is \code{"GAMES"}.
  }
  \item{SP}{
    A numeric value representing the starting Elo rating assigned to all players. The default is \code{1500}.
  }
  \item{K}{
    Either a character string or numeric value specifying the K-factor used for Elo rating adjustments. Options include \code{"Kovalchik"}, \code{"Grand_Slam"}, \code{"Surface_Hard"}, \code{"Surface_Clay"}, \code{"Surface_Grass"}, or a numeric K-value. Default is \code{"Kovalchik"}.
  }
  \item{s}{
    A numeric scaling factor applied to adjust K-factor values based on tournament significance. The default is \code{0.5}.
  }
  \item{CI}{
    A logical value indicating whether to calculate confidence intervals for Elo predictions. The default is \code{FALSE}.
  }
  \item{alpha}{
    A numeric value for the significance level when calculating confidence intervals. The default is \code{0.05}.
  }
  \item{B}{
    An integer specifying the number of bootstrap samples used for confidence interval estimation. The default is \code{1000}.
  }
  \item{WELO}{
    A logical value indicating whether to calculate WElo ratings, which incorporate additional match performance data. The default is \code{FALSE}.
  }
}
\details{
The Elo rating system estimates the probability that player \(i\) will win against player \(j\) in a match at time \(t\) as follows:

\deqn{\hat{p}_{i,j}(t) = \frac{1}{1 + 10^{(E_j(t) - E_i(t))/400}}}

The Elo rating for player \(i\) is then updated based on match results:

\deqn{E_i(t + 1) = E_i(t) + K_i(t) \left [W_i(t) - \hat{p}_{i,j}(t)\right]}

Where:
\itemize{
  \item W_i(t): Binary indicator (1 if player \(i\) wins, 0 otherwise).
  \item K_i(t): Constant factor determining the magnitude of rating adjustments, which can vary based on match significance or surface type.
}

This function also supports surface-specific Elo ratings and WElo ratings, where WElo further incorporates match dominance indicators. Different K-factor options adjust the Elo updates according to match conditions such as surface and tournament significance. For example, adjustments for Grand Slam tournaments or specific surfaces are made by setting parameter \(s\) based on match importance:

\deqn{S =
\begin{cases}
1 & \text{if match is significant (e.g., Grand Slam)} \\
s & \text{otherwise, where \( s < 1 \)}
\end{cases}}

Confidence intervals are optionally computed using a bootstrap approach, and custom weights can be applied to tailor the rating updates.
}
\value{
A list containing:
  \item{results}{A data frame with match results, including Elo ratings before and after each match, and, if requested, WElo ratings and confidence intervals.}
  \item{matches}{A summary of the total number of matches processed.}
  \item{period}{A string indicating the date range covered by the matches.}
  \item{loss}{A matrix with Brier and log-loss values for both Elo and WElo ratings (if \code{WELO = TRUE}).}
  \item{highest_elo}{A string with the player and date corresponding to the highest Elo rating achieved.}
  \item{highest_welo}{If \code{WELO = TRUE}, a string with the player and date corresponding to the highest WElo rating achieved.}
  \item{dataset}{The original input data frame, \code{x}.}
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. Arco Publishing.
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127-138.
}
\seealso{
\code{\link{clean_data}}, \code{\link{merged_tennis_data}}
}
\examples{
# Example usage:
# Assuming `match_data` is a data frame with the required columns:
result <- compute_elo(match_data, W = "SETS", K = "Surface_Clay", WELO = TRUE)
print(result)
}
\author{
Saverio Fontana
}
\keyword{Elo}
\keyword{tennis}
\keyword{ratings}


