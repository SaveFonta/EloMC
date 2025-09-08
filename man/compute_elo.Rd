\name{compute_elo}
\alias{compute_elo}
\title{Compute Elo and WElo Ratings for Tennis Matches}
\description{
The \code{compute_elo} function calculates Elo ratings for tennis players based on historical match outcomes.
It supports multiple variants of Elo, including:
\itemize{
  \item Standard Elo (\code{K = "Kovalchik"} or numeric constant K).
  \item Tournament-adjusted Elo (\code{K = "Grand_Slam"}).
  \item Surface-adjusted Elo (\code{K = "Surface_Hard"}, \code{"Surface_Clay"}, \code{"Surface_Grass"}).
  \item Weighted Elo (WElo), which incorporates match dominance indicators (games or sets won fractions).
}
Optional confidence intervals for Elo and WElo updates are computed using a bootstrap approach.
The function outputs detailed match-level ratings and overall performance metrics.
}
\usage{
compute_elo(x, W = "GAMES", SP = 1500, K = "Kovalchik", s = 0.5,
            CI = FALSE, alpha = 0.05, B = 1000, WELO = FALSE, verbose = TRUE)
}
\arguments{
  \item{x}{
    A \code{data.frame} (typically prepared using \code{\link{clean_data}}) containing tennis match data.
    Required columns: \code{Winner}, \code{Loser}, \code{Comment}, \code{Date}, \code{Series}, \code{Round}, \code{Surface}.
    If \code{WELO = TRUE}, the dataset must also include either:
    \itemize{
      \item \code{f_g_Winner}, \code{f_g_Loser} (for \code{W = "GAMES"})
      \item \code{f_s_Winner}, \code{f_s_Loser} (for \code{W = "SETS"})
    }
  }
  \item{W}{
    Weighting type: either \code{"GAMES"} (default) or \code{"SETS"}.
    Determines whether WElo uses fractions of games or sets won.
  }
  \item{SP}{
    Numeric; starting Elo rating for all players. Default is \code{1500}.
  }
  \item{K}{
    K-factor rule for Elo updates. Options:
    \itemize{
      \item \code{"Kovalchik"}: dynamic K-factor depending on matches played.
      \item \code{"Grand_Slam"}: reduces K outside Grand Slam tournaments (scaled by \code{s}).
      \item \code{"Surface_Hard"}, \code{"Surface_Clay"}, \code{"Surface_Grass"}: reduce K outside the specified surface (scaled by \code{s}).
      \item Numeric: constant K value.
    }
    Default is \code{"Kovalchik"}.
  }
  \item{s}{
    Numeric scaling factor applied when tournament/surface conditions differ from the target. Default is \code{0.5}.
  }
  \item{CI}{
    Logical; if \code{TRUE}, computes confidence intervals for Elo and WElo ratings using bootstrap simulations. Default is \code{FALSE}.
  }
  \item{alpha}{
    Numeric; significance level for confidence intervals. Default is \code{0.05}.
  }
  \item{B}{
    Integer; number of bootstrap samples used for CI estimation. Default is \code{1000}.
  }
  \item{WELO}{
    Logical; if \code{TRUE}, computes Weighted Elo (WElo), which incorporates match dominance indicators. Default is \code{FALSE}.
  }
  \item{verbose}{
    Logical; if \code{TRUE}, prints progress messages, summary statistics, and performance metrics. Default is \code{TRUE}.
  }
}
\details{
The Elo probability that player \eqn{i} beats player \eqn{j} at time \eqn{t} is:
\deqn{\hat{p}_{i,j}(t) = \frac{1}{1 + 10^{(E_j(t) - E_i(t))/400}}}

After a completed match, ratings are updated as:
\deqn{E_i(t+1) = E_i(t) + K_i(t) \cdot \left[ W_i(t) - \hat{p}_{i,j}(t) \right]}

Where:
\itemize{
  \item \eqn{W_i(t)} = 1 if player \eqn{i} wins, 0 otherwise.
  \item \eqn{K_i(t)} = K-factor depending on chosen rule, tournament, and surface.
}

\strong{Weighted Elo (WElo):}
If \code{WELO = TRUE}, updates are weighted by match dominance indicators:
\deqn{E_i(t+1) = E_i(t) + K_i(t) \cdot \left[ W_i(t) - \hat{p}_{i,j}(t) \right] \cdot w_i}
where \eqn{w_i} is the fraction of games or sets won by player \eqn{i}.

\strong{Confidence Intervals:}
If \code{CI = TRUE}, bootstrap simulations are used to generate percentile-based confidence intervals for ratings before and after each match.
}
\value{
An object of class \code{welo}, which is a list containing:
\item{results}{A \code{data.frame} with match-level Elo/WElo ratings before and after each match, probabilities, and optional confidence intervals.}
\item{matches}{Character string summarizing number of matches processed.}
\item{period}{Character string with start and end dates of the dataset.}
\item{loss}{A data frame with Brier and LogLoss metrics for Elo and WElo predictions.}
\item{highest_elo}{A string with the player and date of the highest Elo rating achieved.}
\item{highest_welo}{If \code{WELO = TRUE}, a string with the player and date of the highest WElo rating achieved.}
\item{dataset}{The original input dataset.}
\item{execution_time}{Computation time in seconds.}
\item{players_count}{Number of unique players evaluated.}
\item{final_ratings}{A list containing final Elo (and WElo, if applicable) ratings for all players.}
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. \cr
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127–138. \cr
Angelini, G., Candila, V., & De Angelis, L. (2022). \emph{Weighted Elo rating for tennis match predictions}. European Journal of Operational Research, 297(1), 120–132. \cr
\url{https://github.com/SaveFonta}
}
\author{Saverio Fontana}
\note{
The function ignores matches with \code{Comment != "Completed"} when updating ratings.
WElo requires precomputed dominance statistics, which can be added using \code{\link{clean_data}(WELO = TRUE)}.
}
\seealso{
\code{\link{clean_data}}, \cr
\code{\link{merged_tennis_data}}, \cr
\code{\link{define_tournament}}
}
\examples{
## Load and clean tennis dataset
tennis_data <- merged_tennis_data()
cleaned <- clean_data(tennis_data, WELO = TRUE)

## Compute Elo with surface-adjusted K-factor (Clay)
elo_out <- compute_elo(cleaned, K = "Surface_Clay", s = 0.5, WELO = FALSE)
head(elo_out$results)

## Compute WElo with confidence intervals
welo_out <- compute_elo(cleaned, W = "SETS", WELO = TRUE, CI = TRUE, B = 500)
head(welo_out$results)
}
\keyword{Elo}
\keyword{WElo}
\keyword{tennis}
\keyword{ratings}
\keyword{probability}
