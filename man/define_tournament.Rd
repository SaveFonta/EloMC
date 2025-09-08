\name{define_tournament}
\alias{define_tournament}
\title{Define Tournament Structure and Extract Data for Each Round}
\description{
The \code{define_tournament} function extracts and orders match-level data for a given tennis tournament, round by round, ensuring the correct structure of the draw.
It is typically applied to the \code{results} data frame from \code{\link{compute_elo}} and prepares the tournament bracket for later simulation with \code{\link{simulate_tournament}}.
}
\usage{
define_tournament(X, start_date, Serie = "Grand Slam", WELO = FALSE)
}
\arguments{
  \item{X}{
    A \code{data.frame}, usually the \code{results} element of \code{\link{compute_elo}}, containing match data with Elo or WElo ratings before each match.
  }
  \item{start_date}{
    Character string; the start date of the tournament (e.g., the date of the Round of 128 for Grand Slams or the Round of 64 for Masters 1000). Used to filter the tournament matches.
  }
  \item{Serie}{
    Character string specifying the tournament type: either \code{"Grand Slam"} or \code{"Masters 1000"}. Default is \code{"Grand Slam"}.
  }
  \item{WELO}{
    Logical; if \code{TRUE}, the function uses WElo ratings instead of standard Elo ratings. Default is \code{FALSE}.
  }
}
\details{
The function verifies that the specified \code{Serie} and \code{start_date} correspond to a valid tournament.
It then extracts the matches for each round, in chronological order, and assembles ordered tables that represent the tournament bracket.

For each round, the function includes:
\itemize{
  \item player identifiers,
  \item Elo or WElo ratings before the match,
  \item match outcomes,
  \item the number of matches each player had played before that round (counted across all matches in the dataset, not just completed ones).
}

\strong{Grand Slam:} the function extracts rounds starting from the Round of 128 (\code{sessantaquattresimi}), then 64, 32, 16, quarterfinals, and semifinals.
\strong{Masters 1000:} the function extracts rounds starting from the Round of 64 (\code{trentaduesimi}), then 32, 16, quarterfinals, and semifinals.

Final matches are not explicitly returned as a separate table; the semifinals list covers the players reaching the final.
}
\value{
A list of ordered data frames representing the rounds of the specified tournament:
\itemize{
  \item{\code{sessantaquattresimi_ord}}{(Grand Slam only) Round of 128.}
  \item{\code{trentaduesimi_ord}}{Round of 64.}
  \item{\code{sedicesimi_ord}}{Round of 32.}
  \item{\code{ottavi_ord}}{Round of 16.}
  \item{\code{quarti_ord}}{Quarterfinals.}
  \item{\code{semifinali}}{Semifinals (the winners are the finalists).}
}
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. \cr
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127–138. \cr
Angelini, G., Candila, V., & De Angelis, L. (2022). Weighted Elo rating for tennis match predictions. \emph{European Journal of Operational Research}, 297(1), 120–132. \cr
De Angelis, Luca & Fontana, Saverio. (2024). Monte Carlo meets Wimbledon: Elo-based simulations for predicting tennis tournament winners. 10.2139/ssrn.5027380. \cr
\url{https://github.com/SaveFonta}
}
\seealso{
  \code{\link{clean_data}}, \cr
  \code{\link{compute_elo}}, \cr
  \code{\link{simulate_tournament}}
}
\author{Saverio Fontana}
\examples{
## Example for a Grand Slam
## Assume 'estimation' is the result of compute_elo()
start_date <- "2023-01-16"
Serie <- "Grand Slam"
fit <- estimation$results

bracket <- define_tournament(fit, start_date, Serie, WELO = TRUE)

## Ordered table for the Round of 128
round_of_128 <- bracket$sessantaquattresimi_ord
}
\keyword{Elo}
\keyword{WElo}
\keyword{tennis}
\keyword{tournament}
\keyword{bracket}
