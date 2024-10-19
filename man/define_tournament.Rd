\name{define_tournament}
\alias{define_tournament}
\title{Define Tournament Structure and Extract Data for Each Round}
\description{
  The \code{define_tournament} function creates an ordered table for a specific tennis tournament, ensuring the correct structure for each round. It extracts match data from a dataset (output from \code{\link{compute_elo}}) and prepares it for simulating the tournament using the \code{\link{simulate_tournament}} function.
}
\usage{
define_tournament(X, start_date, Serie = "Grand Slam", WELO = FALSE)
}
\arguments{
  \item{X}{A data frame produced by the \code{\link{compute_elo}} function, containing cleaned match data with updated Elo or WElo ratings.}
  \item{start_date}{The start date of the tournament (e.g., the first day of the Round of 128 for Grand Slams, or the first day of the Round of 64 for Masters 1000). This is used to filter relevant matches.}
  \item{Serie}{A character string indicating the tournament series, either \code{"Grand Slam"} or \code{"Masters 1000"}. Default is \code{"Grand Slam"}.}
  \item{WELO}{A logical value. If \code{TRUE}, the function uses the WElo ratings for players; if \code{FALSE}, it uses the standard Elo ratings. Default is \code{FALSE}.}
}
\details{
  The function filters the dataset based on the specified \code{Serie} and \code{start_date}. It then extracts match data for each round of the tournament:

  \describe{
    \item{Grand Slam:}{Rounds include 128, 64, 32, 16, quarterfinals, semifinals, and finals.}
    \item{Masters 1000:}{Rounds include 64, 32, 16, quarterfinals, semifinals, and finals.}
  }

  For each round, the function extracts Elo or WElo ratings (depending on the \code{WELO} argument) and tracks the number of previous matches played by each player during the tournament, storing this information in separate data frames for each round. These data frames can then be used in the \code{\link{simulate_tournament}} function.
}
\value{
  A list of data frames containing the ordered tables for each round of the tournament:
  \itemize{
    \item{\code{sessantaquattresimi_ord}:}{Ordered table for the Round of 128, including player names, ratings, match outcomes, and previous match counts.}
    \item{\code{trentaduesimi_ord}:}{Ordered table for the Round of 64.}
    \item{\code{sedicesimi_ord}:}{Ordered table for the Round of 32.}
    \item{\code{ottavi_ord}:}{Ordered table for the Round of 16.}
    \item{\code{quarti_ord}:}{Ordered table for the quarterfinals.}
    \item{\code{semifinali}:}{Ordered table for the semifinals.}
    \item{\code{finale_ord}:}{Ordered table for the finals.}
  }
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. \cr
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127-138. \cr
Angelini, G., Candila, V., & De Angelis, L. (2022). Weighted Elo rating for tennis match predictions. \emph{European Journal of Operational Research}, 297(1), 120-132.s \cr
\url{https://github.com/SaveFonta}
}
\seealso{
  \code{\link{clean_data}}\cr
  \code{\link{simulate_tournament}}\cr
  \code{\link{compute_elo}}
}
\author{
Saverio Fontana
}
\examples{
# Example of using the function for extracting the ordered table for a Grand Slam starting 2023-01-16
# Assume 'estimation' is the result of compute_elo() function
start_date <- "2023-01-16"
Serie <- "Grand Slam"
fit <- estimation$results

ordered_table <- define_tournament(fit, start_date, Serie, WELO = TRUE)

# Obtain the ordered table for the Round of 128
round_of_128 <- ordered_table$sessantaquattresimi_ord
}

\keyword{Elo}
\keyword{tennis}
\keyword{ratings}

