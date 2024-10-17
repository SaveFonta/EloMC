\name{define_tournament}
\alias{define_tournament}
\title{Define Tournament Structure and Extract Data for Each Round}
\description{
  The \code{define_tournament} function extracts match data from a cleaned dataset (output from \code{clean_data}) for a specified tournament and round structure. This function enables users to select rounds for either a Grand Slam or Masters 1000 tournament and returns detailed match data for each round.
}
\usage{
define_tournament(X, start_date, Serie = "Grand Slam", WELO = FALSE)
}
\arguments{
  \item{X}{A data frame output from the \code{clean_data} function, containing cleaned match data.}
  \item{start_date}{A date specifying the start of the tournament, used to filter the relevant matches.}
  \item{Serie}{A character string specifying the tournament series: either \code{"Grand Slam"} or \code{"Masters 1000"}. Default is \code{"Grand Slam"}.}
  \item{WELO}{A logical value. If \code{TRUE}, the function includes the WELO rating values for each player. If \code{FALSE}, the standard Elo rating values are used. Default is \code{FALSE}.}
}
\details{
  The function first filters the dataset for the selected tournament type (\code{Serie}) and \code{start_date}. It then extracts match data for each tournament round:

  \describe{
    \item{Grand Slam:}{Includes rounds of 128, 64, 32, 16, quarterfinals, semifinals, and finals.}
    \item{Masters 1000:}{Includes rounds of 64, 32, 16, quarterfinals, semifinals, and finals.}
  }

  For each round, the function calculates the Elo or WELO ratings (based on the \code{WELO} parameter) and the number of previous matches played by each player in the season, storing this information in separate data frames for each round. The resulting data frames can then be used to simulate the tournament rounds using the \code{simulate_tournament} function.
}
\value{
  Returns a list of data frames containing:
  \itemize{
    \item{sessantaquattresimi_ord:}{Match data for the round of 128, including player names, ratings, outcomes, and previous match counts.}
    \item{trentaduesimi_ord:}{Ordered table for the round of 64.}
    \item{sedicesimi_ord:}{Ordered table for the round of 32.}
    \item{ottavi_ord:}{Ordered table for the round of 16.}
    \item{quarti_ord:}{Ordered table for the quarterfinals.}
    \item{semifinali:}{Ordered table for the semifinals.}
  }
}
\examples{
# Example of using the function with a dataset
# Assume cleaned_data is the result of clean_data() function
start_date <- "2023-01-16"
Serie <- "Grand Slam"
result <- define_tournament(cleaned_data, start_date, Serie, WELO = TRUE)

#Obtain the ordered table for the round of 128
round_of_128 <- result$sessantaquattresimi_ord
}
\seealso{
  \code{\link{clean_data}}: Clean and preprocess tennis match data for analysis.
  \code{\link{simulate_tournament}}: Simulate tournament outcomes based on player ratings.
  \code{\link{compute_elo}}: Compute Elo ratings for players based on match results.
}
\author{
Saverio Fontana
}

