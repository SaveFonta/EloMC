\name{clean_data}
\alias{clean_data}
\title{Clean and Preprocess Tennis Match Data for Elo and WElo Rating Computation}
\description{
The \code{clean_data} function prepares a tennis match dataset for Elo or WElo rating analysis.
It validates input data, removes rows with missing essential values, standardizes column names,
and optionally calculates additional match dominance statistics for WElo ratings.
This ensures compatibility with \code{\link{compute_elo}} and related functions.
}
\usage{
clean_data(x, WELO = FALSE, verbose = TRUE, add_id = FALSE)
}
\arguments{
  \item{x}{
A \code{data.frame} containing tennis match data, typically obtained using \code{\link{merged_tennis_data}}.
Must include the following columns: \code{Location}, \code{Tournament}, \code{Date}, \code{Series},
\code{Surface}, \code{Round}, \code{Winner}, \code{Loser}, and \code{Comment}.
Depending on the dataset, one of \code{ATP} or \code{WTA} must also be present.
If any required columns are missing, the function stops with an error.
}
  \item{WELO}{
Logical; default \code{FALSE}.
If \code{TRUE}, the function computes match dominance statistics — fractions of games and sets won by both players —
used for Weighted Elo (WElo) rating estimation (Angelini et al., 2022).
}
  \item{verbose}{
Logical; default \code{TRUE}.
If \code{TRUE}, the function prints detailed information, progress messages, and cleaning statistics to the console.
}
  \item{add_id}{
Logical; default \code{FALSE}.
If \code{TRUE}, a unique identifier column \code{id} is added to the cleaned dataset.
}
}
\details{
The function performs the following steps:
\enumerate{
    \item \strong{Validation:} Ensures the input is a non-empty \code{data.frame} with all required columns.
    \item \strong{Column Renaming:} Renames \code{Tier} to \code{Series}, if present, to maintain consistency.
    \item \strong{Row Removal:} Eliminates rows with missing values in essential columns, recording how many rows were removed and why.
    \item \strong{WElo Statistics:} If \code{WELO = TRUE}, computes fractions of games and sets won by both winner and loser, provided the dataset contains set-level information (\code{W1–W5, L1–L5, Wsets, Lsets}).
    \item \strong{Sorting and ID:} Orders matches by date and round, and optionally adds a match identifier column.
}
This ensures the dataset is properly structured for Elo or WElo computation with \code{\link{compute_elo}}.
}
\value{
A cleaned \code{data.frame} containing tennis match data, ready for Elo or WElo rating computation.
If \code{WELO = TRUE}, additional columns are included:
\itemize{
  \item \code{f_g_Winner}, \code{f_g_Loser}: Fraction of games won by winner and loser.
  \item \code{f_s_Winner}, \code{f_s_Loser}: Fraction of sets won by winner and loser.
}
If \code{add_id = TRUE}, an \code{id} column is added as the first column.
}
\references{
Angelini, G., Candila, V., & De Angelis, L. (2022).
\emph{Weighted Elo rating for tennis match predictions}.
European Journal of Operational Research, 297(1), 120–132. \cr
\url{https://github.com/SaveFonta}
}
\author{
Saverio Fontana
}
\note{
This function is a crucial preprocessing step for Elo and WElo analyses.
If essential columns are missing, execution stops with an error.
}
\seealso{
\code{\link{compute_elo}}, \cr
\code{\link{merged_tennis_data}}
}
\examples{
## Example dataset
tennis_data <- data.frame(
  Location = c("Paris", "London"),
  Tournament = c("Roland Garros", "Wimbledon"),
  Date = as.Date(c("2021-06-01", "2021-07-01")),
  Series = c("Grand Slam", "Grand Slam"),
  Surface = c("Clay", "Grass"),
  Round = c("Final", "Final"),
  Winner = c("Player A", "Player B"),
  Loser = c("Player C", "Player D"),
  Comment = c("", "")
)

## Clean data without WElo
basic_cleaned <- clean_data(tennis_data)

## Clean data with WElo statistics and ID column
advanced_cleaned <- clean_data(tennis_data, WELO = TRUE, add_id = TRUE)
}
\keyword{datasets}
