\name{clean_data}
\alias{clean_data}
\title{Clean and Preprocess Tennis Match Data for Elo Rating Computation}
\description{
The \code{clean_data} function processes a data frame of tennis match data by removing rows with missing values and renaming specific columns as necessary. This preparation is essential for using the \code{\link{compute_elo}} function, which computes Elo ratings for all players involved. When \code{WELO = TRUE}, the function also calculates statistics needed for estimating the WElo rating as proposed by Angelini et al. (2022).
}
\usage{
clean_data(x, WELO = FALSE)
}
\arguments{
  \item{x}{
A data frame obtained using the function \code{\link{merged_tennis_data}}, containing tennis match data. The data frame must include the following columns: \code{Location}, \code{Tournament}, \code{Date}, \code{Series}, \code{Surface}, \code{Round}, \code{Winner}, and \code{Loser}. If any of these required columns are missing, the function will halt and return an error message.
}
  \item{WELO}{
Logical; default is \code{FALSE}. If \code{TRUE}, the function calculates additional statistics on the fraction of games and sets won by both the winner and the loser, enabling estimation of the WElo rating, which factors in match dominance.
}
}
\value{
A cleaned data frame ready for Elo rating computation using \code{\link{compute_elo}}. This data frame includes all necessary match information, with optional WElo rating variables when \code{WELO = TRUE}. If WElo statistics are calculated, they are added as new columns representing the fraction of games and sets won by the winner and loser, supporting WElo rating computation.
}
\details{
The function follows these steps:
\enumerate{
    \item \strong{Validation:} Checks that the input \code{x} is a data frame and verifies the presence of required columns.
    \item \strong{Data Cleaning:} Removes any rows with missing values in the essential columns.
    \item \strong{Column Renaming:} If a column named \code{Tier} exists, it is renamed to \code{Series} to standardize column names.
    \item \strong{WElo Computation:} If \code{WELO = TRUE}, additional calculations are performed to add columns for the fraction of games and sets won by the winner and the loser. These statistics are necessary for WElo rating computation, based on the approach detailed by Angelini et al. (2022).
}
This function is designed to handle the output from \code{\link{merged_tennis_data}}, ensuring compatibility for Elo rating calculations with \code{\link{compute_elo}} and other related functions.
}
\references{
Angelini, Giovanni, Vincenzo Candila, and Luca De Angelis. "Weighted Elo rating for tennis match predictions." \emph{European Journal of Operational Research} 297.1 (2022): 120-132. \url{https://doi.org/10.1016/j.ejor.2021.05.024}
}
\author{
Saverio Fontana
}
\note{
The function is crucial for preparing tennis match data for Elo and WElo rating computations. If any required columns are missing, the function will halt and display an error message.
}
\seealso{
\code{\link{compute_elo}}, \code{\link{merged_tennis_data}}
}
\examples{
# Example usage:
tennis_data <- data.frame(
  Location = c("Paris", "London"),
  Tournament = c("Roland Garros", "Wimbledon"),
  Date = as.Date(c("2021-06-01", "2021-07-01")),
  Series = c("Grand Slam", "Grand Slam"),
  Surface = c("Clay", "Grass"),
  Round = c("Final", "Final"),
  Winner = c("Player A", "Player B"),
  Loser = c("Player C", "Player D")
)

# Clean data with WElo statistics included
cleaned_data <- clean_data(tennis_data, WELO = TRUE)

# cleaned_data now includes columns for Elo and WElo rating computation.
}
\keyword{data}
