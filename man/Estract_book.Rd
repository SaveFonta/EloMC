\name{Estract_book}
\alias{Estract_book}
\title{
Extract Betting Odds from Excel Sheets for Grand Slam Tournaments
}
\description{
Reads betting odds from an Excel file for specified Grand Slam tennis tournaments (2019–2025).
Each tournament sheet must follow the format specified in the Tennis_odds_scraping GitHub repository.
}
\usage{
Estract_book(Nome_Excel, tournament = NA)
}
\arguments{
  \item{Nome_Excel}{
    Character string with the path to the Excel file containing the betting odds.
    Each sheet must correspond to a Grand Slam tournament and follow the GitHub formatting guidelines:
    \url{https://github.com/SaveFonta/Tennis_odds_scraping}.
  }
  \item{tournament}{
    Character string specifying the tournament to extract. Options are
    "Australian Open", "Roland Garros", "Wimbledon", "US Open".
    If \code{NA}, all Grand Slam tournaments from 2019 to 2025 are extracted.
  }
}
\details{
This function iterates over the specified Grand Slam tournaments, reads the corresponding sheet
from the Excel file, renames the 7th column to \code{"Player"} for consistency,
and returns a list of data frames. Each data frame corresponds to one tournament's odds.

Tournaments covered include:
\itemize{
  \item Australian Open (2019–2025)
  \item Roland Garros (2019–2025)
  \item Wimbledon (2019–2025)
  \item US Open (2019–2025)
}
Column names and formatting must match the standard used in the GitHub repository to ensure correct extraction.
}
\value{
A list where each element is a data frame of betting odds for a tournament.
\item{<tournament name>}{Data frame containing the betting odds and player names for that tournament.}
}
\references{
GitHub repository for Excel formatting and scraping guidelines:
\url{https://github.com/SaveFonta/Tennis_odds_scraping}
}
\author{
Saverio Fontana
}
\note{
Make sure the Excel sheets are correctly formatted according to the GitHub repository.
The function will overwrite the 7th column name as "Player" to standardize the output.
}
\seealso{
\code{\link{read_excel}}
}
\examples{
## Example usage:
## Extract all Grand Slam betting odds from the Excel file
book_all <- Estract_book("tennis_odds.xlsx")

## Extract only Wimbledon odds
book_wimbledon <- Estract_book("tennis_odds.xlsx", tournament = "Wimbledon")

## Access the data frame for Wimbledon 2024
wimbledon_2024 <- book_wimbledon[["Wimbledon 2024"]]
}
\keyword{betting}
\keyword{tennis}
\keyword{Excel}
\concept{Grand Slam odds}
