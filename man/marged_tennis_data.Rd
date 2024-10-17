\name{merged_tennis_data}
\alias{merged_tennis_data}
\title{Merge Tennis Match Data from Multiple Years}
\description{
  The \code{merged_tennis_data} function retrieves and merges ATP or WTA tennis match datasets from the years 2013 to 2024 based on the specified gender. It ensures that the \code{welo} package is installed and removes unnecessary columns for streamlined data analysis.
}
\usage{
  merged_tennis_data(gender = "ATP")
}
\arguments{
  \item{gender}{
    A character string indicating the gender of the tennis players.
    It can be either \code{"ATP"} for male players or \code{"WTA"} for female players. The default value is \code{"ATP"}.
  }
}
\details{
  This function uses the \code{\link[welo]{tennis_data}} function from the \code{welo} package, authored by V. Candila, to load ATP or WTA tennis match datasets for each year from 2013 to 2024, depending on the specified gender. The data is combined into a single data frame, and unnecessary columns (e.g., \code{SJW}, \code{SJL}, \code{EXW}, \code{EXL}, \code{LBW}, \code{LBL}) are removed to prepare the data for further analysis.

  The resulting data frame includes the following columns:
  \itemize{
    \item \strong{ATP/WTA}: The association of the tournament (either ATP for men or WTA for women).
    \item \strong{Location}: The location of the tournament.
    \item \strong{Tournament}: The name of the tournament.
    \item \strong{Date}: The date of the match.
    \item \strong{Series}: The category of the tournament.
    \item \strong{Court}: The type of court.
    \item \strong{Surface}: The surface type.
    \item \strong{Round}: The round of the match.
    \item \strong{Best of}: The best-of format of the match.
    \item \strong{Winner}: The name of the winning player.
    \item \strong{Loser}: The name of the losing player.
    \item \strong{WRank}: The ranking of the winner.
    \item \strong{LRank}: The ranking of the loser.
    \item \strong{WPts}: Points awarded to the winner.
    \item \strong{LPts}: Points awarded to the loser.
    \item \strong{W1, L1, ..., W5, L5}: Scores for each set of the match.
    \item \strong{Wsets}: Total sets won by the winner.
    \item \strong{Lsets}: Total sets won by the loser.
    \item \strong{Comment}: Additional comments regarding the match.
    \item \strong{B365W, B365L}: Betting odds for the winner and loser from Bet365.
    \item \strong{PSW, PSL}: Betting odds for the winner and loser from Pinnacle Sports.
    \item \strong{MaxW, MaxL}: Maximum odds for the winner and loser.
    \item \strong{AvgW, AvgL}: Average odds for the winner and loser.
  }

  It is important to note that the output data frame should be processed using the \code{\link{clean_data}} function from the \code{EloMC} package before performing any further analysis, such as computing Elo ratings. This function is optimized for preparing tennis match data for analysis and subsequent calculations.
}
\value{
  A data frame containing the merged ATP or WTA tennis match data from 2013 to 2024, with specific columns removed.
}
\references{
  Angelini, Giovanni, Vincenzo Candila, and Luca De Angelis. "Weighted Elo rating for tennis match predictions." \emph{European Journal of Operational Research} 297.1 (2022): 120-132.
}
\author{
  Saverio Fontana
}
\note{
  This function is essential for preparing tennis match data for analysis and computation of Elo ratings.
}
\seealso{
  \code{\link[welo]{tennis_data}}  % Link to the tennis_data function in the welo package
  \code{\link{clean_data}}  % Link to the clean_data function
}
\examples{
  ## Example usage
  tennis_data_merged <- merged_tennis_data(gender = "WTA")  # For women's tennis data
  cleaned_data <- clean_data(tennis_data_merged)
}
\keyword{data}
