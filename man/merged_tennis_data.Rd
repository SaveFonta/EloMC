\name{merged_tennis_data}
\alias{merged_tennis_data}
\title{Download and Merge Tennis Match Data Across Multiple Years}
\description{
  The \code{merged_tennis_data} function downloads and merges ATP or WTA tennis match datasets for a specified range of years.
  The data comes from the archive of \url{https://www.tennis-data.co.uk} and includes tournaments at the \emph{ATP250, ATP500, ATP1000}, and \emph{Grand Slam} levels.
}
\usage{
  merged_tennis_data(gender = "ATP", end_year = 2025, start_year = 2013)
}
\arguments{
  \item{gender}{
    A character string specifying which circuit to download:
    \code{"ATP"} for the men’s circuit (default) or \code{"WTA"} for the women’s circuit.
  }
  \item{end_year}{
    An integer specifying the last year to include. Default: \code{2025}.
  }
  \item{start_year}{
    An integer specifying the first year to include. Default: \code{2013}.
    If less than 2007, a warning is issued because data may not be available.
  }
}
\details{
  This function relies on \code{\link[welo]{tennis_data}} from the \code{welo} package (V. Candila) to download yearly ATP/WTA datasets.
  The data is merged into a single \code{data.frame}, and obsolete columns (e.g., \code{SJW}, \code{SJL}, \code{EXW}, \code{EXL}, \code{LBW}, \code{LBL}) are removed depending on the year.

  The resulting dataset may include the following variables:
  \itemize{
    \item \strong{ATP/WTA}: Circuit of the tournament.
    \item \strong{Location}: Tournament location.
    \item \strong{Tournament}: Tournament name.
    \item \strong{Date}: Match date.
    \item \strong{Series}: Tournament category.
    \item \strong{Court}: Court type (indoor/outdoor).
    \item \strong{Surface}: Surface type (hard, clay, grass, carpet).
    \item \strong{Round}: Round of the match.
    \item \strong{Best of}: Match format (3 or 5 sets).
    \item \strong{Winner}, \strong{Loser}: Match winner and loser.
    \item \strong{WRank}, \strong{LRank}: Player rankings.
    \item \strong{WPts}, \strong{LPts}: Player points.
    \item \strong{W1, L1, ..., W5, L5}: Set-by-set scores.
    \item \strong{Wsets}, \strong{Lsets}: Number of sets won.
    \item \strong{Comment}: Additional notes.
    \item \strong{B365W}, \strong{B365L}: Bet365 odds.
    \item \strong{PSW}, \strong{PSL}: Pinnacle odds.
    \item \strong{MaxW}, \strong{MaxL}: Maximum available odds.
    \item \strong{AvgW}, \strong{AvgL}: Average odds.
  }

  Before further analysis (e.g., Elo rating computation), it is recommended to process the output using the \code{\link{clean_data}} function from the \code{EloMC} package.
}
\value{
  A \code{data.frame} containing merged ATP or WTA tennis match data for the specified range of years, with obsolete columns removed.
}
\references{
  \url{https://github.com/SaveFonta} \cr
  \url{https://www.tennis-data.co.uk}
}
\author{
  Saverio Fontana
}
\note{
  This function is intended as a data preparation step for tennis match analysis and Elo rating computation.
}
\seealso{
  \code{\link[welo]{tennis_data}} \cr
  \code{\link{clean_data}}
}
\examples{
  ## Example: Download and clean WTA data from 2015 to 2020
  wta_data <- merged_tennis_data(gender = "WTA", start_year = 2015, end_year = 2020)
  wta_cleaned <- clean_data(wta_data)
}
\keyword{datasets}
