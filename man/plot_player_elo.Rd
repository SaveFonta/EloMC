\name{plot_player_elo}
\alias{plot_player_elo}
\title{Plot Time Series of Elo Ratings for a Player Across Different Surfaces}
\description{
This function plots the time series of the Elo ratings of a specific player across different surfaces (such as standard, clay, grass, and hard courts).
It allows the user to specify a time range by providing start and end dates. The first point plotted is the Elo rating after the player's first match.
}
\usage{
plot_player_elo(player_name, tables, start_date = NULL, end_date = NULL)
}
\arguments{
  \item{player_name}{A string specifying the name of the player whose Elo ratings will be plotted. The name should match the format used in the Elo tables (e.g., "Sinner J.").}

  \item{tables}{A named list of data frames where each data frame represents Elo ratings for different surfaces (e.g., standard, clay, grass, hard courts). Each data frame should contain the following columns:
  \code{Date}, \code{P_i}, \code{P_j}, \code{Elo_i_after_match}, and \code{Elo_j_after_match}.}

  \item{start_date}{(Optional) A string specifying the earliest date from which to plot Elo ratings (in "YYYY-MM-DD" format). If not provided, the plot will include data from the player's first recorded match.}

  \item{end_date}{(Optional) A string specifying the latest date until which to plot Elo ratings (in "YYYY-MM-DD" format). If not provided, the plot will include all matches up to the last recorded match.}
}
\details{
This function generates a time series plot for the specified player's Elo ratings across various surfaces. It combines the time series data from different surface types (e.g., standard Elo, clay-adjusted Elo, grass-adjusted Elo, and hard-adjusted Elo) and plots them on a single chart. The user can specify a date range by using the \code{start_date} and \code{end_date} parameters.

The Elo tables should be generated using the \code{compute_elo} function. If no valid matches are found for the player in the specified date range, the function will display a message indicating that no valid data was found.
}
\value{
A plot of the player's Elo ratings over time for different surfaces is generated. No value is returned.
}
\examples{
# Assume that Table, Table_Clay, Table_Grass, and Table_Hard are preloaded
# Elo tables obtained using the `compute_elo` function

# Example with no date filtering (plot all available matches)
tables_list <- list("Standard Elo" = Table,
                    "Clay-adjusted Elo" = Table_Clay,
                    "Grass-adjusted Elo" = Table_Grass,
                    "Hard-adjusted Elo" = Table_Hard)
plot_player_elo("Sinner J.", tables_list)

# Example with a specific start date
plot_player_elo("Sinner J.", tables_list, start_date = "2023-01-01")

# Example with both start and end dates
plot_player_elo("Sinner J.", tables_list, start_date = "2023-01-01", end_date = "2023-06-30")

# Example with only an end date
plot_player_elo("Sinner J.", tables_list, end_date = "2023-06-30")
}
\author{
Saverio Fontana
}

