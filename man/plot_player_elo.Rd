\name{plot_player_elo}
\alias{plot_player_elo}
\title{Plot Time Series of Elo Ratings for One or Multiple Players}
\description{
This function plots the time series of Elo ratings for either:
\itemize{
  \item A single player across multiple Elo tables (e.g., different surfaces such as standard, clay, grass, hard).
  \item Multiple players from the same Elo table (e.g., standard Elo rating for a group of players).
}
The user may specify a date range with \code{start_date} and \code{end_date}. Before the first recorded match, the Elo is assumed to remain constant at 1500 until the first available observation.
}
\usage{
plot_player_elo(player_names, tables, start_date = NULL, end_date = NULL)
}
\arguments{
  \item{player_names}{A character string (for a single player) or a character vector (for multiple players). Names must match the format in the Elo tables (e.g., "Sinner J.").}

  \item{tables}{
    If \code{length(player_names) == 1}, this should be a named list of Elo tables where each table corresponds to a surface or adjustment (e.g., standard, clay, grass, hard).

    If \code{length(player_names) > 1}, this should be a single Elo table (data frame) containing Elo ratings for all players.

    Each table must include columns: \code{Date}, \code{P_i}, \code{P_j}, \code{Elo_i_after_match}, \code{Elo_j_after_match}.
  }

  \item{start_date}{(Optional) A string specifying the earliest date to include in the plot ("YYYY-MM-DD"). If provided, Elo is fixed at 1500 until the first recorded match.}

  \item{end_date}{(Optional) A string specifying the latest date to include in the plot ("YYYY-MM-DD").}
}
\details{
\strong{Single player:} The function combines Elo ratings from multiple surfaces/tables into one plot, each shown in a different color.

\strong{Multiple players:} The function extracts Elo ratings from the same table and plots them together for comparison. Each player is represented with a separate color.

If no matches are found in the specified range, the function outputs a message instead of a plot.
}
\value{
A time series plot of Elo ratings. No value is returned.
}
\examples{
# --- Case 1: Single player across multiple surfaces ---
tables_list <- list("Standard Elo" = Table,
                    "Clay-adjusted Elo" = Table_Clay,
                    "Grass-adjusted Elo" = Table_Grass,
                    "Hard-adjusted Elo" = Table_Hard)

plot_player_elo("Sinner J.", tables_list)

# With start and end dates
plot_player_elo("Sinner J.", tables_list, start_date = "2023-01-01", end_date = "2023-06-30")


# --- Case 2: Multiple players from one Elo table ---
# Assume Table contains standard Elo ratings for all players
plot_player_elo(c("Sinner J.", "Alcaraz C."), Table, start_date = "2023-01-01")
}
\author{
Saverio Fontana
}
