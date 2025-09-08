\name{GS_sim}
\alias{GS_sim}
\title{
Simulate Grand Slam Tennis Tournaments for Betting Analysis
}
\description{
Simulates all Grand Slam tennis tournaments from 2019 to 2025 using Elo-based probabilities.
It produces estimated probabilities of winning for each player and is designed for betting or predictive analysis.
}
\usage{
GS_sim(X, tournament = NA, n = 1000, WELO = FALSE)
}
\arguments{
  \item{X}{
    A dataset containing player information, match history, and Elo ratings. This is used to initialize the tournament simulation.
  }
  \item{tournament}{
    Character string specifying which Grand Slam tournament to simulate. Options include
    "Australian Open", "Roland Garros", "Wimbledon", "US Open".
    If \code{NA}, all Grand Slams from 2019 to 2025 are simulated.
  }
  \item{n}{
    Number of Monte Carlo simulations to run per tournament. Defaults to 1000.
  }
  \item{WELO}{
    Logical flag indicating whether to use weighted Elo ratings (TRUE) or standard Elo ratings (FALSE). Defaults to FALSE.
  }
}
\details{
This function runs simulations for each Grand Slam specified, computing winning probabilities for all players.
It relies on \code{define_tournament} to set up each tournament draw and \code{simulate_tournament}
to simulate matches and calculate probabilities.

Grand Slam tournaments simulated include:
\itemize{
  \item Australian Open (2019–2025)
  \item Roland Garros (2019–2025)
  \item Wimbledon (2019–2025)
  \item US Open (2019–2025)
}
A fixed random seed of 123 is recommended for reproducibility.
}
\value{
A list of lists where each element corresponds to a tournament.
\item{<tournament name>}{A list of player win probabilities for that tournament.}
}
\references{
De Angelis, Luca & Fontana, Saverio (2024).
\emph{Monte Carlo meets Wimbledon: Elo-based simulations for predicting tennis tournament winners}}
\author{
Saverio Fontana
}
\note{
This function is intended for research and betting simulation purposes.
Ensure that all input data (\code{X}) is preprocessed correctly and contains valid Elo ratings.
}
\seealso{
\code{\link{define_tournament}}, \code{\link{simulate_tournament}}
}
\examples{
## Example usage:
## Assume 'player_data' is a dataset with Elo ratings
## and past matches. Simulate Wimbledon 2024 with 500 iterations.

# Simulate Wimbledon 2024
results <- GS_sim(player_data, tournament = "Wimbledon", n = 500)

# Simulate all Grand Slams from 2019–2025 using weighted Elo
results_all <- GS_sim(player_data, WELO = TRUE)
}
\keyword{simulation}
\keyword{tennis}
\keyword{Monte Carlo}
\concept{Elo ratings}
\concept{Grand Slam tournaments}
