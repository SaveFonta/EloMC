# EloMC: Tournament Analysis Using Elo Ratings

The **EloMC** package is designed for analyzing and simulating tournament results using the Elo rating system. It provides functions to assess betting strategies and analyze performance in various tournaments.

## Details

This package depends on the following packages:

- **welo**: Needed for various Elo-related calculations and functions.
- **Rlab**: Provides the `rbern` function for generating random Bernoulli variables.

Please ensure you have these packages installed before using **EloMC**. You can install them from CRAN with the following commands:

```r
install.packages("welo")
install.packages("Rlab")
```

You can install **EloMC** directly from GitHub using the `remotes` package:

```r
install.packages("remotes")
remotes::install_github("SaveFonta/EloMC")
```

## Author

Your Name Here

## License

MIT

## Functions

- `merged_tennis_data(data1, data2)`: 
  Merges two datasets containing tennis match data into a single cohesive dataset.
  - **Parameters**:
    - `data1`: The first dataset to merge.
    - `data2`: The second dataset to merge.
  - **Returns**: A merged dataset containing all relevant tennis match data.

- `clean_data(dataset)`: 
  Cleans the given dataset by removing any unnecessary or problematic entries.
  - **Parameters**:
    - `dataset`: The dataset to be cleaned.
  - **Returns**: A cleaned dataset, free of NA values and outliers.

- `compute_elo(player_ratings, match_results)`: 
  Computes the updated Elo ratings for players based on their match results.
  - **Parameters**:
    - `player_ratings`: A data frame containing the current Elo ratings of players.
    - `match_results`: A data frame containing the results of the matches.
  - **Returns**: A data frame with updated Elo ratings for each player.

- `define_tournament(X, start_date, Serie)`: 
  Defines a tournament structure based on the input data and specified start date.
  - **Parameters**:
    - `X`: The input dataset for defining the tournament.
    - `start_date`: The starting date of the tournament.
    - `Serie`: The series/type of the tournament (e.g., "Grand Slam").
  - **Returns**: A structured tournament object containing matchups and player information.

- `simulate_tournament(tournament_data, n)`: 
  Simulates a tournament based on the defined tournament data and number of simulations.
  - **Parameters**:
    - `tournament_data`: The structured tournament object.
    - `n`: The number of simulations to run.
  - **Returns**: A list of results from the simulated tournament.

- `Betting_function(probab, Bookmakers, q_values, r_values, q_max, r_max)`: 
  Evaluates betting strategies based on calculated probabilities and bookmaker odds.
  - **Parameters**:
    - `probab`: The probabilities of outcomes from the simulations.
    - `Bookmakers`: A dataset of bookmaker odds.
    - `q_values`: A vector of threshold values for betting.
    - `r_values`: A vector of odds from bookmakers.
    - `q_max`: Maximum threshold value for betting.
    - `r_max`: Maximum odds value.
  - **Returns**: A list of betting outcomes based on the evaluated strategies.

- `Results_betting(X, n = 1000, tournament = NA, Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3), R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3), Q_max = 1000, R_max = 1000)`: 
  Simulates betting outcomes for various tournaments and returns the results.
  - **Parameters**:
    - `X`: Input data for tournament simulations.
    - `n`: Number of simulations to run (default is 1000).
    - `tournament`: Name of the tournament (e.g., "Australian Open").
    - `Q`: A vector of betting thresholds.
    - `R`: A vector of odds.
    - `Q_max`: Maximum threshold value for betting (default is 1000).
    - `R_max`: Maximum odds value (default is 1000).
  - **Returns**: A list of betting results for each tournament.

- `Results_balance(x)`: 
  Computes the balance, total number of bets, number of won bets, and ROI from the results provided.
  - **Parameters**:
    - `x`: A list of results containing balance, number of bets, and number of won bets.
  - **Returns**: A data frame with calculated statistics.


