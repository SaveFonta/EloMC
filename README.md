# EloMC: Elo Monte Carlo tennis simulation 

**EloMC** is an R library that provides a comprehensive framework for simulating and analyzing tennis results using Elo (or WElo) ratings. This library includes functions for:

- Downloading tennis match data from [www.tennis-data.co.uk](www.tennis-data.co.uk)
- Fitting Elo ratings for each player
- Simulating tournament outcomes

With **EloMC**, users can evaluate the relationship between Elo-simulated probabilities and actual bookmaker odds, allowing for the identification of potentially profitable betting opportunities in major Grand Slam tournaments.

To obtain betting odds, visit the [Tennis Scraping GitHub Repository](https://github.com/SaveFonta/Tennis_odds_scraping).


## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Functions](#functions)
  - [merged_tennis_data](#merged_tennis_data)
  - [clean_data](#clean_data)
  - [compute_elo](#compute_elo)
  - [define_tournament](#define_tournament)
  - [simulate_tournament](#simulate_tournament)
  - [Betting_function](#betting_function)
  - [Results_betting](#results_betting)
  - [Results_balance](#results_balance)
- [Examples](#example)
- [Contributing](#contributing)
- [License](#license)

## Features

- Simulates tournament outcomes using Elo ratings.
- Evaluates betting opportunities based on Elo probabilities and bookmaker odds.
- Provides a detailed summary of betting results for each tournament.
- Calculates overall betting performance, including balance and ROI.

## Installation

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

### Functions

#### `merged_tennis_data`
#### `clean_data`
#### `compute_elo'
#### 'define_tournament'
#### `simulate_tournament`
#### 'Betting_function'
#### 'Results_betting'
#### 'Results_balance'

## Example
This section provides an example of how to use the **EloMC** library for simulating tennis results and analyzing betting opportunities.

```R
# Download tennis data for ATP players 
X <- merged_tennis_data()

# Clean the data frame
X.clean <- clean_data(X)

# Fit the Standard Elo
X.clean.fit <- compute_elo(X.clean)
Table <- X.clean.fit$results

# Set the seed for reproducibility (optional)
# set.seed()

# Define the ordered tables of the Australian Open 2024
AO24 <- define_tournament(Table, start_date = "2024-01-14")

# Simulate the AO2024 starting from the round of 128 100000 times
round_of_128 <- AO24$sessantaquattresimi_ord
sim_results <- simulate_tournament(round_of_128, sim = 100000)

# BETTING:
# Obtain betting results and simulate Grand Slams from 2019 to 2024, simulating 100000 times each tournament.
# Ensure the correct Excel name and path are specified
bet_list <- Results_betting(Table, Excel_name = "Database Tennis.xlsx", n = 100000)

# Visualize the total betting results 
balance <- Results_balance(bet_list)
```

## Contributing
The package draws inspiration from V. Candila's work on the "welo" package, utilizing useful functions from it to enhance its functionality. 
Contributions are welcome! If you find bugs or have suggestions for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.


