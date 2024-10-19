# EloMC: Elo Monte Carlo tennis simulation 

**EloMC** is an R library designed to simulate betting strategies for tennis tournaments using Elo ratings and bookmaker odds. It evaluates the relationship between Elo-simulated probabilities and actual bookmaker odds to identify potentially profitable betting opportunities in major Grand Slam tournaments.

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
- [Examples](#examples)
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



## Usage

After installation, you can load the library and use its functions to evaluate betting strategies for tennis tournaments. Here’s a brief overview of the main functions:

### Functions

#### `merged_tennis_data`

```R
\name{merged_tennis_data}
\alias{merged_tennis_data}
\title{Merge Tennis Match Data from Multiple Years}
\description{
  The \code{merged_tennis_data} function retrieves and merges ATP or WTA tennis match datasets from the years 2013 to 2024 based on the specified gender. The matches are downloaded from the archive of \url{www.tennis-data.co.uk} and involves match data for: ATP250, ATP500, ATP1000 and Grand Slams.
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
 \url{https://github.com/SaveFonta}
}
\author{
  Saverio Fontana
}
\note{
  This function is essential for preparing tennis match data for analysis and computation of Elo ratings.
}
\seealso{
  \code{\link[welo]{tennis_data}} \cr % Link to the tennis_data function in the welo package
  \code{\link{clean_data}}  % Link to the clean_data function
}
\examples{
  ## Example usage
  tennis_data_merged <- merged_tennis_data(gender = "WTA")  # For women's tennis data
  cleaned_data <- clean_data(tennis_data_merged)
}
\keyword{data}
```

###
#### `clean_data`

```R
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
Angelini, Giovanni, Vincenzo Candila, and Luca De Angelis. "Weighted Elo rating for tennis match predictions." \emph{European Journal of Operational Research} 297.1 (2022): 120-132. \cr
 \url{https://github.com/SaveFonta}
}
\author{
Saverio Fontana
}
\note{
The function is crucial for preparing tennis match data for Elo and WElo rating computations. If any required columns are missing, the function will halt and display an error message.
}
\seealso{
\code{\link{compute_elo}}, \cr
\code{\link{merged_tennis_data}}
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
```

### 

#### `compute_elo`

```R
\name{compute_elo}
\alias{compute_elo}
\title{
Calculate Elo Ratings for Tennis Matches with Adjustments
}
\description{
The \code{compute_elo} function calculates Elo ratings for tennis players based on match outcomes, incorporating adjustments for surface types and tournament significance. The function supports standard Elo ratings and variations such as surface-adjusted Elo and surface-specific Elo ratings. It can also generate confidence intervals and custom weights to refine rating computations. The function can optionally calculate WElo ratings, which incorporate additional match performance data. \cr

The function was designed to further simulate tennis tournament and it is the input for the function \code{\link{define_tournament}}.
}
\usage{
compute_elo(x, W = "GAMES", SP = 1500, K = "Kovalchik", s = 0.5,
            CI = FALSE, alpha = 0.05, B = 1000, WELO = FALSE)
}
\arguments{
  \item{x}{
    A data frame (possibly obtained using the function \code{\link{clean_data}}) containing match results with required columns for winners, losers, and match metadata (e.g., \code{Date}, \code{Series}, \code{Round}, \code{Surface}, \code{Comment}).
  }
  \item{W}{
    A character string specifying the type of weighting to apply. Options include \code{"GAMES"} or \code{"SETS"}. The default is \code{"GAMES"}.
  }
  \item{SP}{
    A numeric value representing the starting Elo rating assigned to all players. The default is \code{1500}.
  }
  \item{K}{
    Either a character string or numeric value specifying the K-factor used for Elo rating adjustments. Options include \code{"Kovalchik"}, \code{"Grand_Slam"}, \code{"Surface_Hard"}, \code{"Surface_Clay"}, \code{"Surface_Grass"}, or a numeric K-value. Default is \code{"Kovalchik"}.
  }
  \item{s}{
    A numeric scaling factor applied to adjust K-factor values based on tournament significance. The default is \code{0.5}.
  }
  \item{CI}{
    A logical value indicating whether to calculate confidence intervals for Elo predictions. The default is \code{FALSE}.
  }
  \item{alpha}{
    A numeric value for the significance level when calculating confidence intervals. The default is \code{0.05}.
  }
  \item{B}{
    An integer specifying the number of bootstrap samples used for confidence interval estimation. The default is \code{1000}.
  }
  \item{WELO}{
    A logical value indicating whether to calculate WElo ratings, which incorporate additional match performance data. The default is \code{FALSE}.
  }
}
\details{
The Elo rating system estimates the probability that player i will win against player j in a match at time t as follows:

\deqn{\hat{p}_{i,j}(t) = \frac{1}{1 + 10^{(E_j(t) - E_i(t))/400}}}

The Elo rating for player i is then updated based on match results:

\deqn{E_i(t + 1) = E_i(t) + K_i(t) \left [W_i(t) - \hat{p}_{i,j}(t)\right]}

Where:
\itemize{
  \item W_i(t): Binary indicator (1 if player i wins, 0 otherwise).
  \item K_i(t): Constant factor determining the magnitude of rating adjustments, which can vary based on match significance or surface type.
}

This function also supports surface-specific Elo ratings and WElo ratings, where WElo further incorporates match dominance indicators. Different K-factor options adjust the Elo updates according to match conditions such as surface and tournament significance. For example, adjustments for Grand Slam tournaments or specific surfaces are made by setting parameter s based on match importance:

\deqn{S =
\begin{cases}
1 & \text{if match is significant (e.g., Grand Slam)} \\
s & \text{otherwise, where \( s < 1 \)}
\end{cases}}

The S factor is then used to adjust the rating update:
\deqn{E_i(t + 1) = E_i(t) + K_i(t) \left [W_i(t) - \hat{p}_{i,j}(t)\right] S}


Confidence intervals are optionally computed using a bootstrap approach, and custom weights can be applied to tailor the rating updates.
}
\value{
A list containing:
  \item{results}{A data frame with match results, including Elo ratings before and after each match, and, if requested, WElo ratings and confidence intervals.}
  \item{matches}{A summary of the total number of matches processed.}
  \item{period}{A string indicating the date range covered by the matches.}
  \item{loss}{A matrix with Brier and log-loss values for both Elo and WElo ratings (if \code{WELO = TRUE}).}
  \item{highest_elo}{A string with the player and date corresponding to the highest Elo rating achieved.}
  \item{highest_welo}{If \code{WELO = TRUE}, a string with the player and date corresponding to the highest WElo rating achieved.}
  \item{dataset}{The original input data frame, \code{x}.}
}
\references{
Elo, A. E. (1978). \emph{The Rating of Chess Players, Past and Present}. \cr
Kovalchik, S. A. (2016). Searching for the GOAT of tennis win prediction. \emph{Journal of Quantitative Analysis in Sports}, 12(3), 127-138. \cr
Angelini, Giovanni, Vincenzo Candila, and Luca De Angelis. "Weighted Elo rating for tennis match predictions." \emph{European Journal of Operational Research} 297.1 (2022): 120-132.\cr
 \url{https://github.com/SaveFonta}
}
\seealso{
\code{\link{clean_data}}, \cr
\code{\link{merged_tennis_data}} \cr
\code{\link{define_tournament}}
}
\examples{
# Example usage for estimating the Clay-weighted Elo:
# Assuming `tennis_data` is a data frame obtained using the function merged_tennis_data:
cleaned_data <- clean_data(tennis_data, WELO = FALSE)

estimation <- compute_elo(x = cleaned_data, s=0.5, K = "Surface_Clay", WELO = FALSE)
View(estimation$results)
}
\author{
Saverio Fontana
}
\keyword{Elo}
\keyword{tennis}
\keyword{ratings}
```

#### `define_tournament`

Sets up the structure for a tennis tournament.

**Usage:**
```R
define_tournament(tournament_name)
```

- **Arguments:**
  - `tournament_name`: A string specifying the name of the tournament.

- **Details:**
  This function defines the tournament structure, including the number of rounds and players participating.

- **Value:**
  A list containing the tournament structure.

---

#### `simulate_tournament`

Simulates match outcomes based on player Elo ratings.

**Usage:**
```R
simulate_tournament(tournament_structure, n = 1000)
```

- **Arguments:**
  - `tournament_structure`: The structure of the tournament defined by `define_tournament`.
  - `n`: The number of simulations to run (default = 1000).

- **Details:**
  This function simulates the outcomes of the tournament matches based on the Elo ratings of the players.

- **Value:**
  A data frame with simulated match outcomes and probabilities.

---

#### `Betting_function`

Simulates betting strategy based on Elo probabilities and bookmaker odds.

**Usage:**
```R
Betting_function(X, Book, q_values, r_values, q_max = 1000, r_max = 1000)
```

- **Arguments:**
  - `X`: Data frame containing Elo-based probability estimates.
  - `Book`: Data frame containing bookmaker data.
  - `q_values`: Numeric vector of thresholds for bookmaker probabilities.
  - `r_values`: Numeric vector of thresholds for Elo-to-bookmaker probability ratio.
  - `q_max`: Upper limit for bookmaker probabilities (default = 1000).
  - `r_max`: Upper limit for the betting ratio (default = 1000).

- **Details:**
  This function evaluates a betting strategy based on Elo-simulated probabilities and bookmaker ante-post odds. A bet is placed when the ratio of Elo probability to bookmaker probability exceeds a specified threshold \(r\) and the bookmaker probability is above a certain value \(q\).

- **Value:**
  A data frame summarizing the results for each combination of \(q\) and \(r\), including:
  - `q_Value`: The probability threshold \(q\).
  - `r_Value`: The ratio threshold \(r\).
  - `Balance`: The net profit or loss.
  - `Players`: A list of players bet on.
  - `Num_Bets`: The number of unique bets placed.
  - `N_Bets_won`: The number of winning bets.

---

#### `Results_betting`

Simulates betting results for Grand Slam tournaments from 2019 to 2024.

**Usage:**
```R
Results_betting(X, Excel_name, n = 1000, tournament = NA,
                Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3),
                Q_max = 1000, R_max = 1000)
```

- **Arguments:**
  - `X`: Data frame with Elo-based probability estimates.
  - `Excel_name`: Path to Excel file with bookmaker odds and results.
  - `n`: Number of tournament simulations to perform (default = 1000).
  - `tournament`: Specify which tournament to analyze or set to `NA` for all.
  - `Q`: Numeric vector of thresholds for bookmaker probabilities.
  - `R`: Numeric vector of thresholds for Elo-to-bookmaker probability ratio.
  - `Q_max`: Upper limit for bookmaker probabilities (default = 1000).
  - `R_max`: Upper limit for the betting ratio (default = 1000).

- **Details:**
  The function retrieves tournament data for major Grand Slam events and runs simulations for each tournament specified. It uses the `define_tournament` function to set up the tournament structure and the `simulate_tournament` function to simulate match outcomes.

- **Value:**
  A list of data frames, each containing the betting results for a specific tournament.

---

#### `Results_balance`

Calculates overall balance, total number of bets, number of winning bets, and ROI from betting results.

**Usage:**
```R
Results_balance(x)
```

- **Arguments:**
  - `x`: List of data frames containing results from `Results_betting`.

- **Details:**
  The function iterates over each data frame in the list, summing up the total balance, total number of bets placed, and the number of winning bets across all entries.

- **Value:**
  A data frame summarizing the overall betting performance, including the net balance, number of bets, number of winning bets, and ROI.

---

### Examples

Here’s how you can use the functions in your EloMC package:

```R
# Load the EloMC package
library(EloMC)

# Assuming `estimation` is a data frame obtained using the function compute_elo:
fit <- estimation$results

# Obtain betting results for all tournaments
results <- Results_betting(fit, "Database Tennis.xlsx", n = 10000, tournament = NA,
                            Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                            R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3),
                            Q_max = 1000, R_max = 1000)

# Calculate overall balance from results
balance <- Results_balance(results)
```

## Contributing

Contributions are welcome! If you find bugs or have suggestions for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

  Computes the balance, total number of bets, number of won bets, and ROI from the results provided.
  - **Parameters**:
    - `x`: A list of results containing balance, number of bets, and number of won bets.
  - **Returns**: A data frame with calculated statistics.


