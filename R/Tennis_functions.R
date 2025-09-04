################################################
#PACCHETTO AGGIORNATO DI FUNZIONI per il TENNIS#
################################################

#merge

merged_tennis_data <- function(gender = "ATP", end_year = 2025, start_year = 2013) {

  # Input validation
  if (!gender %in% c("ATP", "WTA")) {
    stop("The 'gender' parameter must be 'ATP' or 'WTA'")
  }

  if (end_year < start_year) {
    stop("End year cannot be earlier than start year")
  }

  if (start_year < 2007) {
    warning("Data might not be available for years before 2007")
  }

  # Installation and loading of welo package
  if (!requireNamespace("welo", quietly = TRUE)) {
    cat("📦 Installing 'welo' package...\n")
    install.packages("welo", quiet = TRUE)
  }

  suppressPackageStartupMessages(library(welo))

  # Create sequence of years
  years <- start_year:end_year
  n_years <- length(years)

  cat("🎾 Downloading", gender, "tennis data from", start_year, "to", end_year, "\n")
  cat("📊 Processing", n_years, "years of data...\n\n")

  # List to store datasets
  tennis_datasets <- list()

  # Simple progress bar
  pb_width <- 50

  # Download data for each year
  for (i in seq_along(years)) {
    year <- as.character(years[i])

    # Update progress bar
    progress <- i / n_years
    filled <- floor(progress * pb_width)
    bar <- paste0(
      "[",
      paste(rep("█", filled), collapse = ""),
      paste(rep("░", pb_width - filled), collapse = ""),
      "] ",
      sprintf("%3.0f%%", progress * 100),
      " - Year ", year
    )
    cat("\r", bar)

    # Download data with error handling
    tryCatch({
      data_year <- suppressMessages(suppressWarnings(tennis_data(year, gender)))

      # Remove specific columns based on year
      columns_to_remove <- c()

      if (as.numeric(year) <= 2014) {
        columns_to_remove <- c("SJW", "SJL", "EXW", "EXL", "LBW", "LBL")
      } else if (as.numeric(year) <= 2018) {
        columns_to_remove <- c("EXW", "EXL", "LBW", "LBL")
      }

      # Remove columns if they exist in the dataset
      if (length(columns_to_remove) > 0) {
        existing_cols <- intersect(columns_to_remove, names(data_year))
        if (length(existing_cols) > 0) {
          data_year <- data_year[, !names(data_year) %in% existing_cols, drop = FALSE]
        }
      }

      tennis_datasets[[year]] <- data_year

    }, error = function(e) {
      cat("\n⚠️  Error downloading data for year", year, ":", e$message, "\n")
    })
  }

  cat("\n\n🔄 Merging datasets...\n")

  # Check if any datasets were downloaded
  if (length(tennis_datasets) == 0) {
    stop("❌ No data was downloaded successfully")
  }

  # Merge all datasets
  merged_data <- do.call(rbind, tennis_datasets)

  # Final statistics
  n_matches <- nrow(merged_data)
  n_tournaments <- length(unique(merged_data$Tournament))
  date_range <- range(as.Date(merged_data$Date), na.rm = TRUE)

  cat("✅ Download completed successfully!\n")
  cat("📈 Dataset statistics:\n")
  cat("   • Total matches:", format(n_matches, big.mark = ".", decimal.mark = ","), "\n")
  cat("   • Unique tournaments:", n_tournaments, "\n")
  cat("   • Period:", format(date_range[1], "%d/%m/%Y"), "-", format(date_range[2], "%d/%m/%Y"), "\n")
  cat("   • Years processed:", paste(names(tennis_datasets), collapse = ", "), "\n\n")

  return(merged_data)
}


############### FUNZIONE MIO CLEAN ################################################

#Non fa molto, sistema solo il df per poterlo mettere dentro welofit, e cancella le righe dove ci sono dati mancanti

clean_data <- function(x, WELO = FALSE, verbose = TRUE, add_id = FALSE) {

  # ⏱️ Start timing
  start_time <- Sys.time()

  # 🔍 Input validation
  if (!inherits(x, "data.frame")) {
    stop("❌ The parameter 'x' must be a data.frame")
  }

  if (nrow(x) == 0) {
    stop("❌ The provided dataset is empty")
  }

  # 📊 Initial information
  initial_rows <- nrow(x)
  if (verbose) {
    cat("🎾 TENNIS DATASET CLEANING\n")
    cat("═════════════════════════\n")
    cat("📈 Initial matches:", format(initial_rows, big.mark = ".", decimal.mark = ","), "\n\n")
  }

  # 🔄 Rename Tier -> Series column if necessary
  if ("Tier" %in% colnames(x)) {
    names(x)[names(x) == "Tier"] <- "Series"
    if (verbose) cat("🔄 Renamed column 'Tier' to 'Series'\n")
  }

  # 📋 Definition of essential columns
  essential_columns <- c("Location", "Tournament", "Date", "Series",
                         "Surface", "Round", "Winner", "Loser", "Comment")

  # Add ATP/WTA if present
  ranking_col <- NULL
  if ("ATP" %in% colnames(x)) {
    essential_columns <- c(essential_columns, "ATP")
    ranking_col <- "ATP"
  } else if ("WTA" %in% colnames(x)) {
    essential_columns <- c(essential_columns, "WTA")
    ranking_col <- "WTA"
  }

  # 🔍 Check existence of essential columns
  missing_cols <- setdiff(essential_columns, colnames(x))
  if (length(missing_cols) > 0) {
    stop("❌ Missing columns in dataset: ", paste(missing_cols, collapse = ", "))
  }

  # 📊 Counter for removed rows
  removal_summary <- data.frame(
    Reason = character(0),
    Rows_Removed = numeric(0),
    stringsAsFactors = FALSE
  )

  # 🧹 Helper function for removing missing data
  remove_na_rows <- function(data, column_name, description) {
    if (column_name %in% colnames(data) && any(is.na(data[[column_name]]))) {
      na_rows <- which(is.na(data[[column_name]]))
      n_removed <- length(na_rows)

      data <- data[-na_rows, , drop = FALSE]

      # Update summary
      removal_summary <<- rbind(removal_summary,
                                data.frame(Reason = description,
                                           Rows_Removed = n_removed,
                                           stringsAsFactors = FALSE))

      if (verbose) {
        cat("🗑️  Removed", format(n_removed, big.mark = "."),
            "matches -", description, "\n")
      }
    }
    return(data)
  }

  # 🧹 Systematic cleaning
  if (verbose) cat("🧹 Data cleaning in progress...\n")

  # Remove rows with NA in essential columns
  column_descriptions <- list(
    "ATP" = "missing ATP ranking",
    "WTA" = "missing WTA ranking",
    "Location" = "missing location",
    "Tournament" = "missing tournament",
    "Date" = "missing date",
    "Series" = "missing series",
    "Surface" = "missing surface",
    "Round" = "missing round",
    "Winner" = "missing winner",
    "Loser" = "missing loser",
    "Comment" = "missing comment"
  )

  for (col in essential_columns) {
    description <- column_descriptions[[col]]
    x <- remove_na_rows(x, col, description)
  }

  # 📋 Create clean dataset
  if (verbose) cat("\n📋 Creating final dataset...\n")

  clean_data <- data.frame(
    Location = x$Location,
    Tournament = x$Tournament,
    Date = x$Date,
    Series = x$Series,
    Surface = x$Surface,
    Round = x$Round,
    Winner = x$Winner,
    Loser = x$Loser,
    Comment = x$Comment,
    stringsAsFactors = FALSE
  )

  # Add ranking column
  if (!is.null(ranking_col)) {
    clean_data[[ranking_col]] <- x[[ranking_col]]
  }

  # 🎯 Calculate WELO statistics if requested
  if (WELO) {
    if (verbose) cat("🎯 Calculating WELO statistics...\n")

    tryCatch({
      # Check availability of game columns
      game_cols_5set <- c("W1", "W2", "W3", "W4", "W5", "L1", "L2", "L3", "L4", "L5")
      game_cols_3set <- c("W1", "W2", "W3", "L1", "L2", "L3")

      if (all(game_cols_5set %in% colnames(x))) {
        # 5-set format
        NG_Winner <- rowSums(x[, c("W1", "W2", "W3", "W4", "W5")], na.rm = TRUE)
        NG_Loser <- rowSums(x[, c("L1", "L2", "L3", "L4", "L5")], na.rm = TRUE)
      } else if (all(game_cols_3set %in% colnames(x))) {
        # 3-set format
        NG_Winner <- rowSums(x[, c("W1", "W2", "W3")], na.rm = TRUE)
        NG_Loser <- rowSums(x[, c("L1", "L2", "L3")], na.rm = TRUE)
      } else {
        warning("⚠️  Game columns not found, WELO skipped")
        WELO <- FALSE
      }

      if (WELO && "Wsets" %in% colnames(x) && "Lsets" %in% colnames(x)) {
        NS_Winner <- as.numeric(x$Wsets)
        NS_Loser <- as.numeric(x$Lsets)

        # Calculate fractions with division by zero control
        total_games <- NG_Winner + NG_Loser
        total_sets <- NS_Winner + NS_Loser

        clean_data$f_g_Winner <- ifelse(total_games > 0, NG_Winner / total_games, 0)
        clean_data$f_g_Loser <- ifelse(total_games > 0, NG_Loser / total_games, 0)
        clean_data$f_s_Winner <- ifelse(total_sets > 0, NS_Winner / total_sets, 0)
        clean_data$f_s_Loser <- ifelse(total_sets > 0, NS_Loser / total_sets, 0)

        if (verbose) cat("✅ WELO statistics added successfully\n")
      }

    }, error = function(e) {
      warning("⚠️  Error in WELO calculation: ", e$message)
    })
  }

  # 📅 Sort by date and round
  if (verbose) cat("📅 Sorting by date and round...\n")
  clean_data <- clean_data[order(clean_data$Date, clean_data$Round), ]

  # 🆔 Add ID if requested
  if (add_id) {
    clean_data$id <- 1:nrow(clean_data)
    clean_data <- clean_data[, c("id", setdiff(names(clean_data), "id"))]
    if (verbose) cat("🆔 Added ID column\n")
  }

  # ⏱️ Final statistics
  end_time <- Sys.time()
  execution_time <- round(as.numeric(end_time - start_time), 3)
  final_rows <- nrow(clean_data)
  removed_rows <- initial_rows - final_rows
  retention_rate <- round((final_rows / initial_rows) * 100, 1)

  if (verbose) {
    cat("\n")
    cat("✅ CLEANING COMPLETED\n")
    cat("═══════════════════\n")
    cat("📊 Final statistics:\n")
    cat("   • Initial matches:", format(initial_rows, big.mark = "."), "\n")
    cat("   • Final matches:  ", format(final_rows, big.mark = "."), "\n")
    cat("   • Removed matches:", format(removed_rows, big.mark = "."), "\n")
    cat("   • Retention rate: ", retention_rate, "%\n")
    cat("   • Execution time: ", execution_time, "seconds\n")

    if (nrow(removal_summary) > 0) {
      cat("\n📋 Removal details:\n")
      for (i in 1:nrow(removal_summary)) {
        cat("   •", removal_summary$Reason[i], ":",
            format(removal_summary$Rows_Removed[i], big.mark = "."), "matches\n")
      }
    }

    if (WELO) {
      welo_cols <- c("f_g_Winner", "f_g_Loser", "f_s_Winner", "f_s_Loser")
      welo_present <- sum(welo_cols %in% colnames(clean_data))
      cat("   • WELO columns added:", welo_present, "/4\n")
    }
    cat("\n")
  }

  return(clean_data)
}




################################ FUNZIONE WELO FIT ##########################################################

#Fa elo e welo, dovrebbe andare, ho cambiato rispetto alla tesi la s, in modo da fare come il WELO, che ha il "tetto max" che è l'Elo, quindi s è minore di 1.
#Quindi ad esempio nell'Elo_Hard, il cambio rating sarà uguale a quello dell'Elo se la partita è su Hard, senno se il match non è su Hard, il cambio sara moltiplicato per s, quindi vale di meno,
#segue semplicemente la logica del Welo. Poi avevo sbagliato una formula della funzione di perdita. I CI non li ho mai guardati

#Le funzioni di perdita vengono calcolate solo per i match "Completed" e per quella specifica superficie.

compute_elo <- function(x, W = "GAMES", SP = 1500, K = "Kovalchik", s = 0.5,
                        CI = FALSE, alpha = 0.05, B = 1000, WELO = FALSE, verbose = TRUE) {

  # ⏱️ Start timing
  start_time <- Sys.time()

  # 🔍 Input validation
  if (!inherits(x, "data.frame")) {
    stop("❌ The parameter 'x' must be a data.frame")
  }

  if (!W %in% c("GAMES", "SETS")) {
    stop("❌ Invalid 'W' parameter. Options: 'GAMES', 'SETS'")
  }

  if (nrow(x) == 0) {
    stop("❌ Empty dataset")
  }

  # Check required columns
  required_cols <- c("Winner", "Loser", "Comment", "Date", "Series", "Round", "Surface")
  missing_cols <- setdiff(required_cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("❌ Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check WELO columns if necessary
  if (WELO) {
    welo_cols <- if (W == "GAMES") c("f_g_Winner", "f_g_Loser") else c("f_s_Winner", "f_s_Loser")
    missing_welo <- setdiff(welo_cols, colnames(x))
    if (length(missing_welo) > 0) {
      warning("⚠️  Missing WELO columns: ", paste(missing_welo, collapse = ", "),
              ". WELO disabled.")
      WELO <- FALSE
    }
  }

  # 📊 Initial information
  total_matches <- nrow(x)
  completed_matches <- sum(x$Comment == "Completed", na.rm = TRUE)

  if (verbose) {
    cat("🎾 ELO RATING CALCULATION\n")
    cat("═══════════════════════\n")
    cat("📈 Total matches:     ", format(total_matches, big.mark = "."), "\n")
    cat("✅ Completed matches: ", format(completed_matches, big.mark = "."), "\n")
    cat("⚙️  Mode:", ifelse(WELO, "ELO + WELO", "ELO"), "\n")
    cat("🎯 Weight:", W, "\n")
    cat("🔧 K method:", K, "\n\n")
  }

  # 📋 Initialize data structures
  n_matches <- nrow(x)

  # Results dataframe
  elo_results <- data.frame(
    Elo_Winner_before_match = numeric(n_matches),
    Elo_Loser_before_match = numeric(n_matches),
    Elo_Winner_after_match = numeric(n_matches),
    Elo_Loser_after_match = numeric(n_matches),
    Elo_pi_hat = numeric(n_matches),
    stringsAsFactors = FALSE
  )

  # Add WELO columns if necessary
  if (WELO) {
    elo_results[c("WElo_Winner_before_match", "WElo_Loser_before_match",
                  "WElo_Winner_after_match", "WElo_Loser_after_match",
                  "WElo_pi_hat")] <- NA_real_
  }

  # Add confidence intervals if requested
  if (CI) {
    ci_cols <- c("Elo_Winner_lb", "Elo_Winner_ub", "Elo_Loser_lb", "Elo_Loser_ub")
    elo_results[ci_cols] <- NA_real_

    if (WELO) {
      welo_ci_cols <- c("WElo_Winner_lb", "WElo_Winner_ub", "WElo_Loser_lb", "WElo_Loser_ub")
      elo_results[welo_ci_cols] <- NA_real_
    }
  }

  # Vectors to track players
  players_elo <- numeric(0)
  players_welo <- if (WELO) numeric(0) else NULL
  players_matches <- integer(0)

  # 🔄 Helper functions
  initialize_player <- function(player) {
    if (!player %in% names(players_elo)) {
      players_elo[player] <<- SP
      players_matches[player] <<- 0

      if (WELO) {
        players_welo[player] <<- SP
      }
    }
  }

  compute_k_factor <- function(player, surface = NULL, series = NULL) {
    matches_played <- players_matches[player]

    base_k <- switch(K,
                     "Kovalchik" = 250 / (matches_played + 5)^0.4,
                     "Grand_Slam" = 250 / (matches_played + 5)^0.4,
                     "Surface_Hard" = 250 / (matches_played + 5)^0.4,
                     "Surface_Clay" = 250 / (matches_played + 5)^0.4,
                     "Surface_Grass" = 250 / (matches_played + 5)^0.4,
                     as.numeric(K)  # Constant numeric K
    )

    # Apply modifiers
    modifier <- 1

    if (K == "Grand_Slam" && !is.null(series) && series != "Grand Slam") {
      modifier <- s
    }

    if (K %in% c("Surface_Hard", "Surface_Clay", "Surface_Grass") && !is.null(surface)) {
      target_surface <- switch(K,
                               "Surface_Hard" = "Hard",
                               "Surface_Clay" = "Clay",
                               "Surface_Grass" = "Grass"
      )
      if (surface != target_surface) {
        modifier <- s
      }
    }

    return(base_k * modifier)
  }

  # Progress bar setup if verbose
  if (verbose) {
    pb_width <- 50
    update_interval <- max(1, floor(n_matches / 100))
  }

  # 🔄 Main loop
  if (verbose) cat("🔄 Processing matches in progress...\n")

  for (row in 1:n_matches) {
    # Progress bar
    if (verbose && (row %% update_interval == 0 || row == n_matches)) {
      progress <- row / n_matches
      filled <- floor(progress * pb_width)
      bar <- paste0(
        "[",
        paste(rep("█", filled), collapse = ""),
        paste(rep("░", pb_width - filled), collapse = ""),
        "] ",
        sprintf("%3.0f%%", progress * 100),
        " (", row, "/", n_matches, ")"
      )
      cat("\r", bar)
    }

    winner <- x$Winner[row]
    loser <- x$Loser[row]

    # Initialize players if necessary
    initialize_player(winner)
    initialize_player(loser)

    # Save pre-match ratings
    elo_results$Elo_Winner_before_match[row] <- players_elo[winner]
    elo_results$Elo_Loser_before_match[row] <- players_elo[loser]

    if (WELO) {
      elo_results$WElo_Winner_before_match[row] <- players_welo[winner]
      elo_results$WElo_Loser_before_match[row] <- players_welo[loser]
    }

    # Increment match counter
    players_matches[winner] <- players_matches[winner] + 1
    players_matches[loser] <- players_matches[loser] + 1

    # Calculate probabilities
    elo_results$Elo_pi_hat[row] <- tennis_prob(players_elo[winner], players_elo[loser])

    if (WELO) {
      elo_results$WElo_pi_hat[row] <- tennis_prob(players_welo[winner], players_welo[loser])
    }

    # Calculate confidence intervals if requested
    if (CI) {
      tryCatch({
        # CI calculations for ELO
        p <- elo_results$Elo_pi_hat[row]
        sim_winner <- sample(0:1, B, replace = TRUE, prob = c(1-p, p))
        k_winner <- compute_k_factor(winner, x$Surface[row], x$Series[row])

        winner_updates <- players_elo[winner] + k_winner * (sim_winner - p)
        elo_results$Elo_Winner_lb[row] <- quantile(winner_updates, alpha/2)
        elo_results$Elo_Winner_ub[row] <- quantile(winner_updates, 1 - alpha/2)

        sim_loser <- sample(0:1, B, replace = TRUE, prob = c(p, 1-p))
        k_loser <- compute_k_factor(loser, x$Surface[row], x$Series[row])

        loser_updates <- players_elo[loser] + k_loser * (sim_loser - (1-p))
        elo_results$Elo_Loser_lb[row] <- quantile(loser_updates, alpha/2)
        elo_results$Elo_Loser_ub[row] <- quantile(loser_updates, 1 - alpha/2)

        # CI calculations for WELO if necessary
        if (WELO) {
          p_w <- elo_results$WElo_pi_hat[row]
          sim_winner_w <- sample(0:1, B, replace = TRUE, prob = c(1-p_w, p_w))
          winner_updates_w <- players_welo[winner] + k_winner * (sim_winner_w - p_w)
          elo_results$WElo_Winner_lb[row] <- quantile(winner_updates_w, alpha/2)
          elo_results$WElo_Winner_ub[row] <- quantile(winner_updates_w, 1 - alpha/2)

          sim_loser_w <- sample(0:1, B, replace = TRUE, prob = c(p_w, 1-p_w))
          loser_updates_w <- players_welo[loser] + k_loser * (sim_loser_w - (1-p_w))
          elo_results$WElo_Loser_lb[row] <- quantile(loser_updates_w, alpha/2)
          elo_results$WElo_Loser_ub[row] <- quantile(loser_updates_w, 1 - alpha/2)
        }
      }, error = function(e) {
        if (verbose) warning("⚠️  Error in CI calculation for row ", row, ": ", e$message)
      })
    }

    # Update ratings only for completed matches
    if (x$Comment[row] == "Completed") {
      # Calculate K factors
      k_winner <- compute_k_factor(winner, x$Surface[row], x$Series[row])
      k_loser <- compute_k_factor(loser, x$Surface[row], x$Series[row])

      # Update ELO
      elo_results$Elo_Winner_after_match[row] <- players_elo[winner] +
        k_winner * (1 - elo_results$Elo_pi_hat[row])
      elo_results$Elo_Loser_after_match[row] <- players_elo[loser] -
        k_loser * (1 - elo_results$Elo_pi_hat[row])

      # Update WELO if necessary
      if (WELO) {
        weight_col <- if (W == "GAMES") "f_g_Winner" else "f_s_Winner"
        weight <- x[[weight_col]][row]

        elo_results$WElo_Winner_after_match[row] <- players_welo[winner] +
          k_winner * (1 - elo_results$WElo_pi_hat[row]) * weight
        elo_results$WElo_Loser_after_match[row] <- players_welo[loser] -
          k_loser * (1 - elo_results$WElo_pi_hat[row]) * weight

        players_welo[winner] <- elo_results$WElo_Winner_after_match[row]
        players_welo[loser] <- elo_results$WElo_Loser_after_match[row]
      }

      # Update current ratings
      players_elo[winner] <- elo_results$Elo_Winner_after_match[row]
      players_elo[loser] <- elo_results$Elo_Loser_after_match[row]

    } else {
      # Incomplete match - keep previous ratings and decrement counters
      players_matches[winner] <- players_matches[winner] - 1
      players_matches[loser] <- players_matches[loser] - 1

      elo_results$Elo_Winner_after_match[row] <- players_elo[winner]
      elo_results$Elo_Loser_after_match[row] <- players_elo[loser]

      if (WELO) {
        elo_results$WElo_Winner_after_match[row] <- players_welo[winner]
        elo_results$WElo_Loser_after_match[row] <- players_welo[loser]
      }
    }
  }

  if (verbose) cat("\n\n📊 Creating final dataset...\n")

  # 📋 Create final dataset
  final_data <- data.frame(
    Date = x$Date,
    Series = x$Series,
    Round = x$Round,
    Surface = x$Surface,
    P_i = x$Winner,
    P_j = x$Loser,
    Outcome_P_i = 1,
    Outcome_P_j = 0,
    Elo_i_before_match = elo_results$Elo_Winner_before_match,
    Elo_j_before_match = elo_results$Elo_Loser_before_match,
    Elo_pi_hat = elo_results$Elo_pi_hat,
    Elo_i_after_match = elo_results$Elo_Winner_after_match,
    Elo_j_after_match = elo_results$Elo_Loser_after_match,
    Comment = x$Comment,
    stringsAsFactors = FALSE
  )

  # Add WELO columns to final dataset
  if (WELO) {
    welo_cols <- data.frame(
      WElo_i_before_match = elo_results$WElo_Winner_before_match,
      WElo_j_before_match = elo_results$WElo_Loser_before_match,
      WElo_pi_hat = elo_results$WElo_pi_hat,
      WElo_i_after_match = elo_results$WElo_Winner_after_match,
      WElo_j_after_match = elo_results$WElo_Loser_after_match,
      stringsAsFactors = FALSE
    )
    final_data <- cbind(final_data, welo_cols)
  }

  # Add confidence intervals to final dataset
  if (CI) {
    ci_data <- elo_results[, grep("_lb$|_ub$", names(elo_results)), drop = FALSE]
    names(ci_data) <- gsub("Winner", "i", gsub("Loser", "j", names(ci_data)))
    final_data <- cbind(final_data, ci_data)
  }

  # 📈 Calculate performance metrics
  if (verbose) cat("📈 Calculating performance metrics...\n")

  completed_data <- final_data[final_data$Comment == "Completed", ]

  # Filter based on K type
  if (K == "Grand_Slam") {
    completed_data <- completed_data[completed_data$Series == "Grand Slam", ]
  } else if (K == "Surface_Hard") {
    completed_data <- completed_data[completed_data$Surface == "Hard", ]
  } else if (K == "Surface_Clay") {
    completed_data <- completed_data[completed_data$Surface == "Clay", ]
  } else if (K == "Surface_Grass") {
    completed_data <- completed_data[completed_data$Surface == "Grass", ]
  }

  # Calculate loss functions
  loss_metrics <- data.frame(
    Brier = mean((completed_data$Elo_pi_hat - completed_data$Outcome_P_i)^2, na.rm = TRUE),
    LogLoss = mean(-log(pmax(completed_data$Elo_pi_hat, 1e-15)), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  rownames(loss_metrics) <- "Elo"

  if (WELO && nrow(completed_data) > 0) {
    welo_loss <- data.frame(
      Brier = mean((completed_data$WElo_pi_hat - completed_data$Outcome_P_i)^2, na.rm = TRUE),
      LogLoss = mean(-log(pmax(completed_data$WElo_pi_hat, 1e-15)), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    rownames(welo_loss) <- "WElo"
    loss_metrics <- rbind(welo_loss, loss_metrics)
  }

  # 🏆 Find highest ratings
  max_elo_idx <- which.max(final_data$Elo_i_after_match)
  highest_elo_info <- list(
    player = final_data$P_i[max_elo_idx],
    rating = final_data$Elo_i_after_match[max_elo_idx],
    date = final_data$Date[max_elo_idx]
  )

  highest_welo_info <- NULL
  if (WELO) {
    max_welo_idx <- which.max(final_data$WElo_i_after_match)
    highest_welo_info <- list(
      player = final_data$P_i[max_welo_idx],
      rating = final_data$WElo_i_after_match[max_welo_idx],
      date = final_data$Date[max_welo_idx]
    )
  }

  # ⏱️ Final time
  end_time <- Sys.time()
  execution_time <- round(as.numeric(end_time - start_time), 3)

  # 📊 Final statistics
  if (verbose) {
    cat("\n")
    cat("✅ ELO CALCULATION COMPLETED\n")
    cat("═══════════════════════════\n")
    cat("⏱️  Execution time:  ", execution_time, "seconds\n")
    cat("🎾 Unique players:   ", length(players_elo), "\n")
    cat("📈 Processed matches:", format(total_matches, big.mark = "."), "\n")
    cat("✅ Evaluated matches:", format(nrow(completed_data), big.mark = "."), "\n")
    cat("📊 Period:           ", as.character(min(final_data$Date)), "→",
        as.character(max(final_data$Date)), "\n")
    cat("🏆 Highest ELO:      ", highest_elo_info$player, "(",
        round(highest_elo_info$rating), ") on", highest_elo_info$date, "\n")

    if (WELO) {
      cat("🏆 Highest WELO:     ", highest_welo_info$player, "(",
          round(highest_welo_info$rating), ") on", highest_welo_info$date, "\n")
    }

    cat("\n📈 PERFORMANCE METRICS:\n")
    print(round(loss_metrics, 4))
    cat("\n")
  }

  # 📦 Create result object
  result <- list(
    results = final_data,
    matches = paste("Number of matches:", total_matches),
    period = paste("From", min(final_data$Date), "to", max(final_data$Date)),
    loss = loss_metrics,
    highest_elo = paste("The player with the highest Elo rate, reached on",
                        highest_elo_info$date, "is:", highest_elo_info$player),
    dataset = x,
    execution_time = execution_time,
    players_count = length(players_elo),
    final_ratings = list(elo = players_elo, welo = if(WELO) players_welo else NULL)
  )

  if (WELO) {
    result$highest_welo <- paste("The player with the highest WElo rate, reached on",
                                 highest_welo_info$date, "is:", highest_welo_info$player)
  }

  class(result) <- c("welo", "list")

  return(result)
}







############################ FUNZIONE DEFINE TOURNAMENT ######################

  "Prende in input il df pulito con la funzione mioclean e con gli elo calcolati. La date da inserire è
  la data in cui la prima partita del torneo viene disputata. Series = (Grand Slam/ Master 1000)"


#Che tiene conto della possibilita che due tornei si giochino insieme:

#Ho aggiunto la possibilità di scegliere se usare l'Elo o il WElo
define_tournament <- function (X, start_date, Serie = "Grand Slam", WELO = FALSE){

  # Controllo sulla Serie
  if (!(Serie %in% c("Grand Slam", "Masters 1000"))) {
    stop("Error: Serie not valid: choose betweeen Grand Slam and Masters 1000 ")
  }

  # Filtraggio del dataframe
  filtered_df <- X[X$Date == start_date & X$Series == Serie, ]

  # Controllo sul numero di righe trovate
  if (nrow(filtered_df) == 0) {
    stop("Error: No matches found in the specific Date and Serie")
  }


  #NOTA: i Grandi Slam cominciano dai 64esimi, i Masters dai 32, Quindi:

  if (Serie == "Grand Slam"){

    # Filtraggio del dataframe
    indice_64esimi <- which(X$Date == start_date & X$Series == "Grand Slam")
    inizio_64 <- min(indice_64esimi) #Trovo la riga della prima partita del torneo

    #Vorrei trovare le successive 63 righe che hanno First Round scritto nella colonna X$Round
    successive_rows <- c()
    for (i in (inizio_64):nrow(X)) {
      if (X$Round[i] == "1st Round" & X$Series[i] == Serie) {
        successive_rows <- c(successive_rows, i)
        if (length(successive_rows) == 64) {
          break
        }
      }
    }
    primo_turno <- X[successive_rows, ]

    if (nrow(primo_turno) != 64) {
      stop("Error: this is not a Grand Slam, or there aren't enough games: maybe you cleaned them")
    }



    # Creazione dei vettori
    if (WELO){
      WElo_i.64 <- primo_turno$WElo_i_before_match
      WElo_j.64 <- primo_turno$WElo_j_before_match
    } else {
      Elo_i.64 <- primo_turno$Elo_i_before_match
      Elo_j.64 <- primo_turno$Elo_j_before_match
    }

    player_i.64  <- primo_turno$P_i
    player_j.64 <- primo_turno$P_j
    outcome_P_i.64 <- primo_turno$Outcome_P_i
    outcome_P_j.64 <- primo_turno$Outcome_P_j

    # Creiamo il set informativo, ovverò tutto ciò che è successo prima dei 64esimi
    set_info.64 <- X[X$Date < start_date, ]

    # Vettore risultati per i giocatori del Primo_turno
    n_partite_i.64 <- numeric(nrow(primo_turno))
    n_partite_j.64 <- numeric(nrow(primo_turno))

    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.64)) {
      count_P_i <- sum(set_info.64$P_i == player_i.64[t]) #in questo caso deicidamo di contare tutte le partite, non solo le completed etc, meglio cosi
      count_P_j <- sum(set_info.64$P_j == player_i.64[t])
      n_partite_i.64[t] <- count_P_i + count_P_j
    }

    for (t in 1:length(player_j.64)) {
      count_P_i <- sum(set_info.64$P_i == player_j.64[t])
      count_P_j <- sum(set_info.64$P_j == player_j.64[t])
      n_partite_j.64[t] <- count_P_i + count_P_j
    }


    # Creazione del dataframe per i 64esimi
    if (WELO){
      sessantaquattresimi <- data.frame(player_i.64, player_j.64, WElo_i.64, WElo_j.64, n_partite_i.64, n_partite_j.64, outcome_P_i.64, outcome_P_j.64)
    } else {
      sessantaquattresimi <- data.frame(player_i.64, player_j.64, Elo_i.64, Elo_j.64, n_partite_i.64, n_partite_j.64, outcome_P_i.64, outcome_P_j.64)
    }

    ###################################


    # 32esimi

    #Vorrei trovare le successive 32 righe che hanno Second Round scritto nella colonna X$Round
    successive_rows.32 <- c()
    for (i in (inizio_64):nrow(X)) {
      if (X$Round[i] == "2nd Round" & X$Series[i] == Serie) {
        successive_rows.32 <- c(successive_rows.32, i)
        if (length(successive_rows.32) == 32) {
          break
        }
      }
    }
    inizio_32  <- min (successive_rows.32)
    secondo_turno <- X[successive_rows.32, ]

    # Creazione dei vettori
    if (WELO){
      WElo_i.32 <- secondo_turno$WElo_i_before_match
      WElo_j.32 <- secondo_turno$WElo_j_before_match
    } else {
      Elo_i.32 <- secondo_turno$Elo_i_before_match
      Elo_j.32 <- secondo_turno$Elo_j_before_match
    }
    player_i.32 <- secondo_turno$P_i
    player_j.32 <- secondo_turno$P_j
    outcome_P_i.32 <- secondo_turno$Outcome_P_i
    outcome_P_j.32 <- secondo_turno$Outcome_P_j

    # Creiamo il set informativo aggiornato
    set_info.32 <- rbind(set_info.64, primo_turno)

    n_partite_i.32 <- numeric(length(player_i.32))
    n_partite_j.32 <- numeric(length(player_j.32))

    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_i.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_i.32[t])
      n_partite_i.32[t] <- count_P_i.32 + count_P_j.32
    }
    for (t in 1:length(player_j.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_j.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_j.32[t])
      n_partite_j.32[t] <- count_P_i.32 + count_P_j.32
    }

    # Creazione del dataframe per i trentaduesimi
    if (WELO){
      trentaduesimi <- data.frame(player_i.32, player_j.32, WElo_i.32, WElo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    } else {
      trentaduesimi <- data.frame(player_i.32, player_j.32, Elo_i.32, Elo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    }
  }






  ##################################
  else if (Serie == "Masters 1000"){

    # Filtraggio del dataframe
    indice_32esimi <- which(X$Date == start_date & X$Series == Serie)
    inizio_32 <- min(indice_32esimi)
    #Vorrei trovare le successive 31 righe che hanno Second Round scritto nella colonna X$Round
    successive_rows.32 <- c()
    for (i in (inizio_32):nrow(X)) {
      if (X$Round[i] == "2nd Round" & X$Series[i] == Serie) {
        successive_rows.32 <- c(successive_rows.32, i)
        if (length(successive_rows.32) == 32) {
          break
        }
      }
    }
    secondo_turno <- X[successive_rows.32, ]

    if (nrow(secondo_turno) != 32) {
      stop("Error: this is not a Masters 1000")
    }



    # Creazione dei vettori
    if (WELO == TRUE){
      WElo_i.32 <- secondo_turno$WElo_i_before_match
      WElo_j.32 <- secondo_turno$WElo_j_before_match
    } else {
      Elo_i.32 <- secondo_turno$Elo_i_before_match
      Elo_j.32 <- secondo_turno$Elo_j_before_match
    }
    player_i.32 <- secondo_turno$P_i
    player_j.32 <- secondo_turno$P_j
    outcome_P_i.32 <- secondo_turno$Outcome_P_i
    outcome_P_j.32 <- secondo_turno$Outcome_P_j

    # Creiamo il set informativo aggiornato
    set_info.32 <- X[X$Date < start_date, ]

    n_partite_i.32 <- numeric(length(player_i.32))
    n_partite_j.32 <- numeric(length(player_j.32))

    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_i.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_i.32[t])
      n_partite_i.32[t] <- count_P_i.32 + count_P_j.32
    }
    for (t in 1:length(player_j.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_j.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_j.32[t])
      n_partite_j.32[t] <- count_P_i.32 + count_P_j.32
    }

    # Creazione del dataframe per i trentaduesimi
    if (WELO == TRUE){
      trentaduesimi <- data.frame(player_i.32, player_j.32, WElo_i.32, WElo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    } else {
      trentaduesimi <- data.frame(player_i.32, player_j.32, Elo_i.32, Elo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    }
  }


  ###################################################

  #Sedicesimi

  successive_rows.16 <- c()
  for (i in (inizio_32):nrow(X)) {
    if (X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
      successive_rows.16 <- c(successive_rows.16, i)
      if (length(successive_rows.16) == 16) {
        break
      }
    }
  }
  Turno_16 <- X[successive_rows.16, ]
  fine_16 <- max(successive_rows.16)


  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.16 <- Turno_16$WElo_i_before_match
    WElo_j.16 <- Turno_16$WElo_j_before_match
  } else {
    Elo_i.16 <- Turno_16$Elo_i_before_match
    Elo_j.16 <- Turno_16$Elo_j_before_match
  }
  player_i.16 <- Turno_16$P_i
  player_j.16 <- Turno_16$P_j
  outcome_P_i.16 <- Turno_16$Outcome_P_i
  outcome_P_j.16 <- Turno_16$Outcome_P_j

  # Creiamo il set informativo aggiornato
  set_info.16 <- rbind (set_info.32, secondo_turno)

  n_partite_i.16 <- numeric(length(player_i.16))
  n_partite_j.16 <- numeric(length(player_j.16))

  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.16)) {
    count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
    count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
    n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
  }

  for (t in 1:length(player_j.16)) {
    count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
    count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
    n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
  }

  # Creazione del dataframe per i 16esimi
  if (WELO == TRUE){
    sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
  } else {
    sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
  }






  ####################################################
  # Ottavi
  successive_rows.8 <- c()
  for (i in (fine_16):nrow(X)) {
    if (X$Round[i] == "4th Round" & X$Series[i] == Serie) {
      successive_rows.8 <- c(successive_rows.8, i)
      if (length(successive_rows.8) == 8) {
        break
      }
    }
  }
  righe_8 <- X[successive_rows.8, ]
  fine_8 <- max(successive_rows.8)


  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.8 <- righe_8$WElo_i_before_match
    WElo_j.8 <- righe_8$WElo_j_before_match
  } else {
    Elo_i.8 <- righe_8$Elo_i_before_match
    Elo_j.8 <- righe_8$Elo_j_before_match
  }

  player_i.8 <- righe_8$P_i
  player_j.8 <- righe_8$P_j
  outcome_P_i.8 <- righe_8$Outcome_P_i
  outcome_P_j.8 <- righe_8$Outcome_P_j

  # Creiamo il set informativo aggiornato
  set_info.8 <- rbind(set_info.16, Turno_16)

  n_partite_i.8 <- numeric(length(player_i.8))
  n_partite_j.8 <- numeric(length(player_j.8))

  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.8)) {
    count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
    count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
    n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
  }

  for (t in 1:length(player_j.8)) {
    count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
    count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
    n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
  }

  # Creazione del dataframe per gli Ottavi
  if (WELO == TRUE){
    ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
  } else {
    ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
  }






  ########################################################
  # Quarti
  successive_rows.4 <- c()
  for (i in (fine_8):nrow(X)) {
    if (X$Round[i] == "Quarterfinals" & X$Series[i] == Serie) {
      successive_rows.4 <- c(successive_rows.4, i)
      if (length(successive_rows.4) == 4) {
        break
      }
    }
  }
  righe_4 <- X[successive_rows.4, ]
  fine_4 <- max(successive_rows.4)


  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.4 <- righe_4$WElo_i_before_match
    WElo_j.4 <- righe_4$WElo_j_before_match
  } else {
    Elo_i.4 <- righe_4$Elo_i_before_match
    Elo_j.4 <- righe_4$Elo_j_before_match
  }

  player_i.4 <- righe_4$P_i
  player_j.4 <- righe_4$P_j
  outcome_P_i.4 <- righe_4$Outcome_P_i
  outcome_P_j.4 <- righe_4$Outcome_P_j

  # Creiamo il set informativo aggiornato
  set_info.4 <- rbind(set_info.8, righe_8)

  n_partite_i.4 <- numeric(length(player_i.4))
  n_partite_j.4 <- numeric(length(player_j.4))

  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.4)) {
    count_P_i.4 <- sum(set_info.4$P_i == player_i.4[t])
    count_P_j.4 <- sum(set_info.4$P_j == player_i.4[t])
    n_partite_i.4[t] <- count_P_i.4 + count_P_j.4
  }

  for (t in 1:length(player_j.4)) {
    count_P_i.4 <- sum(set_info.4$P_i == player_j.4[t])
    count_P_j.4 <- sum(set_info.4$P_j == player_j.4[t])
    n_partite_j.4[t] <- count_P_i.4 + count_P_j.4
  }


  # Creazione del dataframe per i Quarti
  if (WELO == TRUE){
    quarti <- data.frame(player_i.4, player_j.4, WElo_i.4, WElo_j.4, n_partite_i.4, n_partite_j.4, outcome_P_i.4, outcome_P_j.4)
  } else {
    quarti <- data.frame(player_i.4, player_j.4, Elo_i.4, Elo_j.4, n_partite_i.4, n_partite_j.4, outcome_P_i.4, outcome_P_j.4)
  }










  ########################################################
  # Semi-finals
  successive_rows.2 <- c()
  for (i in (fine_4):nrow(X)) {
    if (X$Round[i] == "Semifinals" & X$Series[i] == Serie) {
      successive_rows.2 <- c(successive_rows.2, i)
      if (length(successive_rows.2) == 2) {
        break
      }
    }
  }
  righe_2 <- X[successive_rows.2, ]
  fine_2 <- max(successive_rows.2)



  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.2 <- righe_2$WElo_i_before_match
    WElo_j.2 <- righe_2$WElo_j_before_match
  } else {
    Elo_i.2 <- righe_2$Elo_i_before_match
    Elo_j.2 <- righe_2$Elo_j_before_match
  }
  player_i.2 <- righe_2$P_i
  player_j.2 <- righe_2$P_j
  outcome_P_i.2 <- righe_2$Outcome_P_i
  outcome_P_j.2 <- righe_2$Outcome_P_j

  # Creiamo il set informativo aggiornato
  set_info.2 <- rbind(set_info.4, righe_4)

  n_partite_i.2 <- numeric(length(player_i.2))
  n_partite_j.2 <- numeric(length(player_j.2))

  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.2)) {
    count_P_i.2 <- sum(set_info.2$P_i == player_i.2[t])
    count_P_j.2 <- sum(set_info.2$P_j == player_i.2[t])
    n_partite_i.2[t] <- count_P_i.2 + count_P_j.2
  }

  for (t in 1:length(player_j.2)) {
    count_P_i.2 <- sum(set_info.2$P_i == player_j.2[t])
    count_P_j.2 <- sum(set_info.2$P_j == player_j.2[t])
    n_partite_j.2[t] <- count_P_i.2 + count_P_j.2
  }


  # Creazione del dataframe per le Semi-finals
  if (WELO == TRUE){
    semifinali <- data.frame(player_i.2, player_j.2, WElo_i.2, WElo_j.2, n_partite_i.2, n_partite_j.2, outcome_P_i.2, outcome_P_j.2)
  } else {
    semifinali <- data.frame(player_i.2, player_j.2, Elo_i.2, Elo_j.2, n_partite_i.2, n_partite_j.2, outcome_P_i.2, outcome_P_j.2)
  }








  #Se va lento::
  #Non mi piace la count così:: --> cerca di prendere il valore del conteggio del turno prima e semplicemente aggiungi uno


  #SECONDA PARTE:




  # Determiniamo le righe di quarti che soddisfano le condizioni
  "riga_1 <- which(quarti$player_i.4 == semifinali$player_i.2[1] | quarti$player_j.4 == semifinali$player_i.2[1])
riga_2 <- which(quarti$player_i.4 == semifinali$player_j.2[1] | quarti$player_j.4 == semifinali$player_j.2[1])
riga_3 <- which(quarti$player_i.4 == semifinali$player_i.2[2] | quarti$player_j.4 == semifinali$player_i.2[2])
riga_4 <- which(quarti$player_i.4 == semifinali$player_j.2[2] | quarti$player_j.4 == semifinali$player_j.2[2])
"








  ######################################################################################################àà

  # ORDINAMENTO TABELLONE
  results.4 <- c()
  for (i in 1:2) {
    # Andiamo a trovare quale dei due player di riga dei quarti(player i o j), ha una corrispondenza nelle semifinali
    #Cosi troviamo il vincitore.  Di fatto lo facciamo in ordine e segnamo il nome delle righe,
    #Cosi avremo un vettore di indici da usare per ordinare i quarti

    results.4 <- c(results.4,
                   which(quarti$player_i.4 == semifinali$player_i.2[i] | quarti$player_j.4 == semifinali$player_i.2[i]),
                   which(quarti$player_i.4 == semifinali$player_j.2[i] | quarti$player_j.4 == semifinali$player_j.2[i])
    )
  }

  quarti_ord <- quarti[results.4, ]

  #Reset indici di riga disordinati
  rownames(quarti_ord) <- NULL




  #################################################
  # OTTAVI
  results.8 <- c()

  # STessa roba:
  "Nel ciclo for,
  Data una riga dei quarti ordinati (un match tra A e B), trovi di fatto le righe in cui A ha giocato il
  suo ottavo e B il suo, poi le metti vicino.
  "
  for (i in 1:4) {
    results.8 <- c(results.8,
                   which(ottavi$player_i.8 == quarti_ord$player_i.4[i] | ottavi$player_j.8 == quarti_ord$player_i.4[i]),
                   which(ottavi$player_i.8 == quarti_ord$player_j.4[i] | ottavi$player_j.8 == quarti_ord$player_j.4[i])
    )
  }

  ottavi_ord <- ottavi[results.8, ]

  rownames(ottavi_ord) <- NULL





  # Sempre stesso:
  " In che righe, coloro che giocheranno gli ottavi, hanno giocato i 16esimi?"
  # Chi ha giocato contro viene messo vicino

  results.16 <- c()

  for (i in 1:8) {
    results.16 <- c(results.16,
                    which(sedicesimi$player_i.16 == ottavi_ord$player_i.8[i] | sedicesimi$player_j.16 == ottavi_ord$player_i.8[i]),
                    which(sedicesimi$player_i.16 == ottavi_ord$player_j.8[i] | sedicesimi$player_j.16 == ottavi_ord$player_j.8[i])
    )
  }

  sedicesimi_ord <- sedicesimi[results.16, ]
  rownames(sedicesimi_ord) <- NULL






  results.32 <- c()

  for (i in 1:16) {
    results.32 <- c(results.32,
                    which(trentaduesimi$player_i.32 == sedicesimi_ord$player_i.16[i] | trentaduesimi$player_j.32 == sedicesimi_ord$player_i.16[i]),
                    which(trentaduesimi$player_i.32 == sedicesimi_ord$player_j.16[i] | trentaduesimi$player_j.32 == sedicesimi_ord$player_j.16[i])
    )
  }

  trentaduesimi_ord <- trentaduesimi[results.32, ]

  rownames(trentaduesimi_ord) <- NULL



  if (Serie == "Grand Slam"){
    results.64 <- c()

    for (i in 1:32) {
      results.64 <- c(results.64,
                      which(sessantaquattresimi$player_i.64 == trentaduesimi_ord$player_i.32[i] | sessantaquattresimi$player_j.64 == trentaduesimi_ord$player_i.32[i]),
                      which(sessantaquattresimi$player_i.64 == trentaduesimi_ord$player_j.32[i] | sessantaquattresimi$player_j.64 == trentaduesimi_ord$player_j.32[i])
      )
    }

    sessantaquattresimi_ord <- sessantaquattresimi[results.64, ]

    rownames(sessantaquattresimi_ord) <- NULL
  }



  return_list <- list()

  if (Serie == "Grand Slam") {
    return_list$sessantaquattresimi_ord <- sessantaquattresimi_ord
  }

  return_list <- c(return_list,
                   list(trentaduesimi_ord = trentaduesimi_ord,
                        sedicesimi_ord = sedicesimi_ord,
                        ottavi_ord = ottavi_ord,
                        quarti_ord = quarti_ord,
                        semifinali = semifinali))

  return(return_list)


}




################# FUNZIONE simulate Tournament (efficiente) ###########################
"Prende in input il valore del tabellone ordinato con la funzione apposita. Successivamente imponi
  il numero delle simulazioni, e simula il torneo quel numero di volte.
  L'output sarà un oggetto formato dalle varie tabelle di probabilità di arrivare a un certo punto
  del torneo"

simulate_tournament <- function(X, sim =  10000, WELO = FALSE) {

  n <- nrow(X)

  table.64 <- matrix(0, nrow = sim, ncol = 64)
  table.32 <- matrix(0, nrow = sim, ncol = 32)
  table.16 <- matrix(0, nrow = sim, ncol = 16)
  table.8 <- matrix(0, nrow = sim, ncol = 8)
  table.4 <- matrix(0, nrow = sim, ncol = 4)
  table.2 <- matrix(0, nrow = sim, ncol = 2)
  table.1 <- matrix(0, nrow = sim, ncol = 1)

  #  startiamo i vettori per i 64esimi
  p.64 <- numeric(64)

  if (WELO){
    welo_winner.64 <- numeric(64)
  } else {
    elo_winner.64 <- numeric(64)
  }

  n_winner.64 <- numeric(64)
  winner.64 <- numeric(64)

  #  #startiamo i vettori per i 32esimi
  p.32<-numeric(32)
  if (WELO){
    welo_winner.32 <- numeric(32)
  } else {
    elo_winner.32 <- numeric(32)
  }

  n_winner.32 <- numeric(32)
  winner.32 <- numeric(32)

  #startiamo i valori per i 16esimi
  p.16<-numeric(16)

  if (WELO){
    welo_winner.16 <- numeric(16)
  } else {
    elo_winner.16<- numeric(16)
  }

  n_winner.16 <- numeric(16)
  winner.16 <- numeric(16)

  #startiamo i valori per i ottavi
  p.8<-numeric(8)

  if (WELO){
    welo_winner.8 <- numeric(8)
  } else {
    elo_winner.8<- numeric(8)
  }
  n_winner.8 <- numeric(8)
  winner.8 <- numeric(8)

  #startiamo i valori per i quarti
  p.4<-numeric(4)

  if (WELO){
    welo_winner.4 <- numeric(4)
  } else {
    elo_winner.4<- numeric(4)
  }
  n_winner.4 <- numeric(4)
  winner.4 <- numeric(4)

  #startiamo i valori per semi
  p.2<-numeric(2)
  if (WELO){
    welo_winner.2 <- numeric(2)
  } else {
    elo_winner.2<- numeric(2)
  }
  n_winner.2 <- numeric(2)
  winner.2 <- numeric(2)

  #startiamo i valori per finale
  p.1<-numeric(1)
  if (WELO){
    welo_winner.1 <- numeric(1)
  } else {
    elo_winner.1<- numeric(1)
  }
  n_winner.1 <- numeric(1)
  winner.1 <- numeric(1)

  pb <- progress_bar$new(total = sim, format = "[:bar] :percent Time remaining: :eta")

  # Ora incomincio una serie di if, quello che fanno è tutti lo stesso, cambia solo in base
  # a che fase del torneo gli metto in input

  if ( n==64) {
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento


      p.64 <- sapply(1:64, function (x) tennis_prob(X[x,3], X[x,4]))
      match_result.64 <- rbern (64, p.64)

      if (WELO){
        welo_winner.64 <- ifelse(match_result.64 ==1, X[,3], X[,4])
      } else {
        elo_winner.64<-ifelse(match_result.64 ==1, X[,3], X[,4])}

      n_winner.64 <- ifelse(match_result.64 == 1, X[,5] + 1, X[,6] + 1)

      if (WELO){
        welo_winner.64 <- welo_winner.64 + ((250/((n_winner.64 + 5)^0.4)) * (ifelse(match_result.64 == 1, 1 - p.64, p.64)))
      } else {
        elo_winner.64 <- elo_winner.64 + ((250/((n_winner.64 + 5)^0.4)) * (ifelse(match_result.64 == 1, 1 - p.64, p.64)))
      }

      winner.64 <- ifelse(match_result.64 == 1, X[, 1], X[, 2])
      table.64[t, ] <- winner.64





      if (WELO){
        p.32 <- sapply(1:32, function (f) tennis_prob(welo_winner.64[f*2-1], welo_winner.64[f*2]))
      } else {
        p.32 <- sapply(1:32, function (f) tennis_prob(elo_winner.64[f*2-1], elo_winner.64[f*2]))}

      match_result.32 <- rbern (32, p.32)

      if (WELO){
        welo_winner.32 <- ifelse(match_result.32 == 1, welo_winner.64[seq(1, 64, by = 2)], welo_winner.64[seq(2, 64, by = 2)])
      } else {
        elo_winner.32 <- ifelse(match_result.32 == 1, elo_winner.64[seq(1, 64, by = 2)], elo_winner.64[seq(2, 64, by = 2)])
      }
      n_winner.32 <- ifelse(match_result.32 == 1, n_winner.64[seq(1, 64, by = 2)] + 1, n_winner.64[seq(2, 64, by = 2)] + 1)

      if (WELO){
        welo_winner.32 <- welo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      } else {
        elo_winner.32 <- elo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      }
      winner.32 <- ifelse(match_result.32 == 1, winner.64[seq(1, 64, by = 2)], winner.64[seq(2, 64, by = 2)])
      table.32[t, ] <- winner.32






      if (WELO){
        p.16 <- sapply(1:16, function (g) tennis_prob(welo_winner.32[g*2-1], welo_winner.32[g*2]))
      } else {
        p.16 <- sapply(1:16, function (g) tennis_prob(elo_winner.32[g*2-1], elo_winner.32[g*2]))
      }

      match_result.16 <- rbern (16, p.16)

      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 == 1, welo_winner.32[seq(1, 32, by = 2)], welo_winner.32[seq(2, 32, by = 2)])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, elo_winner.32[seq(1, 32, by = 2)], elo_winner.32[seq(2, 32, by = 2)])
      }

      n_winner.16 <- ifelse(match_result.16 == 1, n_winner.32[seq(1, 32, by = 2)] + 1, n_winner.32[seq(2, 32, by = 2)] + 1) #tipo qua se vuoi fare qualcosa di utile dovresti mettere +2 perche si ipotizza che il 32 sia gia stato giocato?

      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      }

      winner.16 <- ifelse(match_result.16 == 1, winner.32[seq(1, 32, by = 2)], winner.32[seq(2, 32, by = 2)])
      table.16[t, ] <- winner.16




      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8

      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4

      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2


      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1


    }

    prob.64 <- table(table.64) / sim
    prob.32 <- table(table.32) / sim
    prob.16 <- table(table.16) / sim
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.64 <- sort(prob.64, decreasing = TRUE)
    prob.32 <- sort(prob.32, decreasing = TRUE)
    prob.16 <- sort(prob.16, decreasing = TRUE)
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.64 = prob.64, prob.32 = prob.32, prob.16 = prob.16, prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1))
  }
  else if (n == 32){
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento
      p.32 <- sapply(1:32, function (f) tennis_prob(X[f,3], X[f,4]))
      match_result.32 <- rbern (32, p.32)
      if (WELO){
        welo_winner.32 <- ifelse(match_result.32 ==1, X[,3], X[,4])
      } else {
        elo_winner.32 <- ifelse(match_result.32 == 1, X[,3], X[,4])    }
      n_winner.32 <- ifelse(match_result.32 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.32 <- welo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      } else {
        elo_winner.32 <- elo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))}
      winner.32 <- ifelse(match_result.32 == 1, X[, 1], X[, 2])
      table.32[t, ] <- winner.32



      if (WELO){
        p.16 <- sapply(1:16, function (g) tennis_prob(welo_winner.32[g*2-1], welo_winner.32[g*2]))
      } else {
        p.16 <- sapply(1:16, function (g) tennis_prob(elo_winner.32[g*2-1], elo_winner.32[g*2]))
      }

      match_result.16 <- rbern (16, p.16)

      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 == 1, welo_winner.32[seq(1, 32, by = 2)], welo_winner.32[seq(2, 32, by = 2)])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, elo_winner.32[seq(1, 32, by = 2)], elo_winner.32[seq(2, 32, by = 2)])
      }

      n_winner.16 <- ifelse(match_result.16 == 1, n_winner.32[seq(1, 32, by = 2)] + 1, n_winner.32[seq(2, 32, by = 2)] + 1)

      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      }

      winner.16 <- ifelse(match_result.16 == 1, winner.32[seq(1, 32, by = 2)], winner.32[seq(2, 32, by = 2)])
      table.16[t, ] <- winner.16




      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8

      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4

      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2


      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }

    prob.16 <- table(table.16) / sim
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.16 <- sort(prob.16, decreasing = TRUE)
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.16 = prob.16, prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1))
  }

  else if (n == 16){
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento
      p.16 <- sapply(1:16, function (j) tennis_prob(X[j,3], X[j,4]))
      match_result.16 <- rbern (16, p.16)
      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 ==1, X[,3], X[,4])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, X[,3], X[,4])    }
      n_winner.16 <- ifelse(match_result.16 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))}
      winner.16 <- ifelse(match_result.16 == 1, X[, 1], X[, 2])
      table.16[t, ] <- winner.16

      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8

      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4

      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2


      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }

    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1))
  }

  else if (n == 8){
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento

      p.8 <- sapply(1:8, function (m)  tennis_prob(X[m,3], X[m,4]))
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, X[,3], X[,4])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, X[,3], X[,4]) }
      n_winner.8 <- ifelse(match_result.8 == 1, X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, X[, 1], X[, 2])
      table.8[t, ] <- winner.8

      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4

      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2


      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }

    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1))
  }


  else if (n == 4){
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento

      p.4 <- sapply(1:4, function (c) tennis_prob(X[c,3], X[c,4]))
      match_result.4 <- rbern (4, p.4)
      if (WELO) {
        welo_winner.4 <- ifelse(match_result.4 == 1, X[,3], X[,4])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, X[,3], X[,4])}
      n_winner.4 <- ifelse(match_result.4 == 1, X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1,  X[, 1], X[, 2])
      table.4[t, ] <- winner.4

      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2


      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1

    }

    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1))
  }

  else if (n == 2){
    for (t in 1:sim) {

      pb$tick() # Aggiorna la barra di avanzamento

      p.2 <- sapply(1:2, function (b) tennis_prob(X[b,3], X[b,4]))
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, X[,3], X[,4])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, X[,3], X[,4])    }
      n_winner.2 <- ifelse(match_result.2 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, X[, 1], X[, 2])
      table.2[t, ] <- winner.2




      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1


    }

    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim

    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)

    return(list(prob.2 = prob.2, prob.1 = prob.1))
  }
}




#################plot
plot_player_elo <- function(player_name, tables, start_date = NULL, end_date = NULL) {

  # Initialize an empty list to store xts time series objects for each table
  ts_list <- list()
  surface_names <- c()  # Keep track of valid surface names

  # Convert start_date and end_date to Date format if provided
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date, format = "%Y-%m-%d")
  }
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date, format = "%Y-%m-%d")
  }

  # Loop over each table type in the input list
  for (i in 1:length(tables)) {
    # Extract current table and filter it for the chosen player
    table <- tables[[i]]
    player_data <- table[table$P_i == player_name | table$P_j == player_name, ]

    # Check if the filtered data has any rows
    if (nrow(player_data) > 0) {

      # Select relevant columns
      player_data <- player_data[, c("Date", "P_i", "P_j", "Elo_i_after_match", "Elo_j_after_match")]

      # Create a new column for the player's Elo after the match
      player_data$elo_player <- ifelse(player_data$P_i == player_name,
                                       player_data$Elo_i_after_match,
                                       ifelse(player_data$P_j == player_name,
                                              player_data$Elo_j_after_match,
                                              NA))

      # Convert the "Date" column to Date format
      player_data$Date <- as.Date(player_data$Date, format = "%Y-%m-%d")

      # Filter the data based on start_date and end_date if provided
      if (!is.null(start_date)) {
        player_data <- player_data[player_data$Date >= start_date, ]
      }
      if (!is.null(end_date)) {
        player_data <- player_data[player_data$Date <= end_date, ]
      }

      # Create an xts object for the Elo time series
      player_ts <- xts(player_data$elo_player, order.by = player_data$Date)

      # Add the time series to the list if it's not empty
      if (nrow(player_ts) > 0) {
        ts_list[[i]] <- player_ts
        surface_names <- c(surface_names, names(tables)[i])  # Save the surface name
      }
    }
  }

  # Check if we have valid time series to combine
  if (length(ts_list) > 0) {
    # Combine all the individual time series into one xts object
    combined_ts <- do.call(cbind, ts_list)

    # Change column names to indicate surface type
    colnames(combined_ts) <- surface_names

    # Plot combined time series with appropriate labels and settings
    plot.xts(combined_ts,
             col = c("blue", "green", "brown", "purple")[1:length(ts_list)],
             lwd = 2,
             type = "l",
             major.ticks = NULL,
             grid.ticks.on = "years",
             legend.loc = "bottomright",
             ylab = "Elo Rating",
             xlab = "Date",
             #main = paste(player_name, "Elo Ratings Over Time"),
             main = NULL,
             main.timespan = FALSE
    )
  } else {
    # If no valid time series, print a message
    cat("No valid data found for player", player_name, "in the specified date range\n")
  }
}

