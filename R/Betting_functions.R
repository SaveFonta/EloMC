#############################################
############ FUNZIONI PER IL BETTING#########
#############################################
#In input ci mettiamo la table ottenuta dalla simulazioni del torneo dai 64esimi con le probabilità di vittoria:


#Funzione fondamentale
Betting_function <- function(X, Book, q_values, r_values, q_max = 1000, r_max = 1000) {

  # Se esiste colonna "Resuslt" in Book, rinominala
  if ("Resuslt" %in% names(Book)) {
    names(Book)[names(Book) == "Resuslt"] <- "Result"
    message("✅ Colonna 'Resuslt' rinominata in 'Result' nel book")
  }

  # Trasformiamo probab in un df
  probab_df <- as.data.frame(X)

  # Chiamiamo la prima colonna Nome
  names(probab_df)[1] <- "Player"

  # La seconda prob_elo
  names(probab_df)[2] <- "Prob_elo"

  # Uniamo i due dataframe sulla colonna "Player"
  merged_df <- merge(probab_df, Book, by = "Player")

  # Inizializza un dataframe per memorizzare i risultati
  risultati_df <- data.frame(q_Value = numeric(0), r_Value = numeric(0),
                             Balance = numeric(0), Players = character(0),
                             Num_Bets = numeric(0), N_Bets_won = numeric(0))

  # Itera su ogni valore di q
  for (q in q_values) {
    # Itera su ogni valore di r
    for (j in seq_along(r_values)) {
      risultato <- merged_df[merged_df$Prob_bookmaker > q & merged_df$Prob_bookmaker <= q_max &
                               (merged_df$Prob_elo / merged_df$Prob_bookmaker) <= r_max &
                               (merged_df$Prob_elo / merged_df$Prob_bookmaker) > r_values[j], ]
      players_su_cui_betto <- as.character(risultato$Player)

      if (nrow(risultato) == 0) {
        bilancio <- 0
        giocatori <- character(0)
        num_players <- 0
        n_bets_won <- 0
      } else {
        perdita <- 0
        guadagno <- 0
        giocatori <- character(0)
        n_bets_won <- 0
        for (i in 1:nrow(risultato)) {
          if (!is.na(risultato$Result[i]) && risultato$Result[i] == "WINNER") {
            guadagno <- (risultato$Quota[i] - 1)
            giocatori <- c(giocatori, players_su_cui_betto[i])
            n_bets_won <- n_bets_won + 1
          } else if (is.na(risultato$Result[i])) {
            perdita <- perdita + 1
            giocatori <- c(giocatori, players_su_cui_betto[i])
          }
        }
        bilancio <- guadagno - perdita
        num_players <- length(unique(players_su_cui_betto))
      }

      risultati_df <- rbind(risultati_df, data.frame(
        q_Value = q, r_Value = r_values[j], Balance = bilancio,
        Players = toString(giocatori), Num_Bets = num_players,
        N_Bets_won = n_bets_won
      ))
    }
  }

  return(risultati_df)
}

########################################################################
#Funzioni spezzettate:

##################
GS_sim <- function (X, tournament = NA, n = 10000, WELO = FALSE,
                    only_prob1 = TRUE)  # nuovo argomento
{
  tournament_datas = data.frame(start_date = character(), sheet = character)

  if (tournament == "Australian Open" || is.na(tournament)) {
    tournament_datas <- rbind(tournament_datas, c("2025-01-12","Australian Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-01-14","Australian Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-01-16","Australian Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-01-17","Australian Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-02-08","Australian Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-01-20","Australian Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-01-14","Australian Open 2019"))
  }
  if (tournament == "Roland Garros" || is.na(tournament)) {
    tournament_datas <- rbind(tournament_datas, c("2025-05-25","Roland Garros 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-05-26","Roland Garros 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-05-28","Roland Garros 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-05-22","Roland Garros 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-05-30","Roland Garros 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-09-27","Roland Garros 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-05-26","Roland Garros 2019"))
  }
  if (tournament == "US Open" || is.na(tournament)) {
    tournament_datas <- rbind(tournament_datas, c("2025-08-24","US Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-08-26","US Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-08-28","US Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-08-29","US Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-08-30","US Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-08-31","US Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-08-26","US Open 2019"))
  }
  if (tournament == "Wimbledon" || is.na(tournament)) {
    tournament_datas <- rbind(tournament_datas, c("2025-06-30","Wimbledon 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-07-01","Wimbledon 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-07-03","Wimbledon 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-06-27","Wimbledon 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-06-28","Wimbledon 2021"))
    tournament_datas <- rbind(tournament_datas, c("2019-07-01","Wimbledon 2019"))
  }

  simulazioni <- list()

  for (i in 1:nrow(tournament_datas)) {
    if (WELO) {
      torneo <- define_tournament(X, start_date = tournament_datas[i,1],
                                  Serie = "Grand Slam", WELO = TRUE)
    } else {
      torneo <- define_tournament(X, start_date = tournament_datas[i,1],
                                  Serie = "Grand Slam", WELO = FALSE)
    }

    sessantaquattresimi <- torneo$sessantaquattresimi_ord
    risultati <- simulate_tournament(sessantaquattresimi, n)

    if (only_prob1) {
      simulazioni[[tournament_datas[i,2]]] <- list(risultati$prob.1)
    } else {
      simulazioni[[tournament_datas[i,2]]] <- list(risultati) # tutte le probabilità
    }

    cat("Torneo", tournament_datas[i,2], "completato\n")
  }

  return(simulazioni)
}

######

Estract_book <- function(Nome_Excel, tournament = NA, input_path = input_dir) {
  # Create the full file path
  file_path <- file.path(input_path, Nome_Excel)

  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  tournament_datas <- data.frame(start_date = character(), sheet = character())

  if(tournament == "Australian Open" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-01-12", "Australian Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-01-14", "Australian Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-01-16", "Australian Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-01-17", "Australian Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-02-08", "Australian Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-01-20", "Australian Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-01-14", "Australian Open 2019"))
  }

  if(tournament == "Roland Garros" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-05-25", "Roland Garros 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-05-26", "Roland Garros 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-05-28", "Roland Garros 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-05-22", "Roland Garros 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-05-30", "Roland Garros 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-09-27", "Roland Garros 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-05-26", "Roland Garros 2019"))
  }

  if(tournament == "US Open" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-08-24", "US Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-08-26", "US Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-08-28", "US Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-08-29", "US Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-08-30", "US Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-08-31", "US Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-08-26", "US Open 2019"))
  }

  if(tournament == "Wimbledon" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-06-30", "Wimbledon 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-07-01", "Wimbledon 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-07-03", "Wimbledon 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-06-27", "Wimbledon 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-06-28", "Wimbledon 2021"))
    tournament_datas <- rbind(tournament_datas, c("2019-07-01", "Wimbledon 2019"))
  }

  book_list <- list()

  for (i in 1:nrow(tournament_datas)) {
    suppressMessages({
      book <- read_excel(file_path, sheet = tournament_datas[i, 2])  # Use file_path here
      colnames(book)[7] <- "Player"
    })
    book_list[[tournament_datas[i, 2]]] <- book
    cat("Quote", tournament_datas[i, 2], "ottenute\n")
  }

  return(book_list)
}


##############


Place_bet <- function(probab, book,
                      Q_values = c(0.02, 0.05, 0.10, 0.15, 0.2),
                      R_values = c(1.05, 1.1, 1.20, 1.4, 1.6, 1.8),
                      Q_max = 1000, R_max = 1000) {

  Bet_results <- list()
  missing_tournaments <- c()

  for (tournament_name in names(probab)) {
    if (tournament_name %in% names(book)) {
      Bet_results[[tournament_name]] <- Betting_function(
        probab[[tournament_name]],
        book[[tournament_name]],
        q_values = Q_values,
        r_values = R_values,
        q_max = Q_max,
        r_max = R_max
      )
      cat("✅ Torneo scommesso:", tournament_name, "\n")
    } else {
      missing_tournaments <- c(missing_tournaments, tournament_name)
      warning("⚠️ Nessuna quota trovata per torneo: ", tournament_name)
    }
  }

  if (length(missing_tournaments) > 0) {
    cat("\n📌 Tornei senza quote e quindi saltati:\n")
    cat(paste0(" - ", missing_tournaments), sep = "\n")
  }

  return(Bet_results)
}


#######################################################################
#Functions that does everything togeter (except the first)

Results_betting <- function (X, Excel_name, n = 1000, tournament = NA, Q = c(0.02, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3),
                             R = c(1.05, 1.10, 1.20, 1.30, 1.40, 1.50, 1.7, 2, 3), Q_max = 1000, R_max = 1000) {
  tournament_datas = data.frame(start_date = character(), sheet = character)

  if(tournament == "Australian Open" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-01-12", "Australian Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-01-14", "Australian Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-01-16", "Australian Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-01-17", "Australian Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-02-08", "Australian Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-01-20", "Australian Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-01-14", "Australian Open 2019"))
  }

  if(tournament == "Roland Garros" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-05-25", "Roland Garros 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-05-26", "Roland Garros 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-05-28", "Roland Garros 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-05-22", "Roland Garros 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-05-30", "Roland Garros 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-09-27", "Roland Garros 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-05-26", "Roland Garros 2019"))

  }

  if(tournament == "US Open" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-08-24", "US Open 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-08-26", "US Open 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-08-28", "US Open 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-08-29", "US Open 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-08-30", "US Open 2021"))
    tournament_datas <- rbind(tournament_datas, c("2020-08-31", "US Open 2020"))
    tournament_datas <- rbind(tournament_datas, c("2019-08-26", "US Open 2019"))

  }

  if(tournament == "Wimbledon" || is.na(tournament)){
    tournament_datas <- rbind(tournament_datas, c("2025-06-30", "Wimbledon 2025"))
    tournament_datas <- rbind(tournament_datas, c("2024-07-01", "Wimbledon 2024"))
    tournament_datas <- rbind(tournament_datas, c("2023-07-03", "Wimbledon 2023"))
    tournament_datas <- rbind(tournament_datas, c("2022-06-27", "Wimbledon 2022"))
    tournament_datas <- rbind(tournament_datas, c("2021-06-28", "Wimbledon 2021"))
    tournament_datas <- rbind(tournament_datas, c("2019-07-01", "Wimbledon 2019"))

  }

  Bet_results <- list()

  for(i in 1:nrow(tournament_datas)){
    torneo <- define_tournament(X, start_date = tournament_datas[i, 1], Serie = "Grand Slam")
    sessantaquattresimi <- torneo$sessantaquattresimi_ord
    risultati <- simulate_tournament(sessantaquattresimi, n)
    probab <- risultati$prob.1
    Bookmakers <- read_excel(Excel_name, sheet = tournament_datas[i, 2])
    colnames(Bookmakers)[7] <- "Player"

    Bet_results[tournament_datas[i, 2]] <- list(Betting_function (probab, Bookmakers, q_values = Q, r_values = R, q_max = Q_max, r_max = R_max))

    cat("Torneo", tournament_datas[i, 2], "completato\n")
  }

  return (Bet_results)
}
###########

#Bilancio finale:
Results_balance <- function (x) {
  balance <- 0
  total_bets <- 0
  won_bets <- 0

  for(i in x) { #per ogni valore della lista
    balance <- balance + i$Balance
    total_bets <- total_bets + i$Num_Bets
    won_bets <- won_bets + i$N_Bets_won
  }

  result <- data.frame(q_Value = x[[1]]$q_Value, r_Value = x[[1]]$r_Value, Balance = round(balance, 2), Num_Bets = total_bets, N_Bets_won = won_bets)
  result$ROI <- (result$Balance / result$Num_Bets) * 100

  result$ROI <- ifelse(is.nan(result$ROI), 0, result$ROI)
  return(result)
}








