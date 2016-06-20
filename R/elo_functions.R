
elo_fibs <- function(winner, loser, matchlength){
  # for debuggng: winner = 1900; loser = 1500; matchlength = 7
  prob <- 1 - (1 / (10 ^ ((loser - winner) * sqrt(matchlength) / 2000) + 1))
  change <- 4 * sqrt(matchlength) * prob
  return(c(winner_new = winner + change, loser_new = loser - change))
  
}


rate_tournament <- function(match_results, starting_ratings = NULL){
  # match_results is a data frame with columns "Won" (name of winner), "Lost" (name of loser),
  # "Length" (match length) and Date.
  # starting_ratings is a data frame of original ratings
  
  # If no date, we pretend it all happened today:
  if(is.null(match_results$Date)){
    match_results$Date <- Sys.Date()
  }
  
  # If no match lengths given, we assume they were all one pointers:
  if(is.null(match_results$Length)){
    match_results$Length <- 1
  }
  
  # Check who has got a starting rating, and any missing get 1500
  AllPlayers <- unique(c(as.character(match_results$Won), 
                         as.character(match_results$Lost)))
  
  if(is.null(starting_ratings)){
    starting_ratings <- data.frame(
      Player = AllPlayers,
      Rating = 1500,
      stringsAsFactors =FALSE
      )
  }
  
  starting_ratings$Player <- as.character(starting_ratings$Player)
  
  NoRating <- AllPlayers[!AllPlayers %in% unique(starting_ratings$Player)]
  if(length(NoRating) > 0){
    starting_ratings <- rbind(starting_ratings, 
                              data.frame(
                                Player = NoRating,
                                Rating = 1500,
                                stringsAsFactors = FALSE))
  }
  
  
  # set up variables to hold results
    latest_ratings <- starting_ratings
    full_results <- list()
  
    for (i in 1:nrow(starting_ratings)){
    full_results[[starting_ratings[i, "Player"]]] <- data.frame(
      Rating = starting_ratings[i, "Rating"],
      Date = min(match_results$Date),
      Played = as.character(""))
    }
    names(full_results) <- starting_ratings$Player

  
  # main sequence starts here
  for (i in 1:nrow(match_results)){
    # each row of match_results represents a match won by the player listed under Won
    ThisWinner <-  as.character(match_results[i, "Won"])
    ThisLoser <- as.character(match_results[i, "Lost"])
    new_ratings <- elo_fibs(winner = latest_ratings[latest_ratings$Player == ThisWinner, "Rating"],
                            loser  = latest_ratings[latest_ratings$Player ==ThisLoser, "Rating"],
                            matchlength = match_results[i, "Length"])
    latest_ratings[latest_ratings$Player == ThisWinner, "Rating"] <- new_ratings[1]
    latest_ratings[latest_ratings$Player == ThisLoser, "Rating"] <- new_ratings[2]
  
    full_results[[ThisWinner]] <- rbind(full_results[[ThisWinner]],
                                        data.frame(Rating = new_ratings[1],
                                                   Date = match_results[i, "Date"],
                                                   Played = ThisLoser,
                                                   stringsAsFactors = FALSE))
    full_results[[ThisLoser]] <- rbind(full_results[[ThisLoser]],
                                        data.frame(Rating = new_ratings[2],
                                                   Date = match_results[i, "Date"],
                                                   Played = ThisWinner,
                                                   stringsAsFactors = FALSE))
  }

  # cleanup, remove silly automatically generated row names
  for(i in 1:length(full_results)){
    row.names(full_results[[i]]) <- NULL
  }
    
    latest_ratings$Rating <- round(latest_ratings$Rating, 1)
    row.names(latest_ratings) <- 1:4

  
  return(list(full_results = full_results, 
              latest_ratings = latest_ratings[order(-latest_ratings$Rating), ]))
}


#=================Testing=======================

# match_results <- data.frame(Won = c("A", "A", "A", "B", "B", "C"),
#                             Lost = c("B", "B", "C", "D", "A", "B"),
#                             Length = c(7, 7, 7, 11, 11, 15),
#                             Date = as.Date("2014/09/20") + 1:6)
# starting_ratings <- data.frame(Player = c("A", "B", "C", "D"),
#                                     Rating = c(1500, 1900, 1720, 1420))
# 
# rate_tournament(match_results, starting_ratings)
# 
# 
# X <- data.frame(Won = c(rep("P", 10), "A"),
#                 Lost = c(rep("A", 10), "P"))
# Y <- data.frame(Player = "P", Rating = 1900)
# rate_tournament(X, Y)
