#' Wordle Evaluator
#' 
#' This is a core function of the package.  It takes the name of a bot and a single five letter "wordleWord" and generates a score
#' equal to the number of tries is takes to guess the word, or the failScore (default = 10) if the puzzle is not solved.
#' If a guess is not in the validWordVector then it is a "bad word".  If the bot guesses maxBadWords that is considered a failure.
#' 
#' To retrieve a guess from the bot function, wordleEvaluator calls the bot function with two parameters (so the bot must 
#' accept 2 parameters).  The first parameter is a list.  The second is a validWordVector.  The list will have three elements matching 
#' the output of the wordleEvaluator function.  
#' 
#' wordleEvaluator returns a list with 3 elements.  The first element is 'response' a character vector. Initially this is NULL.  After each guess by 
#' the bot, a string will be added to the response vector.  The string will be five characters consisting of 0s, 1s and 2s.  
#' A 0 represents a letter not in the word. A 1 represents a letter in the word but in the wrong location and a 2 is a letter in the correct location. So
#' a correct guess would return "22222".  A guess of "ABCDE" when the WordleWord is "VWXYZ" would be "00000" while
#' "ABCDE" guessed for "EDCBA" would return "11211".
#' 
#' The 'guesses' element in the list that is returned is a character vector of the guesses provided to the wordleEvaluator function by the bot. Initially
#' guesses will be NULL.
#'
#' The 'score' element is an integer from 1 to 6 if the bot successfully solved the puzzle.  The value represents which try was successful. The
#' score will be the failScore (default = 10) is the bot was unsuccessful.
#' 
#' Hint/Suggestion:  The bot function may want to test either the response or the guesses elements for being NULL. If NULL, then the bot should return its
#' first guess.
#' 
#' The concept of this package is that this function will be called for many (e.g. 1000) words in the validWordVector for each bot in a challenge. The 
#' bot with the lowest average wins.
#'
#' @param bot Name of function to use to solve wordle puzzles
#' @param wordleWord The world which is the solution to the puzzle
#' @param validWordVector A vector of valid 5 letter words
#' @param maxBadWords Integer representing maximum illegal words which may be attempted
#' @param failScore Score if the bot cannot solve the puzzle.
#'
#' @return List with 3 elements:  response, guesses, score
#' @export
#'
#' @examples \dontrun{wordleEvaluator(exampleBot, "CYNIC", validWordVector)}
wordleEvaluator <- function(bot, wordleWord, validWordVector, maxBadWords = 100, failScore = 10) {
  out = list(response = NULL,
             guesses = NULL,
             score = numeric())
  wordleWord <- toupper(wordleWord)
  guess <- getGuess(bot, out, validWordVector, maxBadWords)
  for(i in 1:6){
    wordResult <- testWord(guess, wordleWord)
    out$response <- c(out$response, wordResult)
    out$guesses <- c(out$guesses, guess)
    if(wordResult == "22222") {
      out$score <- i
      return(out)
    } else {
      guess <- getGuess(bot, out, validWordVector, maxBadWords)
    }
  }
  out$score <- failScore
  return(out)
}

#' Get a guess (word) from a bot
#'
#' @param bot Name of bot
#' @param wordleEvaluatorResponse list representing the response from the wordleEvaluator function.
#' @param validWordVector vector of valid words
#' @param maxBadWords Integer representing maximum illegal words which may be attempted
#'
#' @return string which is the guess of a bot
#'
getGuess <- function(bot, wordleEvaluatorResponse, validWordVector, maxBadWords = 100) {
  badWordCount <- 0
  guess <- toupper(bot(wordleEvaluatorResponse, validWordVector))
  while(!(guess %in% validWordVector)){
    badWordCount <- badWordCount + 1
    if(badWordCount >= maxBadWords){
      return(NULL)
    }
    guess <- toupper(bot(wordleEvaluatorResponse, validWordVector))
  }
  return(guess)
}

#' Test a guess
#' 
#' A five character string will be returned consisting of 0s, 1s and 2s.  A 0 represents a letter not in the word.
#' A 1 represents a letter in the word but in the wrong location and a 2 is a letter in the correct location. So
#' a correct guess would return "22222".  A guess of "ABCDE" when the Wordle is "VWXYZ" would be "00000" while
#' "ABCDE" guessed for "EDCBA" would return "11211"
#'
#' @param guess Word suggested by bot (a guess)
#' @param wordleWord Word to be found (the correct Wordle answer)
#' @param n length of a word (default = 5)
#'
#' @return five character string
testWord <- function(guess, wordleWord, n = 5){
  if(class(guess) != "character") {
    stop("Bad guess error. ", guess, " is not a character")
  } else if(nchar(guess) != n) {
    stop("Length of guess is wrong. ", guess, " is not ", n, " characters.")
  }
  guess <- toupper(guess)
  wordleWord <- toupper(wordleWord)
  out <- character(0)
  for(i in 1:nchar(guess)) {
    guessLetter <- substr(guess, i, i)
    if (guessLetter == substr(wordleWord, i, i)){
      out <- paste0(out, "2")
    } else if(grepl(guessLetter, wordleWord, fixed = TRUE)) {
      out <- paste0(out, "1")
    } else {
      out <- paste0(out, "0")
    }
  }
  return(out)
}

#' Wordle Challenge 
#' 
#' This is the main function of the package.  It takes a list of bots and a list (vector) of five letter words and generates an
#' average score for each bot. Essentially it repeatedly calls the wordleEvaluator function for each bot for each word.
#'
#' @param botList list of bots
#' @param challengeWords A character vector of five letter words each bot is to try to guess.
#' @param wordList A character vector of valid words.  
#' 
#' @return List with three elements.  'averageScores' is a data frame with a row for each bot and columns for the name of the bot,
#' the AverageScore of the bot, and the Rank of the bot.  'scores' is a data frame with a row for each challengeWords and columns for 
#' the challenge word for each bot's score for that word. 
#' @export
#'
#' @examples \dontrun{wordleChallenge(botList, challengeWords, wordList)}

wordleChallenge <- function(botList, challengeWords, wordList) {
  nBots <- length(botList)
  if(is.null(names(botList))) names(botList) <- paste0("bot", 1:nBots)
  out <- list(averageScores = data.frame(botName = names(botList),
                                         AverageScore = numeric(nBots)),
              scores = cbind(challengeWords, data.frame(matrix(0, ncol = nBots, nrow = length(challengeWords), 
                                                               dimnames = list(NULL, names(botList))))))
  for(i in 1:nBots){
    out$scores[, i + 1] <- sapply(challengeWords, function(x) wordleEvaluator(botList[[i]], x, wordList)$score)
    out$averageScores[i, 2] <- mean(out$scores[, i + 1])
  }
  out$averageScores <- out$averageScores %>% mutate(Rank = rank(AverageScore, ties.method = "average"))
  winningIdx <- which.min(out$averageScores$Rank)
  out$winner <- paste("Winner is", out$averageScores$botName[winningIdx], "with an average score of", out$averageScores$AverageScore[winningIdx])
  return(out)
}