#' Simulating the "Monty Hall" Game
#'
#' In the game, a prize is located behind one of 3 doors (or in one of 3 boxes). The
#' player makes a guess, after which the game owner opens one door/box that is neither
#' the winner nor the guess.  The player can then make a second guess.  This function
#' runs the game \code{N} times.
#'
#' The assumption is that both the prize location and the first guess are sampled
#' uniformly.  Also, if the owner has a choice, the open is also sampled.
#'
#' The point is to show that the second guess has twice the winning chance of the
#' first.  Sometimes framed as Bayesian inference (Bain, 2016) but in fact
#' obvious if the event space is partitioned relevantly (as the example
#' illustrates).
#'
#' @return  A data frame with columns for the first guess, the opened
#' door/box, the second guess and the location of the prize.
#' @param N sample size; i.e., number of times to run the simulation of the game.
#' @references
#' Chambers, John M. (2016)
#' \emph{Extending R},
#' Chapman & Hall/CRC.
#'
#' Bain, Robert. (2016)
#' \dQuote{Are our brains Bayesian?}
#' \emph{Significance}, vol. 13, issue 4, pp 14-19.
#' @examples
#' set.seed(378)
#' N <- 10000
#' mh <- montyHall(N)
#' x <- mh[mh$guess1 != mh$prize,] # guess1 was wrong
#' all(x$guess2 == x$prize) # => guess2 was right
#' sum(mh$guess1 == mh$prize)/N ## probability 1/3
#' sum(mh$guess2 == mh$prize)/N ## probability 2/3
#' @author John M. Chambers
#' @keywords Programming
montyHall <- function(N) {
  guess1 <- sample(1:3, N, TRUE)
  prize <- sample(1:3, N, TRUE)
  ok <- matrix(NA, 3, N)
  for (j in 1:3)
    ok[j, ] <- guess1 != j & prize != j
  x <- matrix(1:3, 3, N)
  open <- sapply(1:N, function(i){
    y <- x[, i][ok[, i]]
    if(length(y)>1L) sample(y,1L) else y
  })
  guess2 <- integer(N)
  for (j in 1:3)
    guess2[guess1 != j & open != j] <- j
  data.frame(guess1 = guess1, guess2 = guess2,
        prize = prize, open = open)
}
