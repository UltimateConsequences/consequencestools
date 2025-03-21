#' Calculate Share of Largest N Values
#'
#' @param datavector A numeric vector of values
#' @param rank Integer specifying the number of largest values to consider (default: 1)
#'
#' @return A numeric value representing the share of the sum of the largest n values compared to the total sum
#'
#' @description
#' This function calculates the share of the sum of the largest n values in
#' a vector compared to the total sum of all values in the vector.
#'
#' @details
#' The function sorts the input vector in descending order, sums the first n
#' values (where n is specified by the rank parameter), and divides this by
#' the total sum of the vector.
#'
#' @examples
#' share_of_largest_n(c(10, 5, 3, 2), rank = 1)
#' share_of_largest_n(c(10, 5, 3, 2), rank = 2)
#'
#' @export
share_of_largest_n <- function(datavector, rank=1) {
  sum_all <- sum(datavector, na.rm=TRUE)
  stopifnot(rank <= length(datavector))
  if(sum_all == 0)
    stop("Sum of datavector is zero. Cannot calculate share.")
  datavector <- sort(datavector, decreasing = TRUE)
  share <- sum(datavector[1:rank], na.rm=TRUE)/sum_all
  share
}

#' Calculate Number of Observations to Reach a Given Share
#'
#' @param datavector A numeric vector of values
#' @param share Numeric value between 0 and 1 representing the target share
#'
#' @description
#' This function determines how many of the largest observations in a vector are needed to reach a specified share of the total sum.
#'
#' @details
#' The function sorts the input vector in descending order and iteratively checks how many of the largest values are needed to exceed the specified share.
#'
#' @return An integer representing the number of largest values needed to
#'   reach the target share
n_observations_to_reach_share <- function(datavector, share=0.5) {
  assert_that((share>0)&(share<=1))
  assert_that(length(datavector)>0)
  if(any(datavector<0, na.rm = TRUE))
    warning("Negative values in datavector. Results may be misleading.")
  datavector <- sort(datavector, decreasing = TRUE)
  for(i in 1:length(datavector)){
    if (share_of_largest_n(datavector, i)>=share) break
  }
  i
}
