#' @import dplyr tidyr purrr tibble
#' @include utils.R
NULL

#' Get state transition patterns from MarkovFit object
#'
#' \code{get_states_mat} gets state transition patterns from \code{\link{MarkovMix}} object.
#' The number of columns is the order of the (mixture of) Markov chain(s) plus 1 (the destination state).
#' Each column is arranged in the ascending order of the states.
#' The last column serves as the destination state and iterates the fastest.
#'
#' @inheritParams get_prob
#'
#' @return A matrix indicating the state transition patterns.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the class.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family \code{\link{MarkovMix}} utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_states_mat <- function(object, check = TRUE) {
  if (check == TRUE && identical(class(object), "MarkovMix") == FALSE) {
    stop("[object] should be a MarkovMix object")
  }
  states_df <- do.call(expand.grid, replicate(object[["order"]] + 1L, object[["states"]], simplify = FALSE)) %>%
    as.data.frame() %>%
    dplyr::mutate_all(~ factor(.x, levels = object[["states"]])) %>%
    dplyr::arrange_all()
  states_mat <- as.matrix(states_df)
  dimnames(states_mat) <- NULL
  states_mat
}

#' Get probability matrix from MarkovFit object
#'
#' \code{get_prob} gets probability matrix from \code{\link{MarkovMix}} object.
#' It normalizes each column in the count matrix to sum up to 1.
#'
#' @param object \code{\link{MarkovMix}} object.
#' @param check Logical (1L) indicating whether to check \code{object}
#' at the beginning.
#'
#' @return A numeric matrix indicating probabilities of each state transition pattern in each component.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the class.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family \code{\link{MarkovMix}} utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_prob <- function(object, check = TRUE) {
  if (check == TRUE && identical(class(object), "MarkovMix") == FALSE) {
    stop("[object] should be a MarkovMix object")
  }
  count_mat <- object[["counts"]]
  t(t(count_mat) / colSums(count_mat, na.rm = TRUE))
}

#' Get component priors from MarkovFit object
#'
#' \code{get_prior} gets component priors from \code{\link{MarkovMix}} object,
#' normalized to sum up to 1.
#'
#' @inheritParams get_prob
#'
#' @return A numeric vector indicates component priors.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the class.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family \code{\link{MarkovMix}} utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_prior <- function(object, check = TRUE) {
  if (check == TRUE && identical(class(object), "MarkovMix") == FALSE) {
    stop("[object] should be a MarkovMix object")
  }
  count_mat <- object[["counts"]]
  comp_prior <- colSums(count_mat, na.rm = TRUE)
  comp_prior / sum(comp_prior, na.rm = TRUE)
}
