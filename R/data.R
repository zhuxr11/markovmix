#' Mixture of Markov chain example
#'
#' A mixture of 2-order Markov chain fit from 100 random sequences
#' with `r length(get_states(object = markov_mix_ex, check = FALSE))` states
#' (`r paste(get_states(object = markov_mix_ex, check = FALSE), sep = ", ")`)
#' and `r ncol(get_counts(object = markov_mix_ex, check = FALSE))` components.
#'
#' @format A \code{\link{MarkovMix}} object.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the dataset.}
#' }
#' @author Xiurui Zhu
"markov_mix_ex"
