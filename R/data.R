#' Mixture of Markov chain example
#'
#' A mixture of 2-order Markov chain fit from 100 random sequences
#' with `r length(markov_mix_ex[["states"]])` states
#' (`r paste(markov_mix_ex[["states"]], sep = ", ")`)
#' and `r ncol(markov_mix_ex[["counts"]])` components.
#'
#' @format A \code{\link{MarkovMix}} object.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the dataset.}
#' }
#' @author Xiurui Zhu
"markov_mix_ex"
