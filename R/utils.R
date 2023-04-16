#' @import dplyr tidyr purrr tibble forcats
NULL

#' Helper function to check input sequence list
#'
#' \code{seq_list_to_factors} checks input sequence list.
#'
#' @param seq_list Sequence list.
#' @param states States used as factor levels.
#'
#' @return Invisible \code{NULL}.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @noRd
check_seq_list <- function(seq_list, states = NULL) {
  if (is.list(seq_list) == FALSE) {
    stop("[seq_list] should be a list of vectors")
  }
  if (length(seq_list) == 0L) {
    stop("[seq_list] does not contain any sequences (length 0)")
  }
  seq_is_vector_lgl <- purrr::map_lgl(seq_list, is.vector)
  if (any(seq_is_vector_lgl == FALSE)) {
    stop("All elements in [seq_list] should be vectors, exception(s) at index(es): ",
         paste0(which(seq_is_vector_lgl), collapse = ", "))
  }
  ref_class <- class(seq_list[[1L]])
  if (is.null(states) == FALSE) {
    ref_class <- class(states)
  } else {
    ref_class <- class(seq_list[[1L]])
  }
  seq_class_consist_lgl <- purrr::map_lgl(seq_list, ~ identical(class(.x), ref_class))
  if (any(seq_class_consist_lgl == FALSE)) {
    stop("Class of elements should be consistent with ",
         if (is.null(states) == FALSE) "[states]" else "each other",
         ", exception(s) at index(es): ",
         paste0(which(seq_class_consist_lgl), collapse = ", "))
  }
  invisible(NULL)
}

#' Helper function to convert sequences to factors in a list
#'
#' \code{seq_list_to_factors} converts sequences to factors in a list.
#'
#' @param seq_list Sequence list.
#' @param states States used as factor levels.
#' @param verbose Logical indicating whether messages should be printed.
#'
#' @return A list of \code{seq_list} containing a list of factors
#' and \code{states} containing states.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @noRd
seq_list_to_factors <- function(seq_list, states = NULL, verbose = TRUE) {
  if (is.null(states) == TRUE) {
    states <- seq_list %>%
      unlist() %>%
      unique() %>%
      as.factor() %>%
      levels()
    if (verbose == TRUE) {
      message("Generating states from sequence list: ", paste0(states, collapse = TRUE))
    }
  } else {
    states_bk <- states
    states <- states_bk[is.na(states_bk) == FALSE]
    if (identical(states, states_bk) == FALSE) {
      message("Refining [states]: ", paste(states_bk, collapse = ", "),
              " -> ", paste(states, collapse = ", "))
    }
  }
  if (length(states) == 0L) {
    stop("No valid [states] found")
  }
  list(seq_list = purrr::map(seq_list, factor, levels = states),
       states = states)
}

#' Helper function to preprocess factor sequence list
#'
#' \code{preproc_seq_list} preprocess factor sequence list by removing invalid sequences
#' and generate sequence indices.
#'
#' @param seq_list List of sequences, with each element converted to a factor.
#'
#' @return List of \code{seq_mat} containing matrix of sub-sequences
#' and \code{seq_idx} containing sequence indices.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @noRd
preproc_seq_list <- function(seq_list, order.) {
  seq_split_res <- split_seq(seq_list, order. + 1L)
  seq_idx_raw <- purrr::map_int(seq_list, length) - order.
  seq_idx_raw <- pmax(seq_idx_raw, 1L)
  seq_idx_raw <- rep(seq_along(seq_list), seq_idx_raw)
  seq_df <- do.call(rbind, purrr::flatten(seq_split_res)) %>%
    as.data.frame() %>%
    tibble::add_column(.seq_idx = seq_idx_raw) %>%
    tidyr::drop_na(!c(".seq_idx"))
  if (nrow(seq_df) == 0L) {
    stop("[seq_list] does not contain any valid sub-sequences >= length ", order.)
  }
  seq_mat <- seq_df %>%
    dplyr::select(-.seq_idx) %>%
    as.matrix()
  dimnames(seq_mat) <- NULL
  list(seq_mat = seq_mat, seq_idx = seq_df[[".seq_idx"]])
}

#' Calculate sequence score from sub-sequence matrix
#'
#' \code{calc_seq_score} calculates sequence scores from sub-sequence matrix,
#' e.g. for 2-column sub-sequence matrix with states A, B and C, sub-sequence
#' A-A is 1, A-B is 2, A-C is 3, B-A is 4, ...
#'
#' @param seq_mat Sub-sequence matrix
#' @param states Sequence states.
#'
#' @return An integer vector as sequence scores.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @noRd
calc_seq_score <- function(seq_mat, states) {
  colSums(t(seq_mat - 1L) * length(states)^(rev(seq_len(ncol(seq_mat))) - 1L), na.rm = TRUE) + 1L
}
