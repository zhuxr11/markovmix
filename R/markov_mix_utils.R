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
#' @param object \code{\link{MarkovMix}} object.
#' @param check Logical (1L) indicating whether to check \code{object}
#' at the beginning.
#'
#' @return A matrix indicating the state transition patterns.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_states_mat <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  states_df <- do.call(expand.grid,
                       replicate(get_order(object = object, check = FALSE) + 1L,
                                 get_states(object = object, check = FALSE),
                                 simplify = FALSE)) %>%
    as.data.frame() %>%
    dplyr::mutate_all(~ factor(.x, levels = get_states(object = object, check = FALSE))) %>%
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
#' @inheritParams get_states_mat
#'
#' @return A numeric matrix indicating probabilities of each state transition pattern in each component.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_prob <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  count_mat <- get_counts(object = object, check = FALSE)
  t(t(count_mat) / colSums(count_mat, na.rm = TRUE))
}

#' Get component priors from MarkovFit object
#'
#' \code{get_prior} gets component priors from \code{\link{MarkovMix}} object,
#' normalized to sum up to 1.
#'
#' @inheritParams get_states_mat
#'
#' @return A numeric vector indicates component priors.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_prior <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  count_mat <- get_counts(object = object, check = FALSE)
  comp_prior <- colSums(count_mat, na.rm = TRUE)
  comp_prior / sum(comp_prior, na.rm = TRUE)
}

#' Get transition pattern counts from MarkovFit object
#'
#' \code{get_counts} gets transition pattern counts from \code{\link{MarkovMix}} object.
#'
#' @inheritParams get_states_mat
#'
#' @return A numeric matrix indicates transition pattern (soft) counts,
#' where each row corresponds to a transition pattern and each column
#' corresponds to a component.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.2 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_counts <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  object[["counts"]]
}

#' Get the order of Markov chains
#'
#' \code{get_order} gets the order of Markov chains from \code{\link{MarkovMix}} object.
#'
#' @inheritParams get_states_mat
#'
#' @return An integer as the order of Markov chains.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.2 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_order <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  object[["order"]]
}

#' Get the states of Markov chains
#'
#' \code{get_states} gets the states of Markov chains from \code{\link{MarkovMix}} object.
#'
#' @inheritParams get_states_mat
#'
#' @return A vector as the states used in Markov chains.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.2 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
get_states <- function(object, check = TRUE) {
  if (check == TRUE) {
    .check_MarkovMix(object = object)
  }
  object[["states"]]
}

#' Extract or replace components of MarkovMix object
#'
#' Operators to extract or replace components of a \code{\link{MarkovMix}} object.
#'
#' @aliases `[.MarkovMix`
#'
#' @param x \code{\link{MarkovMix}} object.
#' @param i Indices specifying components to extract or replace.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.1 Xiurui Zhu - Initiate the functions.}
#'   \item{0.1.2 Xiurui Zhu - Update function documentation.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-markov_mix_utils.R
#'
#' @name Extract.MarkovMix
`[.MarkovMix` <- function(x, i) {
  res <- x
  res[["counts"]] <- get_counts(object = x, check = FALSE)[, i, drop = FALSE]
  res
}

#' @aliases `[<-.MarkovMix`
#' @param value Numeric matrix as soft counts for transition patterns (like \code{get_counts(object = x)}),
#' whose rows correspond to the rows in \code{get_states_mat(x)}
#' and columns correspond to the number of components to replace.
#' @export
#' @rdname Extract.MarkovMix
`[<-.MarkovMix` <- function(x, i, value) {
  if (is.matrix(value) == FALSE) {
    value <- as.matrix(value)
  }
  if (nrow(value) != 1L && nrow(value) != nrow(get_counts(object = x, check = FALSE))) {
    stop("Number of rows in [value] (", nrow(value), ") is not 1 (recycled) ",
         "or the same as length([states])^([order] + 1L) (",
         length(get_states(object = x, check = FALSE))^(get_order(object = x, check = FALSE) + 1L) , ")")
  }
  res <- x
  res[["counts"]][, i] <- value
  res
}

#' Reorganize states in MarkovMix object
#'
#' \code{restate} reorganizes states in \code{\link{MarkovMix}} object with a function.
#'
#' @importFrom rlang inject !!!
#'
#' @param .object \code{\link{MarkovMix}} object.
#' @param .fun Function to process each column in state transition patterns as factors,
#' such as those in \code{\link[forcats:forcats-package]{forcats}} package.
#' @param .check Logical (1L) indicating whether to check \code{object}
#' at the beginning.
#' @param ... Additional arguments passed on to \code{.fun}.
#'
#' @return A \code{\link{MarkovMix}} object with modified states and count matrix.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix utilities
#'
#' @example man-roxygen/ex-restate.R
restate <- function(.object, .fun, .check = TRUE, ...) {
  if (.check == TRUE) {
    .check_MarkovMix(object = .object)
  }
  .fun <- rlang::as_function(.fun)
  args_list <- list(...)
  states_mat <- get_states_mat(object = .object, check = FALSE)
  states_mat_new <- states_mat %>%
    as.data.frame() %>%
    tibble::rowid_to_column(".row_id") %>%
    dplyr::mutate_at(dplyr::vars(!c(".row_id")), ~ rlang::inject(.fun(.x, !!!args_list))) %>%
    tidyr::drop_na(!c(".row_id")) %>%
    dplyr::arrange_at(dplyr::vars(!c(".row_id")))
  # Check level consistency
  new_levels <- levels(dplyr::select(states_mat_new, -dplyr::all_of(".row_id"))[[1L]])
  purrr::walk2(
    dplyr::select(states_mat_new, -dplyr::all_of(".row_id"))[-1L],
    seq_len(ncol(states_mat_new) - 1L)[-1L],
    ~ if (identical(levels(.x), new_levels) == FALSE) {
      stop("Factor levels inconsistency at variable ", .y)
    }
  )
  states_mat_new_group_id <- states_mat_new %>%
    dplyr::group_by_at(dplyr::vars(!c(".row_id"))) %>%
    dplyr::group_indices()
  count_mat <- get_counts(object = .object, check = FALSE)
  count_mat_new <- count_mat[states_mat_new[[".row_id"]], , drop = FALSE] %>%
    split(states_mat_new_group_id) %>%
    colsums_by_group(ncol(count_mat))
  res <- .object
  res[["counts"]] <- count_mat_new
  res[["states"]] <- new_levels
  res
}
