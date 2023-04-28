#' @import dplyr tidyr purrr tibble
#' @include utils.R markov_mix_utils.R
NULL

#' MarkovMix class
#'
#' An object of class \code{MarkovMix} is a list containing the following components:
#' \describe{
#'   \item{counts}{Numeric matrix containing soft counts of sub-sequence patterns in each component.
#'   For (non-mixture) Markov chains, the matrix contains only 1 column and counts are actually integers,
#'   but they are still stored as numeric values.}
#'   \item{order}{Integer (1L) as the order of (mixture) Markov chain(s).}
#'   \item{states}{Vector as the states in the (mixture) Markov chain(s).}
#' }
#'
#' @aliases MarkovMix
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the class.}
#' }
#' @author Xiurui Zhu
#'
#' @name MarkovMix-class
NULL

#' Fit mixture of Markov chains
#'
#' \code{fit_markov_mix} fits mixture of Markov chains. It supports high-order Markov chains,
#' multiple sequences and mixture components with cluster probabilities.
#'
#' @param seq_list Sequence list containing vectors of the same class.
#' @param order. Integer (1L) indicating the order of the Markov chain.
#' @param states \code{NULL} or vector indicating the states in the Markov chain.
#' If \code{NULL}, states are inferred from unique non-NA elements in all the sequences.
#' If vector, it should match the class of the sequences. NA elements in the vector are removed.
#' @param clusters \code{NULL} or matrix containing clustering probabilities.
#' If \code{NULL}, Markov chain is fit without mixture components.
#' If matrix, rows are probabilities of sequences and columns are components.
#' As probabilities of sequences, rows are normalized to sum up to 1.
#' @param verbose Logical (1L) indicating whether additional messages should be printed.
#'
#' @return An object of class \code{\link{MarkovMix}}.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @example man-roxygen/ex-fit_markov_mix.R
fit_markov_mix <- function(seq_list, order. = 1L, states = NULL, clusters = NULL, verbose = TRUE) {
  if (is.numeric(order.) == FALSE || length(order.) != 1L || order. <= 0L || order. != round(order.)) {
    stop("[order.] should be a single positive integer")
  }
  if (is.null(clusters) == TRUE) {
    clusters <- matrix(1, nrow = length(seq_list), ncol = 1L)
    if (verbose == TRUE) {
      message("Fitting Markov chain with a single (non-mixture) component ...")
    }
  }
  if (nrow(clusters) != length(seq_list)) {
    stop("Number of rows in [clusters] (", nrow(clusters), ") should match the length of [seq_list] (", length(seq_list), ")")
  }
  invisible(check_seq_list(seq_list = seq_list, states = states))
  seq_factor_res <- seq_list_to_factors(seq_list, states = states)
  seq_factor_list <- seq_factor_res[["seq_list"]]
  states <- seq_factor_res[["states"]]
  preproc_seq_res <- preproc_seq_list(seq_list = seq_factor_list, order. = order.)
  preproc_seq_mat <- preproc_seq_res[["seq_mat"]]
  preproc_seq_idx <- preproc_seq_res[["seq_idx"]]
  seq_score <- calc_seq_score(seq_mat = preproc_seq_mat, states = states)
  clusters <- clusters / rowSums(clusters, na.rm = TRUE)
  count_mat <- matrix(0, nrow = length(states)^(order. + 1L), ncol = ncol(clusters))
  seq_cluster_mat <- clusters[preproc_seq_idx, , drop = FALSE]
  count_mat_no_missing <- colsums_by_group(value_list = split(seq_cluster_mat, as.factor(seq_score)), n_col = ncol(seq_cluster_mat))
  count_mat[sort(unique(seq_score)), ] <- count_mat_no_missing
  res <- list(counts = count_mat,
              order = order.,
              states = states)
  class(res) <- "MarkovMix"
  res
}

#' Print MarkovMix object
#'
#' \code{print.MarkovMix} prints \code{\link{MarkovMix}} object in a user-friendly form,
#' including component priors and transition matrices.
#'
#' @importFrom pillar pillar
#'
#' @param x \code{\link{MarkovMix}} object.
#' @param sep Character (1L) used as separator between states in the row names of transition matrix.
#' @param print_max,print_min Integers as the numbers of rows to print each transition matrix.
#' See \code{\link[pillar]{pillar_options}} for details.
#' @param ... Currently ignored for this method.
#'
#' @return Input \code{x}, invisibly.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix methods
#'
#' @example man-roxygen/ex-fit_markov_mix.R
print.MarkovMix <- function(x, sep = "->", print_max = 20L, print_min = 10L, ...) {
  count_mat <- get_counts(object = x, check = FALSE)
  order. <- get_order(object = x, check = FALSE)
  states <- get_states(object = x, check = FALSE)
  states_df <- get_states_mat(object = x, check = FALSE) %>%
    as.data.frame()
  n_comp <- ncol(count_mat)
  dest_colname <- dplyr::last(colnames(states_df))
  if (n_comp == 0L) {
    stop("No valid components in MarkovMix object")
  } else if (n_comp == 1L) {
    cat("This is a ", order., "-order Markov chain.\n", sep = "")
  } else {
    cat("This is a ", n_comp, "-component mixture of ", order., "-order Markov chains.\n", sep = "")
  }
  prob_mat <- get_prob(object = x, check = FALSE)
  comp_prior <- get_prior(object = x, check = FALSE)
  purrr::walk(
    seq_len(n_comp),
    ~ {
      trans_mat_comp <- states_df %>%
        tibble::add_column(.prob = prob_mat[, .x, drop = TRUE]) %>%
        tidyr::unite(col = ".row_name", !dplyr::all_of(c(dest_colname, ".prob")), sep = sep) %>%
        tidyr::pivot_wider(id_cols = ".row_name",
                           names_from = dplyr::all_of(dest_colname),
                           values_from = ".prob",
                           values_fill = 0) %>%
        tibble::column_to_rownames(".row_name") %>%
        as.matrix()
      trans_mat_comp_norm <- trans_mat_comp / rowSums(trans_mat_comp, na.rm = TRUE)
      if (n_comp > 1L) {
        cat("\nComponent ", .x, ": prior = ", comp_prior[.x], "\n", sep = "")
      }
      cat("Transition matrix:\n")
      if (nrow(trans_mat_comp_norm) <= print_max) {
        print(trans_mat_comp_norm)
      } else {
        print(head(trans_mat_comp_norm, n = print_min))
        cat("# ... ", format(nrow(trans_mat_comp_norm) - print_min, big.mark = ","), " more ",
            if (nrow(trans_mat_comp_norm) == print_min + 1L) "row" else "rows",
            " in transition matrix ...\n", sep = "")
      }
    }
  )
  invisible(x)
}

#' Predict probabilities with MarkovMix object and new sequence list
#'
#' \code{predict.MarkovMix} predicts probabilities with \code{\link{MarkovMix}} object
#' and new sequence list. NA values are returned for sequences with no valid sub-sequences
#' to distinguish them from those that are truly not observed (probabilities = 0)
#' in the transition matrices.
#'
#' @param object \code{\link{MarkovMix}} object.
#' @param newdata Sequence list containing vectors of the same class.
#' @param aggregate. Logical (1L) indicating whether probabilities
#' from each component should be weighted mean by component priors
#' (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Currently ignored for this method.
#'
#' @return For \code{aggregate. = TRUE}, a numeric vector of probabilities.
#' For \code{aggregate. = TRUE}, a numeric matrix of probabilities
#' from each component.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @family MarkovMix methods
#'
#' @example man-roxygen/ex-predict_markov_mix.R
predict.MarkovMix <- function(object, newdata, aggregate. = TRUE, ...) {
  prob_mat <- get_prob(object = object, check = FALSE)
  order. <- get_order(object = object, check = FALSE)
  states <- get_states(object = object, check = FALSE)
  seq_factor_res <- seq_list_to_factors(newdata, states = states)
  seq_factor_list <- seq_factor_res[["seq_list"]]
  states <- seq_factor_list[["states"]]
  preproc_seq_res <- preproc_seq_list(seq_list = seq_factor_list, order. = order.)
  preproc_seq_mat <- preproc_seq_res[["seq_mat"]]
  preproc_seq_idx <- preproc_seq_res[["seq_idx"]]
  seq_score <- calc_seq_score(seq_mat = preproc_seq_mat, states = states)
  seq_prob_mat_log <- log(prob_mat[seq_score, , drop = FALSE])
  res_comp <- matrix(NA_real_, nrow = length(newdata), ncol = ncol(seq_prob_mat_log))
  res_comp_log_no_missing <- colsums_by_group(split(seq_prob_mat_log, as.factor(preproc_seq_idx)), n_col = ncol(seq_prob_mat_log))
  res_comp[sort(unique(preproc_seq_idx)), ] <- exp(res_comp_log_no_missing)
  if (aggregate. == FALSE) {
    res <- res_comp
  } else {
    comp_prior <- get_prior(object = object, check = FALSE)
    res <- colSums(t(res_comp) * comp_prior) / sum(comp_prior)
  }
  res
}
