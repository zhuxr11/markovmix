library(testthat)

test_that("fit_markov_mix fits mixture of Markov chains", {
  # Sequence preparation
  test_states <- seq_len(4L)
  test_maxlen <- 10L
  set.seed(1111L)
  test_seq <- purrr::map(
    seq_len(100L),
    ~ sample(test_states, sample.int(test_maxlen, 1L), replace = TRUE)
  )

  # Markov chain
  expect_message(markov_fit <<- fit_markov_mix(
    seq_list = test_seq,
    order. = 1L,
    states = test_states
  ), "a single.*component")
  # Infer states from sequences
  expect_message(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L
  ), "Generating states from sequence list")
  # Remove NA values in states
  expect_message(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L,
    states = c(test_states, NA_integer_)
  ), "Refining \\[states\\]")
  # Remove duplicated values in states
  expect_message(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L,
    states = rep(test_states, 2L)
  ), "Refining \\[states\\]")
  # Error if seq_list is not list
  expect_error(fit_markov_mix(
    seq_list = unlist(test_seq),
    order. = 1L,
    states = test_states
  ), "should be a list")
  # Error if seq_list is length 0
  expect_error(fit_markov_mix(
    seq_list = list(),
    order. = 1L,
    states = test_states
  ), "does not contain any sequences")
  # Error if sequences are not vectors
  expect_error(fit_markov_mix(
    seq_list = purrr::map(test_seq, as.matrix),
    order. = 1L,
    states = test_states
  ), "All elements in \\[seq_list\\] should be vectors")
  # Error if states type mismatch
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L,
    states = as.character(test_states)
  ), "Class of elements should be consistent with.*\\[states\\]")
  # Error if states are empty
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L,
    states = integer()
  ), "No valid \\[states\\] found")
  # Error if no valid sequences
  expect_error(fit_markov_mix(
    seq_list = as.list(test_states),
    order. = 1L,
    states = test_states
  ), "does not contain any valid sub\\-sequences")
  # Errors on [order.]
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 0L,
    states = test_states
  ), "single positive integer")
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 1L:2L,
    states = test_states
  ), "single positive integer")
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 1.5,
    states = test_states
  ), "single positive integer")
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = "2",
    states = test_states
  ), "single positive integer")

  # Cluster preparation
  test_n_comp <- 3L
  test_clusters <- matrix(
    runif(length(test_seq) * test_n_comp),
    nrow = length(test_seq),
    ncol = test_n_comp
  )

  # Mixture of Markov chains
  expect_no_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 2L,
    states = test_states,
    clusters = test_clusters
  ))
  # Errors on number of rows in clusters
  expect_error(fit_markov_mix(
    seq_list = test_seq,
    order. = 2L,
    states = test_states,
    clusters = test_clusters[-1L, , drop = FALSE]
  ), "Number of rows.*should match the length")
})

test_that("print.MarkovMix prints MarkovMix objects", {
  # Load example MarkovMix object
  data("markov_mix_ex")

  # Markov chain or mixture of them
  expect_no_match(capture.output(print(markov_fit)), "component")
  expect_output(print(markov_mix_ex), "component")

  # Number of rows to print
  expect_no_match(capture.output(print(markov_mix_ex)), "more row")
  expect_output(print(markov_mix_ex, print_max = 12L), "more rows")
  expect_output(print(markov_mix_ex, print_max = 15L, print_min = 15L), "more row(?!=s)", perl = TRUE)
})

test_that("predict.MarkovMix predicts MarkovMix objects", {
  # Load example MarkovMix object
  data("markov_mix_ex")

  # Generate a new list of sequences
  set.seed(2222L)
  new_maxlen <- 8L
  new_seq_list <- purrr::map(
    seq_len(50L),
    ~ sample(markov_mix_ex[["states"]], sample.int(10L, 1L), replace = TRUE)
  )
  new_seq_list_short <- purrr::map(
    seq_len(50L),
    ~ sample(markov_mix_ex[["states"]], sample.int(markov_mix_ex[["order"]], 1L), replace = TRUE)
  )

  # Predict MarkovMix
  expect_true(is.numeric(predict(markov_mix_ex, newdata = new_seq_list)))
  expect_false(is.matrix(predict(markov_mix_ex, newdata = new_seq_list)))
  expect_true(is.matrix(predict(markov_mix_ex, newdata = new_seq_list, aggregate. = FALSE)))
  # All sequences with lengths shorter than order + 1L should result in error
  expect_error(predict(markov_mix_ex, newdata = new_seq_list_short), "does not contain any valid sub\\-sequences")
})
