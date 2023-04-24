library(testthat)

test_that("Get state transition patterns, probability matrices and component priors", {
  # Load example MarkovMix object
  data("markov_mix_ex")

  # Transition patterns
  states_mat_test <- do.call(expand.grid,
                             replicate(markov_mix_ex[["order"]] + 1L,
                                       markov_mix_ex[["states"]],
                                       simplify = FALSE))
  states_mat_test <- as.matrix(states_mat_test)[, rev(seq_len(ncol(states_mat_test))), drop = FALSE]
  dimnames(states_mat_test) <- NULL
  expect_equal(get_states_mat(markov_mix_ex), states_mat_test)
  # Error if object type mismatch
  expect_error(get_states_mat(unclass(markov_mix_ex)), "should be a MarkovMix object")
  # But not if check = FALSE
  expect_no_error(get_states_mat(unclass(markov_mix_ex), check = FALSE))

  # Probability matrices
  expect_no_error(get_prob(markov_mix_ex))
  expect_equal(colSums(get_prob(markov_mix_ex), na.rm = TRUE), rep(1, ncol(markov_mix_ex[["counts"]])))

  # Component priors
  expect_no_error(get_prior(markov_mix_ex))
  expect_equal(sum(get_prior(markov_mix_ex), na.rm = TRUE), 1)
})

test_that("Extracting and replacing components in MarkovMix objects", {
  expect_no_error(markov_mix_ex[2L])
  expect_no_error(markov_mix_ex[c(1L, 3L)])
  # Subscript out of bounds error
  expect_error(markov_mix_ex[ncol(markov_mix_ex[["counts"]]) + 1L])

  markov_mix_ex2 <- markov_mix_ex
  expect_no_error(markov_mix_ex2[2L] <- runif(length(markov_mix_ex[["states"]])^(markov_mix_ex[["order"]] + 1L)))
  markov_mix_ex3 <- markov_mix_ex
  expect_no_error(markov_mix_ex3[c(1L, 3L)] <- matrix(runif(length(markov_mix_ex[["states"]])^(markov_mix_ex[["order"]] + 1L) * 2L), ncol = 2L))
  # Error when value is wrong length
  expect_error(markov_mix_ex2[2L] <- runif(length(markov_mix_ex[["states"]])^(markov_mix_ex[["order"]] + 1L) - 1L), "is not 1 \\(recycled\\) or the same as")
  # But it is OK to recycle value of length 1
  expect_no_error(markov_mix_ex2[c(1L, 3L)] <- 0.5)
})

test_that("Reorganizing states in MarkovMix objects", {
  expect_no_error(restate(
    .object = markov_mix_ex,
    .fun = forcats::fct_rev
  ))
  expect_no_error(restate(
    .object = markov_mix_ex,
    .fun = "levels<-",
    value = c("B", "D", "C", "A")
  ))
  expect_no_error(restate(
    .object = markov_mix_ex,
    .fun = ~ forcats::fct_recode(.x, "C" = "D")
  ))
  # Error if states of each variable differs
  set.seed(1234L)
  expect_error(restate(
    .object = markov_mix_ex,
    .fun = ~ `levels<-`(.x, unique(c(levels(.x), sample(LETTERS, size = 1L))))
  ), "Factor levels inconsistency")
})
