## code to prepare `markov_mix_ex` dataset goes here

# Generate a list of integer sequences of different lengths with 4 states
test_states <- LETTERS[seq_len(4L)]
test_maxlen <- 10L
set.seed(1111L)
test_seq <- purrr::map(seq_len(100L), ~ sample(test_states, sample.int(test_maxlen, 1L), replace = TRUE))

# Fit a mixture of 2-order Markov chain with 3 components
test_n_comp <- 3L
test_clusters <- matrix(runif(length(test_seq) * test_n_comp), nrow = length(test_seq), ncol = test_n_comp)
markov_mix_ex <- fit_markov_mix(seq_list = test_seq, order. = 2L, states = test_states, clusters = test_clusters)

usethis::use_data(markov_mix_ex, overwrite = TRUE)
