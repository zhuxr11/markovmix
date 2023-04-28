# Load example MarkovMix object
data("markov_mix_ex")

# Generate a new list of sequences
set.seed(2222L)
new_maxlen <- 8L
new_seq_list <- purrr::map(
  seq_len(50L),
  ~ sample(get_states(object = markov_mix_ex, check = FALSE),
           sample.int(new_maxlen, 1L),
           replace = TRUE)
)

# Predict the probabilities of sequences
predict(markov_mix_ex, newdata = new_seq_list)

# Predict the probabilities of sequences from each component
predict(markov_mix_ex, newdata = new_seq_list, aggregate. = FALSE)
