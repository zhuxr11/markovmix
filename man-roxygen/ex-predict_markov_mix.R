# Load example MarkovMix object
data("markov_mix_ex")

# Generate a new list of sequences
set.seed(2222L)
new_maxlen <- 8L
new_seq_list <- purrr::rerun(.n = 50L, sample(markov_mix_ex[["states"]], sample.int(new_maxlen, 1L), replace = TRUE))

# Predict the probabilities of sequences
predict(markov_mix_ex, newdata = new_seq_list)

# Predict the probabilities of sequences from each component
predict(markov_mix_ex, newdata = new_seq_list, aggregate. = FALSE)
