# Load example MarkovMix object
data("markov_mix_ex")

# Derive transition pattern soft counts
get_counts(object = markov_mix_ex)

# Derive the order of Markov chains
get_order(object = markov_mix_ex)

# Derive the states of Markov chains
get_states(object = markov_mix_ex)

# Derive state transition patterns
get_states_mat(markov_mix_ex)

# Derive probability matrices
get_prob(markov_mix_ex)

# Derive component priors
get_prior(markov_mix_ex)

# Combine state transition patterns and their probabilities
cbind(
  as.data.frame(get_states_mat(markov_mix_ex)),
  as.data.frame(get_prob(markov_mix_ex))
)

# Extract 1 or more components
markov_mix_ex[2L]
markov_mix_ex[c(1L, 3L)]

# Replace 1 or more components
nrow_value <- length(get_states(object = markov_mix_ex, check = FALSE))^
  (get_order(object = markov_mix_ex, check = FALSE) + 1L)
markov_mix_ex2 <- markov_mix_ex
markov_mix_ex2[2L] <- runif(nrow_value)
print(markov_mix_ex2)
markov_mix_ex3 <- markov_mix_ex
markov_mix_ex3[c(1L, 3L)] <- matrix(runif(nrow_value * 2L), ncol = 2L)
print(markov_mix_ex3)
