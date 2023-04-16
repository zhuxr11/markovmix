# Load example MarkovMix object
data("markov_mix_ex")

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
