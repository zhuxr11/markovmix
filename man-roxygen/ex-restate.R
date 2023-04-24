# Load example MarkovMix object
data("markov_mix_ex")

# Reverse states (using function)
markov_mix_new1 <- restate(
  .object = markov_mix_ex,
  .fun = forcats::fct_rev
)
print(markov_mix_new1)

# Reorder states by hand (using function name with additional arguments)
markov_mix_new2 <- restate(
  .object = markov_mix_ex,
  .fun = "levels<-",
  value = c("B", "D", "C", "A")
)
print(markov_mix_new2)

# Merge state D into C (using purrr-style lambda function)
markov_mix_new3 <- restate(
  .object = markov_mix_ex,
  .fun = ~ forcats::fct_recode(.x, "C" = "D")
)
print(markov_mix_new3)
