# markovmix 0.1.1

## Enhancements:

* Add `pillar` explicitly to imports, since it is mentioned in the documentation of `print.MarkovMix()` function.

## Bug fixes:

* Use `testthat::expect_equal(a, b)` instead of `testthat::expect_true(all(a == b))` to avoid small numeric differences,
as reported from platforms as failed tests.


# markovmix 0.1.0

* Initial release of the package.
