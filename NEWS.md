# markovmix 0.1.3

## Bug fixes

* Update `RcppExports.R` and `RcppExports.cpp` with `Rcpp@4a8d30f` on GitHub to resolve warnings listed in issue: https://github.com/RcppCore/Rcpp/issues/1287.


# markovmix 0.1.2

## New features:

* Add `get_counts()`, `get_order()` and `get_states()` functions to get members of a `MarkovMix` object
in an easier way.

## Enhancements:

* Update package documentation.

* Add documentation to C++ functions and export them to C++ interface.

## Bug fixes:

* Import `pillar::pillar_options` since it is mentioned in the documentation of `print.MarkovMix()`,
as reported from platforms as notes that `pillar` is listed as "Imports" but no function is imported.

* Fix incorrect documentation of `value` parameter in the replacement method of `MarkovMix` objects.

* Fix typos in the change logs of the documentations of `MarkovMix` utility functions.


# markovmix 0.1.1

## New features:

* Add `[.MarkovMix()` and `[<-.MarkovMix()` methods to extract and replace components in a `MarkovMix` object.

## Enhancements:

* Add examples of utility functions to `README.Rmd`.

* Add `pillar` explicitly to imports, since it is mentioned in the documentation of `print.MarkovMix()` function.

## Bug fixes:

* Use `testthat::expect_equal(a, b)` instead of `testthat::expect_true(all(a == b))` to avoid small numeric differences,
as reported from platforms as failed tests.


# markovmix 0.1.0

* Initial release of the package.
