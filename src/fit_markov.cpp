#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List split_seq(const List& seq_list, size_t len) {
  size_t list_len = seq_list.length();
  List out(list_len);
  for (size_t i = 0; i < list_len; i++) {
    IntegerVector seq_one = as<IntegerVector>(seq_list[i]);
    size_t seq_one_len = seq_one.length();
    size_t seq_out_len;
    if (seq_one_len < len) {
      seq_out_len = 1;
    } else {
      seq_out_len = seq_one_len - len + 1;
    }
    List seq_out_list(seq_out_len);
    if (seq_one_len <= len) {
      IntegerVector seq_out(len, NA_INTEGER);
      seq_out[seq(0, seq_one_len - 1)] = seq_one;
      seq_out_list[0] = seq_out;
    } else {
      for (size_t seq_pos = 0; seq_pos < seq_one_len - len + 1; seq_pos++) {
        IntegerVector seq_out = seq_one[seq(seq_pos, seq_pos + len - 1)];
        seq_out_list[seq_pos] = seq_out;
      }
    }
    out[i] = seq_out_list;
  }
  return out;
}

/*** R
cat("Testing [split_seq] ...\n")
set.seed(1111L)
test_seq <- purrr::map(seq_len(10L), ~ sample(1L:4L, sample.int(10L, 1L), replace = TRUE))
split_seq(seq_list = test_seq, len = 4L)
*/

// [[Rcpp::export]]
NumericMatrix colsums_by_group(const List& value_list, size_t n_col) {
  size_t list_len = value_list.length();
  NumericMatrix out(list_len, n_col);
  for (size_t i = 0; i < list_len; i++) {
    NumericVector value_one = as<NumericVector>(value_list[i]);
    NumericMatrix value_one_mat(value_one.length() / n_col, n_col, value_one.begin());
    NumericVector colsum_one(n_col);
    for (size_t col_idx = 0; col_idx < n_col; col_idx++) {
      colsum_one[col_idx] = sum(value_one_mat(_, col_idx));
    }
    out(i, _) = colsum_one;
  }
  return out;
}

/*** R
cat("Testing [colsum_by_group] ...\n")
colsums_by_group(value_list = split(matrix(1L:18L, 6L, 3L), rep(1L:3L, each = 2L)), n_col = 3L)
*/
