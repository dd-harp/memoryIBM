/*
 * integer_ragged_variable.cpp
 *
 *  Created on: 25 Feb 2022
 *      Author: Sean L. Wu (slwood89@gmail.com)
 */

#include "../inst/include/common_types.h"
#include "../inst/include/utils.h"

// [[Rcpp::export]]
Rcpp::XPtr<int_ragged> create_integer_ragged_variable(
    const std::vector<std::vector<int>>& values
) {
  return Rcpp::XPtr<int_ragged>(
    new RaggedVariable<int>(values),
    true
  );
}

// [[Rcpp::export]]
size_t integer_ragged_variable_get_size(
    Rcpp::XPtr<int_ragged> variable
) {
  return variable->size;
}

// [[Rcpp::export]]
std::vector<std::vector<int>> integer_ragged_variable_get_values(
    Rcpp::XPtr<int_ragged> variable
) {
  return variable->get_values();
}

// [[Rcpp::export]]
std::vector<std::vector<int>> integer_ragged_variable_get_values_at_index(
    Rcpp::XPtr<int_ragged> variable,
    Rcpp::XPtr<individual_index_t> index
) {
  return variable->get_values(*index);
}

// [[Rcpp::export]]
std::vector<std::vector<int>> integer_ragged_variable_get_values_at_index_vector(
    Rcpp::XPtr<int_ragged> variable,
    std::vector<size_t> index
) {
  decrement(index);
  return variable->get_values(index);
}

// [[Rcpp::export]]
std::vector<size_t> integer_ragged_variable_get_length(
    Rcpp::XPtr<int_ragged> variable
) {
  return variable->get_length();
}

// [[Rcpp::export]]
std::vector<size_t> integer_ragged_variable_get_length_at_index(
    Rcpp::XPtr<int_ragged> variable,
    Rcpp::XPtr<individual_index_t> index
) {
  return variable->get_length(*index);
}

// [[Rcpp::export]]
std::vector<size_t> integer_ragged_variable_get_length_at_index_vector(
    Rcpp::XPtr<int_ragged> variable,
    std::vector<size_t> index
) {
  decrement(index);
  return variable->get_length(index);
}

// [[Rcpp::export]]
void integer_ragged_variable_queue_fill(
    Rcpp::XPtr<int_ragged> variable,
    const std::vector<std::vector<int>> value
) {
  variable->queue_update(value, std::vector<size_t>());
}

// [[Rcpp::export]]
void integer_ragged_variable_queue_update(
    Rcpp::XPtr<int_ragged> variable,
    const std::vector<std::vector<int>> value,
    std::vector<size_t> index
) {
  decrement(index);
  variable->queue_update(value, index);
}

// [[Rcpp::export]]
void integer_ragged_variable_queue_update_bitset(
    Rcpp::XPtr<int_ragged> variable,
    const std::vector<std::vector<int>> value,
    Rcpp::XPtr<individual_index_t> index
) {
  auto index_vec = bitset_to_vector_internal(*index, false);
  variable->queue_update(value, index_vec);
}

// [[Rcpp::export]]
void integer_ragged_variable_update(
    Rcpp::XPtr<int_ragged> variable
) {
  variable->update();
}
