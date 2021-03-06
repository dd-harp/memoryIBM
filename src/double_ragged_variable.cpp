/*
 * double_ragged_variable.cpp
 *
 *  Created on: 24 Feb 2022
 *      Author: Sean L. Wu (slwood89@gmail.com)
 */

#include "../inst/include/common_types.h"
#include "../inst/include/utils.h"

// [[Rcpp::export]]
Rcpp::XPtr<dbl_ragged> create_double_ragged_variable(
    const std::vector<std::vector<double>>& values
) {
  return Rcpp::XPtr<dbl_ragged>(
    new RaggedVariable<double>(values),
    true
  );
}

// [[Rcpp::export]]
size_t double_ragged_variable_get_size(
  Rcpp::XPtr<dbl_ragged> variable
) {
  return variable->size;
}

// [[Rcpp::export]]
std::vector<std::vector<double>> double_ragged_variable_get_values(
    Rcpp::XPtr<dbl_ragged> variable
) {
  return variable->get_values();
}

// [[Rcpp::export]]
std::vector<std::vector<double>> double_ragged_variable_get_values_at_index(
    Rcpp::XPtr<dbl_ragged> variable,
    Rcpp::XPtr<individual_index_t> index
) {
  return variable->get_values(*index);
}

// [[Rcpp::export]]
std::vector<std::vector<double>> double_ragged_variable_get_values_at_index_vector(
    Rcpp::XPtr<dbl_ragged> variable,
    std::vector<size_t> index
) {
  decrement(index);
  return variable->get_values(index);
}

// [[Rcpp::export]]
std::vector<size_t> double_ragged_variable_get_length(
    Rcpp::XPtr<dbl_ragged> variable
) {
  return variable->get_length();
}

// [[Rcpp::export]]
std::vector<size_t> double_ragged_variable_get_length_at_index(
    Rcpp::XPtr<dbl_ragged> variable,
    Rcpp::XPtr<individual_index_t> index
) {
  return variable->get_length(*index);
}

// [[Rcpp::export]]
std::vector<size_t> double_ragged_variable_get_length_at_index_vector(
    Rcpp::XPtr<dbl_ragged> variable,
    std::vector<size_t> index
) {
  decrement(index);
  return variable->get_length(index);
}

// [[Rcpp::export]]
void double_ragged_variable_queue_fill(
    Rcpp::XPtr<dbl_ragged> variable,
    const std::vector<std::vector<double>> value
) {
  variable->queue_update(value, std::vector<size_t>());
}

// [[Rcpp::export]]
void double_ragged_variable_queue_update(
    Rcpp::XPtr<dbl_ragged> variable,
    const std::vector<std::vector<double>> value,
    std::vector<size_t> index
) {
  decrement(index);
  variable->queue_update(value, index);
}

// [[Rcpp::export]]
void double_ragged_variable_queue_update_bitset(
    Rcpp::XPtr<dbl_ragged> variable,
    const std::vector<std::vector<double>> value,
    Rcpp::XPtr<individual_index_t> index
) {
  auto index_vec = bitset_to_vector_internal(*index, false);
  variable->queue_update(value, index_vec);
}

// [[Rcpp::export]]
void double_ragged_variable_update(
    Rcpp::XPtr<dbl_ragged> variable
) {
  variable->update();
}
