/*
 * ragged_variable.h
 *
 *  Created on: 24 Feb 2022
 *      Author: Sean L. Wu (slwood89@gmail.com)
 */

#ifndef RAGGED_VARIABLE
#define RAGGED_VARIABLE

#include <individual.h>
#include <Rcpp.h>
#include <queue>
#include <algorithm>

//' @title a variable class for ragged arrays
//' @description this class takes as template parameters the container used for
//' each individual's array, as well as the type of the elements which will
//' be stored for each individual. The array storing each individual's container
//' is a std::vector.
template <typename T>
struct RaggedVariable : public Variable {

  using update_t = std::pair<std::vector<std::vector<T>>, std::vector<size_t>>;
  std::queue<update_t> updates;
  size_t size;
  std::vector<std::vector<T, std::allocator<T>>> values;

  RaggedVariable(const std::vector<std::vector<T>>& initial_values);
  virtual ~RaggedVariable() = default;

  virtual std::vector<std::vector<T, std::allocator<T>>> get_values() const;
  virtual std::vector<std::vector<T, std::allocator<T>>> get_values(const individual_index_t& index) const;
  virtual std::vector<std::vector<T, std::allocator<T>>> get_values(const std::vector<size_t>& index) const;

  virtual std::vector<size_t> get_length() const;
  virtual std::vector<size_t> get_length(const individual_index_t& index) const;
  virtual std::vector<size_t> get_length(const std::vector<size_t>& index) const;

  virtual void queue_update(const std::vector<std::vector<T>>& values, const std::vector<size_t>& index);
  virtual void update() override;

};


template <typename T>
inline RaggedVariable<T>::RaggedVariable(const std::vector<std::vector<T>>& initial_values) :
  size(initial_values.size()), values(size)
{
  values.reserve(initial_values.size());
  for (auto i = 0u; i < initial_values.size(); ++i) {
    std::copy(initial_values[i].begin(), initial_values[i].end(), std::back_inserter(values[i]));
  };
}

//' @title get all values
template <typename T>
inline std::vector<std::vector<T, std::allocator<T>>> RaggedVariable<T>::get_values() const {
  return values;
};

//' @title get all values at index given by a bitset
template <typename T>
inline std::vector<std::vector<T, std::allocator<T>>> RaggedVariable<T>::get_values(const individual_index_t& index) const {
  if (size != index.max_size()) {
    Rcpp::stop("incompatible size bitset used to get values from RaggedVariable");
  }
  std::vector<std::vector<T, std::allocator<T>>> result(index.size());
  auto result_i = 0u;
  for (auto i : index) {
    std::copy(values[i].begin(), values[i].end(), std::back_inserter(result[result_i]));
    ++result_i;
  }
  return result;
};

//' @title get all values at index given by a bitset
template <typename T>
inline std::vector<std::vector<T, std::allocator<T>>> RaggedVariable<T>::get_values(const std::vector<size_t>& index) const {
  std::vector<std::vector<T, std::allocator<T>>> result(index.size());
  for (auto i = 0u; i < index.size(); ++i) {
    if (index[i] >= size) {
      std::stringstream message;
      message << "index for RaggedVariable out of range, supplied index: " << index[i] << ", size of variable: " << size;
      Rcpp::stop(message.str());
    }
    std::copy(values[index[i]].begin(), values[index[i]].end(), std::back_inserter(result[i]));
  }
  return result;
};


//' @title get all lengths of each ragged array
template <typename T>
inline std::vector<size_t> RaggedVariable<T>::get_length() const {
  std::vector<size_t> lengths(size);
  for (auto i = 0u; i < size; ++i) {
    lengths[i] = values[i].size();
  }
  return lengths;
};

//' @title get all lengths of ragged array at index given by a bitset
template <typename T>
inline std::vector<size_t> RaggedVariable<T>::get_length(const individual_index_t& index) const {
  if (size != index.max_size()) {
    Rcpp::stop("incompatible size bitset used to get values from RaggedVariable");
  }
  std::vector<size_t> lengths(index.size());
  auto result_i = 0u;
  for (auto i : index) {
    lengths[result_i] = values[i].size();
    ++result_i;
  }
  return lengths;
};

//' @title get all lengths of ragged array at index given by a vector
template <typename T>
inline std::vector<size_t> RaggedVariable<T>::get_length(const std::vector<size_t>& index) const {
  std::vector<size_t> lengths(index.size());
  for (auto i = 0u; i < index.size(); ++i) {
    if (index[i] >= size) {
      std::stringstream message;
      message << "index for RaggedVariable out of range, supplied index: " << index[i] << ", size of variable: " << size;
      Rcpp::stop(message.str());
    }
    lengths[i] = values[index[i]].size();
  }
  return lengths;
};


//' @title queue a state update for some subset of individuals
template <typename T>
inline void RaggedVariable<T>::queue_update(
    const std::vector<std::vector<T>>& values,
    const std::vector<size_t>& index
) {
  if (values.empty()) {
    return;
  }
  if (values.size() > 1 && values.size() < size && values.size() != index.size()) {
    Rcpp::stop("Mismatch between value and index length");
  }

  for (auto i = 0u; i < index.size(); ++i) {
    if (index[i] >= size) {
      Rcpp::stop("Index out of bounds");
    }
    if (values[i].empty()) {
      Rcpp::stop("Please provide non-empty updates");
    }
  }

  updates.push({ values, index });
}

//' @title apply all queued state updates in FIFO order
template <typename T>
inline void RaggedVariable<T>::update() {
  while(updates.size() > 0) {
    const auto& update = updates.front();
    const auto& values = update.first;
    const auto& index = update.second;

    auto vector_replacement = (index.size() == 0);
    auto value_fill = (values.size() == 1);

    auto& to_update = this->values;

    if (vector_replacement) {
      // For a full vector replacement
      if (value_fill) {
        std::fill(to_update.begin(), to_update.end(), values[0]);
      } else {
        to_update = values;
      }
    } else {
      if (value_fill) {
        // For a fill update
        for (auto i : index) {
          to_update[i] = values[0];
        }
      } else {
        // Subset assignment
        for (auto i = 0u; i < index.size(); ++i) {
          to_update[index[i]] = values[i];
        }
      }
    }
    updates.pop();
  }
}


#endif
