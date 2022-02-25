test_that("ragged double variable functions as expected", {
  library(individual)

  size <- 10
  vals <- replicate(n = size, expr = rexp(n = rpois(n = 1,lambda = 10)), simplify = FALSE)
  vals_len <- vapply(X = vals, FUN = length, FUN.VALUE = integer(1), USE.NAMES = FALSE)

  var <- DoubleRaggedVariable$new(vals)

  expect_equal(memoryIBM:::double_ragged_variable_get_size(var$.variable), size)
  expect_equal(var$get_values(), vals)
  expect_equal(var$get_length(), vapply(X = vals, FUN = length, FUN.VALUE = integer(1), USE.NAMES = FALSE))

  idx <- c(1, 5, 10)
  expect_equal(var$get_values(index = idx), vals[idx])
  expect_equal(var$get_values(index = Bitset$new(size = size)$insert(idx)), vals[idx])

  expect_equal(var$get_length(index = idx), vals_len[idx])
  expect_equal(var$get_length(index = Bitset$new(size = size)$insert(idx)), vals_len[idx])

  var$queue_update(values = list(1:5))
  var$.update()
  expect_equal(var$get_values(), replicate(n = size, expr = 1:5, simplify = FALSE))

  var$queue_update(values = vals)
  var$.update()
  expect_equal(var$get_values(), vals)

  sub_update <- list(1:3, 2:4, 3:5)
  var$queue_update(values = sub_update, index = idx)
  var$.update()
  new_vals <- vals
  new_vals[idx] <- sub_update
  expect_equal(var$get_values(), new_vals)

  sub_update <- list(3:1, 4:2, 5:3)
  var$queue_update(values = sub_update, index = Bitset$new(size = size)$insert(idx))
  var$.update()
  new_vals <- vals
  new_vals[idx] <- sub_update
  expect_equal(var$get_values(), new_vals)
})


test_that("ragged double variable errors as expected", {
  library(individual)

  size <- 10
  vals <- replicate(n = size, expr = 1:10, simplify = FALSE)

  var <- DoubleRaggedVariable$new(vals)

  expect_error(var$get_values(index = c(-1, 5)))
  expect_error(var$get_values(index = c(1, size + 100L)))

  expect_error(var$get_values(index = Bitset$new(size = 100)$insert(1)))
  expect_error(var$get_values(index = Bitset$new(size = 100)))

  expect_error(var$get_length(index = c(-1, 5)))
  expect_error(var$get_length(index = c(1, size + 100L)))

  expect_error(var$get_length(index = Bitset$new(size = 100)$insert(1)))
  expect_error(var$get_length(index = Bitset$new(size = 100)))

  expect_error(var$queue_update(values = list(1:2, 3:5)))
  expect_error(var$queue_update(values = list(NULL, 1:3), index = c(4, 5)))
  expect_error(var$queue_update(values = list(numeric(0), 1:3), index = c(4, 5)))
  expect_error(var$queue_update(values = list(NULL, 1:3), index = Bitset$new(size)$insert(c(4, 5))))
  expect_error(var$queue_update(values = list(numeric(0), 1:3), index = Bitset$new(size)$insert(c(4, 5))))
  expect_error(var$queue_update(values = list(5:6, 9:4), index = c(1, 20)))
  expect_error(var$queue_update(values = list(1:2, 3:5), index = c(1, 4, 5)))
  expect_error(var$queue_update(values = list(1:2, 3:5), index = c(-2, 5)))
  expect_error(var$queue_update(values = list(1:2, 3:5), index = Bitset$new(size)$insert(c(1, 4, 5))))
  expect_error(var$queue_update(values = list(1:2, 3:5), index = Bitset$new(100L)$insert(c(1, 4))))
})
