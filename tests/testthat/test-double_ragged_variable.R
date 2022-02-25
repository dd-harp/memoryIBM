test_that("can create a ragged double variable", {
  size <- 10
  vals <- replicate(n = size, expr = rexp(n = rpois(n = 1,lambda = 10)))

  var <- DoubleRaggedVariable$new(vals)


  expect_equal(memoryIBM:::double_ragged_variable_get_size(var$.variable), size)
  expect_equal(var$get_values(), vals)
  expect_equal(var$get_length(), vapply(X = vals, FUN = length, FUN.VALUE = integer(1), USE.NAMES = FALSE))

})
