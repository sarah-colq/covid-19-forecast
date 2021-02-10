test_that("Test identity function.",{
  x <- c(1,2,4,5,NA)
  y <- tr.identity(x)
  
  expect_equal(x, y)
})


test_that("Test log pad.",{
  x <- c(0, exp(1)-1, -1, -2)
  y1 <- c(0, 1, -Inf, NaN)
  y2  <- c(0, 1, 0, 0)
  
  expect_equal(tr.log_pad(x, clip_left_0 = FALSE), y1)
  expect_equal(tr.log_pad(x), y2)
})


test_that("Inverse log padding.",{
  x <- c(-1, 0, 1, 2)
  y <- c(exp(-1) - 1, 0, exp(1) - 1, exp(2) - 1)
  
  expect_equal(tr.inv_log_pad(x), y)
})


test_that("Cube root.", {
  x1 <- -8
  x2 <- 8 
  
  expect_equal(tr.cube_root(x1), -2)
  expect_equal(tr.cube_root(x2), 2)
})


test_that("Mean column inputation for matrix.", {
  x <- c(1, NA, 3, 8, NA, NA, NA, NA, 4, 2, 5, 6)
  out_allrows <- c(1, 4, 3, 8, 0, 0, 0, 0, 4, 2, 5, 6)
  out_col23 <- c(1, NA, 3, 8, 0, 0, 0, 0, 4, 2, 5, 6)
  
  test_matrix <- matrix(x, nrow = 4, byrow = FALSE)
  out_all_matrix <- matrix(out_allrows, nrow = 4, byrow = FALSE)
  out_col23_matrix <- matrix(out_col23, nrow = 4, byrow = FALSE)
  
  expect_equal(tr.column_impute(test_matrix), out_all_matrix)
  expect_equal(tr.column_impute(test_matrix, impute_columns = 2:3),
               out_col23_matrix)
})
