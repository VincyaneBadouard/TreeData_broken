test_that("RegressionInterpolation", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  Y <- c(10, 15, NA, 25, NA, NA)
  X <- c(1:6)
  # plot(X, Y)

  # Check the function argument -----------------------------------------------------------------------------------------------------
  expect_error(RegressionInterpolation(Y = "Y", X = "X"),
               regexp = "The 'X' and 'Y' variables of the 'RegressionInterpolation' function must be numeric")

  expect_error(RegressionInterpolation(Y = c(2, 3), X = c(1, 2, 3)),
               regexp = "The variables X and Y must be of the same length")

  expect_error(RegressionInterpolation(Y = 1, X = 2, CorrectionType = TRUE),
               regexp = "The 'CorrectionType' argument value must be 'quadratic' and/or 'linear'")


  # Check the function work ---------------------------------------------------------------------------------------------------------

  Ycor <- RegressionInterpolation(Y = Y, X = X, CorrectionType = "quadratic")
  expect_equal(Ycor, c(10, 15, 20, 25, 30, 35), tolerance = 1e-3)

  Ycor <- RegressionInterpolation(Y = Y, X = X, CorrectionType = "linear")
  expect_equal(Ycor, c(10, 15, 20, 25, 30, 35), tolerance = 1e-3)

})
