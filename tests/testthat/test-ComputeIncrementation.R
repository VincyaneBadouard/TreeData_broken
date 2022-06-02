test_that("ComputeIncrementation", {

  # Check the function argument -------------------------------------------------------------------------------------
  expect_error(ComputeIncrementation(Var = "DBH"),
               regexp = "'Var' argument must be numeric")

  expect_error(ComputeIncrementation(Var = 2),
               regexp = "To compute 'Var' incrementation, 'Var' must have at least 2 values")


  expect_error(ComputeIncrementation(Var = c(1, 2), Type = "numeric"),
               regexp = "The 'Type' argument value must be equal to 'annual' or 'absolute'")


  expect_error(ComputeIncrementation(Var = c(1, 2), Type = "annual", Time = TRUE),
               regexp = "'Time' argument must be numeric")


  expect_error(ComputeIncrementation(Var = c(1, 2), Type = "annual", Time = c(1, 1)),
               regexp = "'Time' argument of the ComputeIncrementation function must have unique values")


  expect_error(ComputeIncrementation(Var = c(1, 2), Type = "annual", Time = c(1, 2, 3)),
               regexp = "'Var' and 'Time' arguments of the ComputeIncrementation function must have the same length")

  # Check the function work ------------------------------------------------------------------------------------------

  # Absolute incrementation
  Var = c(2, 4, 4, 2, 10)

  Rslt <- ComputeIncrementation(Var = Var, Type = "absolute")
  expect_true(all(Rslt == c(2, 0, -2, 8)))

  # Annual incrementation
  Var = c(2, 6, 6, 2, 18)
  Time = c(2000, 2002, 2004, 2006, 2008)

  Rslt <- ComputeIncrementation(Var = Var, Type = "annual", Time = Time)
  expect_true(all(Rslt == c(2, 0, -2, 8)))


  # Particular cases  -----------------------------------------------------------------------------------------------
  # NA in Var
  Rslt <- ComputeIncrementation(Var = c(1, 6, NA, 10), Type = "annual", Time = c(2000, 2001, 2002, 2003))

  expect_true(is.na(Rslt[2])) # don't jump the NA
  expect_true(all(Rslt[-2] == c(5, 2))) # Rslt = c(5, NA, 4)

  # NA in Time
  Rslt <- ComputeIncrementation(Var = c(1, 6, 8, 10, 12), Type = "annual", Time = c(2000, 2001, NA, 2002, 2003))

  expect_true(all(is.na(Rslt[c(2,3)]))) # put NA if Time = NA
  expect_true(all(Rslt[-c(2,3)] == c(5, 2))) # Rslt = c(5, NA, NA, 2)

  # No growth
  Rslt <- ComputeIncrementation(Var = c(1, 6, 6), Type = "annual", Time = c(2000, 2001, 2002))

  expect_true(all(Rslt == c(5, 0)))




})
