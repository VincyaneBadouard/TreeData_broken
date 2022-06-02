test_that("PunctualErrorDetection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  Time <- c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)

  # Check the function work ---------------------------------------------------------------------------------------------------------

  ## Punctual error case: error -> NA -----------------------------------------------------------------------------------------------
  DBHCor <- c(13, 14, 15, 16, 30, 19, 15, 21, 23) # errors: 5 and 7th values

  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time)

  expect_true(all(is.na(Rslt[c(5,7)]))) # error -> NA
  expect_true(all(Rslt[-c(5,7)] == DBHCor[-c(5,7)])) # the other values don't change

  ## 2 shifts error case: values don't change ---------------------------------------------------------------------------------------
  DBHCor <- c(13, 14, 15, 16, 12, 14, 15, 11, 13)

  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time)

  expect_true(all(Rslt == DBHCor)) # the other values don't change


  # If only 2 values, with abnormal difference --------------------------------------------------------------------------------------
  DBHCor <- c(13, 30)
  Time <- c(2000, 2002)

  ## Check the different reading direction (TrustMeasSet) ---------------------------------------------------------------------------
  # Trust the first value
  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time, TrustMeasSet = "first")
  expect_true(Rslt[1] == DBHCor[1]) # the 1st value don't change
  expect_true(Rslt[2] == DBHCor[1]) # give the 1st value to the 2nd

  # Trust the last value
  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time, TrustMeasSet = "last")
  expect_true(Rslt[2] == DBHCor[2]) # the 1st value don't change
  expect_true(Rslt[1] == DBHCor[2]) # give the 1st value to the 2nd

  ### Detect Only case
  # Trust the first value
  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time, TrustMeasSet = "first", DetectOnly = TRUE)
  expect_true(Rslt[1] == DBHCor[1]) # the 1st value don't change
  expect_true(is.na(Rslt[2])) # give the 1st value to the 2nd

  # Trust the last value
  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time, TrustMeasSet = "last", DetectOnly = TRUE)
  expect_true(Rslt[2] == DBHCor[2]) # the 1st value don't change
  expect_true(is.na(Rslt[1])) # give the 1st value to the 2nd


})
